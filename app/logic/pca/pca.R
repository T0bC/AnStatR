box::use(
  mixOmics,
  rhino,
)

box::use(
  app/logic/shared/error_handling,
)

# =============================================================================
# Pure logic functions for PCA / sPCA / IPCA
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Valid PCA analysis types
#' @export
VALID_ANALYSIS_TYPES <- c("pca", "spca", "ipca")

#' Validate inputs before PCA computation
#' @param columns Character vector of selected column names
#' @param data Data frame to validate against
#' @return List with $valid (logical) and $error (app_error or NULL)
#' @export
validate_inputs <- function(columns, data) {
  if (is.null(columns) || length(columns) == 0) {
    rhino$log$warn("PCA: no columns selected")
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = "Please select at least one column.",
        operation_name = "pca_validate_inputs"
      )
    ))
  }

  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    rhino$log$warn(
      "PCA: columns not found: {paste(missing, collapse = ', ')}"
    )
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = paste(
          "Columns not found in data:",
          paste(missing, collapse = ", ")
        ),
        operation_name = "pca_validate_inputs"
      )
    ))
  }

  list(valid = TRUE, error = NULL)
}

#' Run PCA, sPCA, or IPCA using mixOmics
#'
#' Data is assumed to be already cleaned (no NAs). Centering and
#' scaling are handled by mixOmics via the center/scale arguments
#' (ignored for IPCA, which mixOmics does not center/scale by
#' default the same way — see mixOmics::ipca()). The returned
#' structure exposes mixOmics' native fields (scores, loadings,
#' variance) directly rather than reshaping them into a
#' FactoMineR-like object.
#'
#' @param data Data frame (full, may include metadata columns)
#' @param columns Character vector of measurement column names
#' @param meta_cols Character vector of metadata column names
#'   (optional). When provided, the metadata is attached to the
#'   result as $ind_meta and used to label individuals.
#' @param ncp Number of components to retain. NULL (default)
#'   retains all feasible components.
#' @param center Logical, whether to center variables before
#'   fitting. Default FALSE for backward compatibility.
#' @param scale. Logical, whether to scale variables to unit
#'   variance before fitting. Default FALSE.
#' @param analysis_type One of "pca", "spca", "ipca".
#' @param keep_x Integer vector of length ncp, number of
#'   variables to keep per component (sPCA only, required
#'   when analysis_type == "spca").
#' @param ipca_mode Character, "deflation" or "parallel"
#'   (IPCA only).
#' @return List with $success, $result or $error. $result
#'   contains $model, $analysis_type, $scores, $loadings,
#'   $variance, $center, $scale, $ncomp, $call_info.
#' @export
run_pca <- function(data, columns,
                    meta_cols = character(0), ncp = NULL,
                    center = FALSE, scale. = FALSE,
                    analysis_type = "pca", keep_x = NULL,
                    ipca_mode = "deflation") {
  error_context <- list(
    n_variables = length(columns),
    n_observations = nrow(data),
    variables = paste(columns, collapse = ", "),
    analysis_type = analysis_type
  )

  error_handling$safe_execute(
    expr = {
      if (!analysis_type %in% VALID_ANALYSIS_TYPES) {
        stop(paste0(
          "Invalid analysis_type: '", analysis_type,
          "'. Expected one of: ",
          paste(VALID_ANALYSIS_TYPES, collapse = ", ")
        ))
      }

      numeric_data <- data[, columns, drop = FALSE]
      n <- nrow(numeric_data)
      p <- ncol(numeric_data)
      x_mat <- as.matrix(numeric_data)

      max_possible <- min(p, n - 1)
      max_ncp <- if (is.null(ncp)) {
        max_possible
      } else {
        min(ncp, max_possible)
      }

      if (analysis_type == "spca") {
        if (is.null(keep_x) || anyNA(keep_x) ||
            length(keep_x) != max_ncp) {
          stop(
            "keepX invalid or incomplete: a numeric value is ",
            "required for every component in sPCA."
          )
        }
      }

      rhino$log$info(
        "{toupper(analysis_type)}: running mixOmics::",
        "{analysis_type}() — {p} variables, {n} observations,",
        " {max_ncp} components"
      )

      model <- switch(
        analysis_type,
        pca = mixOmics$pca(
          x_mat, ncomp = max_ncp, center = center, scale = scale.
        ),
        spca = mixOmics$spca(
          x_mat, ncomp = max_ncp, keepX = keep_x,
          center = center, scale = scale.
        ),
        ipca = mixOmics$ipca(
          x_mat, ncomp = max_ncp, mode = ipca_mode,
          scale = scale.
        )
      )

      result <- build_pca_result(
        model, analysis_type, n, p, keep_x = keep_x
      )

      # Centering/scaling used at fit time, captured explicitly:
      # mixOmics::pca() stores $center/$scale on the model, but
      # spca()/ipca() do not, so this must be tracked separately
      # for consistent manual projection of new data in the
      # Prediction module. mixOmics::ipca() has no `center`
      # argument and always centers internally, regardless of
      # the `center` flag passed here — so IPCA must always
      # record the true column means, not the requested value.
      result$center <- if (isTRUE(center) || analysis_type == "ipca") {
        colMeans(x_mat)
      } else {
        stats::setNames(rep(0, p), columns)
      }
      result$scale <- if (isTRUE(scale.)) {
        apply(x_mat, 2, stats::sd)
      } else {
        stats::setNames(rep(1, p), columns)
      }

      result$ind_meta <- build_ind_meta(data, meta_cols, n)
      result <- apply_row_labels(result, result$ind_meta)

      rhino$log$info(
        "{toupper(analysis_type)}: complete ({p} variables,",
        " {n} observations, {max_ncp} components retained)"
      )

      result
    },
    operation_name = "PCA",
    context = error_context,
    error_parser = pca_error_parser
  )
}

#' Tune sPCA keepX per component via repeated CV
#'
#' Wraps mixOmics::tune.spca() to select how many variables
#' each component should keep, analogous to
#' run_plsda_tune_keepx() for sPLS-DA. Requires nrepeat >= 3
#' for mixOmics to return a usable choice.
#'
#' @param data Data frame (cleaned, optionally scaled)
#' @param columns Character vector of measurement column names
#' @param ncomp Integer, number of components
#' @param test_keep_x Integer vector, candidate keepX values
#'   to test (default a modest grid capped by column count)
#' @param folds Integer, number of CV folds
#' @param repeats Integer, number of CV repeats (>= 3)
#' @param center Logical, center variables before fitting
#' @param scale. Logical, scale variables before fitting
#' @return List with $success, $result (named integer vector,
#'   one keepX per component) or $error
#' @export
run_pca_tune_keepx <- function(data, columns, ncomp,
                               test_keep_x = NULL,
                               folds = 5, repeats = 3,
                               center = TRUE, scale. = TRUE) {
  error_handling$safe_execute(
    {
      x_mat <- as.matrix(data[, columns, drop = FALSE])
      p <- length(columns)

      candidates <- test_keep_x %||% unique(pmin(
        p, c(5, 10, 15, 20, 30)
      ))

      rhino$log$info(
        "sPCA: tuning keepX — ncomp={ncomp},",
        " candidates=[{paste(candidates, collapse=',')}],",
        " folds={folds}, repeats={repeats}"
      )

      tune_res <- mixOmics$tune.spca(
        x_mat, ncomp = ncomp,
        test.keepX = candidates,
        folds = folds, nrepeat = repeats,
        center = center, scale = scale.
      )

      choice <- tune_res$choice.keepX
      if (is.character(choice)) {
        stop(
          "Not enough repeats to compute a stable keepX ",
          "choice. Increase CV repeats to at least 3."
        )
      }

      keep_x <- as.integer(choice[seq_len(ncomp)])
      names(keep_x) <- paste0("Dim.", seq_len(ncomp))

      rhino$log$info(
        "sPCA: tuning complete — ",
        "keepX=[{paste(keep_x, collapse=',')}]"
      )

      keep_x
    },
    operation_name = "sPCA keepX Tuning",
    error_parser = pca_error_parser
  )
}

#' Extract PCA/sPCA/IPCA scores as a flat data frame
#'
#' Combines individual scores with any attached metadata into
#' a single data frame (metadata columns + Dim.1, Dim.2, …),
#' the shape downstream modules (LDA, Cluster) expect when
#' chaining PCA output as their "PCA scores" data source.
#' Shared so both modules read the PCA result the same way
#' instead of duplicating this extraction logic.
#'
#' @param pca_result_reactive A zero-arg function (e.g. a
#'   Shiny reactive) returning the $result-wrapper from
#'   run_pca() (with $success and $result), or NULL
#' @return Data frame, or NULL if no successful PCA result
#'   is available
#' @export
extract_pca_scores <- function(pca_result_reactive) {
  if (is.null(pca_result_reactive)) return(NULL)
  pca_res <- pca_result_reactive()
  if (is.null(pca_res) || !isTRUE(pca_res$success)) {
    return(NULL)
  }
  res <- pca_res$result
  coord <- as.data.frame(res$scores)
  meta <- res$ind_meta
  if (
    !is.null(meta) &&
    nrow(meta) == nrow(coord) &&
    !("Row" %in% names(meta) && ncol(meta) == 1)
  ) {
    cbind(meta, coord)
  } else {
    coord
  }
}

#' Extract variance-explained recommendation thresholds
#'
#' Reads the cumulative variance table and returns the number
#' of components needed to reach 90%/95% cumulative variance.
#' For IPCA results, this reflects mixOmics' per-component
#' variance in fitted order (not a meaningful ranking, since
#' independent components are not variance-ordered) — callers
#' displaying this to users should note that caveat for IPCA.
#'
#' @param pca_result_reactive A zero-arg function (e.g. a
#'   Shiny reactive) returning the $result-wrapper from
#'   run_pca() (with $success and $result), or NULL
#' @return List with n90, cum90, n95, cum95, or NULL
#' @export
extract_variance_explained <- function(pca_result_reactive) {
  if (is.null(pca_result_reactive)) return(NULL)
  pca_res <- tryCatch(
    pca_result_reactive(),
    error = function(e) NULL
  )
  if (
    is.null(pca_res) ||
    !isTRUE(pca_res$success) ||
    is.null(pca_res$result$variance)
  ) {
    return(NULL)
  }
  variance <- pca_res$result$variance
  cum_var <- variance[["cumulative_variance_percent"]]
  if (is.null(cum_var) || length(cum_var) == 0) {
    return(NULL)
  }
  n90 <- which(cum_var >= 90)[1]
  n95 <- which(cum_var >= 95)[1]
  if (is.na(n90)) n90 <- length(cum_var)
  if (is.na(n95)) n95 <- length(cum_var)
  list(
    n90 = n90,
    cum90 = round(cum_var[n90], 1),
    n95 = n95,
    cum95 = round(cum_var[n95], 1)
  )
}

#' Error parser for PCA-specific errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
pca_error_parser <- function(error_msg,
                             operation_name = "PCA") {
  if (grepl(
    "singular|invertible",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Data matrix is singular.",
      " Remove highly correlated or constant variables."
    )
  } else if (grepl(
    "\\bNA\\b|missing|NaN",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Data contains missing values.",
      " Please handle missing data first."
    )
  } else if (grepl("keepX", error_msg, ignore.case = TRUE)) {
    paste0(operation_name, ": ", error_msg)
  } else if (grepl("numeric", error_msg, ignore.case = TRUE)) {
    paste0(
      operation_name,
      ": All selected columns must be numeric."
    )
  } else if (grepl(
    "ncp|dimension|ncomp",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Invalid number of components.",
      " Check your data dimensions."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Build the structured PCA/sPCA/IPCA result from a fitted mixOmics model
#'
#' Exposes mixOmics' native scores/loadings/variance fields directly.
#' Does not compute contribution/cos2 — see pca_stats.R for those,
#' called on demand by renderers.
#'
#' @param model Fitted mixOmics pca/spca/ipca object
#' @param analysis_type "pca", "spca", or "ipca"
#' @param n Number of observations
#' @param p Number of variables
#' @param keep_x Integer vector, keepX per component (sPCA only)
#' @return List with $model, $analysis_type, $scores, $loadings,
#'   $variance, $ncomp, $call_info, and (sPCA only) $keep_x /
#'   $selected_variables
#' @export
build_pca_result <- function(model, analysis_type, n, p,
                             keep_x = NULL) {
  ncomp <- model$ncomp
  comp_names <- paste0("Dim.", seq_len(ncomp))

  scores <- if (analysis_type == "ipca") model$x else model$variates$X
  scores <- as.matrix(scores)
  colnames(scores) <- comp_names

  loadings <- as.matrix(model$loadings$X)
  colnames(loadings) <- comp_names

  prop_var <- as.numeric(model$prop_expl_var$X)
  variance <- data.frame(
    component = comp_names,
    prop_expl_var = prop_var,
    cumulative_variance_percent = cumsum(prop_var) * 100,
    check.names = FALSE
  )
  variance$prop_expl_var <- variance$prop_expl_var * 100
  names(variance)[names(variance) == "prop_expl_var"] <-
    "variance_percent"
  rownames(variance) <- comp_names

  result <- list(
    model = model,
    analysis_type = analysis_type,
    scores = scores,
    loadings = loadings,
    variance = variance,
    ncomp = ncomp,
    call_info = list(n = n, p = p, ncp = ncomp)
  )

  if (analysis_type == "spca") {
    result$keep_x <- keep_x
    result$selected_variables <- lapply(
      seq_len(ncomp),
      function(i) {
        loadings_i <- loadings[, i]
        rownames(loadings)[loadings_i != 0]
      }
    )
    names(result$selected_variables) <- comp_names
  }

  result
}


#' Build metadata data frame for individuals
#'
#' Extracts selected metadata columns from the original data,
#' aligned with the rows used in PCA. Returns a data frame
#' with one row per individual. If no metadata columns are
#' selected, returns a data frame with a single "Row" column
#' containing row numbers.
#'
#' @param data Full data frame (including metadata columns)
#' @param meta_cols Character vector of metadata column names
#' @param n Number of rows (for fallback labels)
#' @return Data frame with metadata or row numbers
#' @export
build_ind_meta <- function(data, meta_cols, n) {
  if (length(meta_cols) == 0 ||
      !any(meta_cols %in% names(data))) {
    return(data.frame(
      Row = seq_len(n),
      stringsAsFactors = FALSE
    ))
  }

  valid_cols <- intersect(meta_cols, names(data))
  meta <- data[, valid_cols, drop = FALSE]
  # Convert factors to character for consistent handling
  for (col in names(meta)) {
    if (is.factor(meta[[col]])) {
      meta[[col]] <- as.character(meta[[col]])
    }
  }
  meta
}


#' Apply row labels from metadata to score/loading matrices
#'
#' Sets rownames on $scores using a composite label built from
#' metadata columns. If labels are not unique, appends a row
#' number suffix.
#'
#' @param result PCA result list (from build_pca_result())
#' @param meta Data frame from build_ind_meta
#' @return The modified result
#' @export
apply_row_labels <- function(result, meta) {
  if ("Row" %in% names(meta) && ncol(meta) == 1) {
    labels <- as.character(meta$Row)
  } else {
    labels <- apply(meta, 1, function(row) {
      paste(row, collapse = " | ")
    })
  }

  # Ensure uniqueness by appending index where needed
  if (anyDuplicated(labels) > 0) {
    labels <- make.unique(labels, sep = "_")
  }

  rownames(result$scores) <- labels

  result
}
