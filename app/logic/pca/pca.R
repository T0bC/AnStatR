box::use(
  rhino,
  stats,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for PCA
# No Shiny dependencies allowed in this file.
# =============================================================================

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

#' Run PCA using stats::prcomp
#'
#' Data is assumed to be already cleaned (no NAs) and optionally
#' scaled upstream. Computes eigenvalues, variable coordinates /
#' contributions / cos2, and individual coordinates / contributions
#' / cos2. The returned structure mirrors FactoMineR::PCA output
#' so that downstream renderers work with the same shape.
#'
#' @param data Data frame (full, may include metadata columns)
#' @param columns Character vector of measurement column names
#' @param meta_cols Character vector of metadata column names
#'   (optional). When provided, the metadata is attached to the
#'   result as $ind$meta and used to label individual rows.
#' @param ncp Number of components to retain. NULL (default)
#'   retains all feasible components. The full result is always
#'   stored; downstream renderers use a separate display_ncp
#'   to limit what is shown in the UI.
#' @return List with $success, $result or $error.
#'   $result contains $eig, $var, $ind, $ncp, $call_info.
#' @export
run_pca <- function(data, columns,
                    meta_cols = character(0), ncp = NULL) {
  error_context <- list(
    n_variables = length(columns),
    n_observations = nrow(data),
    variables = paste(columns, collapse = ", ")
  )

  error_handling$safe_execute(
    expr = {
      numeric_data <- data[, columns, drop = FALSE]
      n <- nrow(numeric_data)
      p <- ncol(numeric_data)

      # Retain all feasible components by default
      max_possible <- min(p, n - 1)
      max_ncp <- if (is.null(ncp)) {
        max_possible
      } else {
        min(ncp, max_possible)
      }

      pca_obj <- stats$prcomp(
        numeric_data,
        center = FALSE,
        scale. = FALSE
      )

      result <- build_pca_result(pca_obj, max_ncp, n, p)

      # Attach metadata for individual labelling and grouping
      result$ind$meta <- build_ind_meta(
        data, meta_cols, n
      )
      result <- apply_row_labels(
        result, result$ind$meta
      )

      rhino$log$info(
        "PCA: complete ({p} variables, {n} observations,",
        " {max_ncp} components retained)"
      )

      result
    },
    operation_name = "PCA",
    context = error_context,
    error_parser = pca_error_parser
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
  } else if (grepl("numeric", error_msg, ignore.case = TRUE)) {
    paste0(
      operation_name,
      ": All selected columns must be numeric."
    )
  } else if (grepl(
    "ncp|dimension",
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

#' Build the structured PCA result from a prcomp object
#'
#' @param pca_obj prcomp result
#' @param ncp Number of components to retain
#' @param n Number of observations
#' @param p Number of variables
#' @return List with $eig, $var, $ind, $ncp, $call_info
build_pca_result <- function(pca_obj, ncp, n, p) {
  sdev <- pca_obj$sdev
  eigenvalues <- sdev^2
  total_var <- sum(eigenvalues)
  var_pct <- eigenvalues / total_var * 100
  cum_pct <- cumsum(var_pct)

  # Eigenvalue table (all components)
  eig <- data.frame(
    eigenvalue = eigenvalues,
    `variance.percent` = var_pct,
    `cumulative.variance.percent` = cum_pct,
    check.names = FALSE
  )
  rownames(eig) <- paste0("Dim.", seq_along(eigenvalues))
  colnames(eig) <- c(
    "eigenvalue", "variance.percent",
    "cumulative.variance.percent"
  )

  # Limit to ncp components for var/ind results
  comp_idx <- seq_len(ncp)
  dim_names <- paste0("Dim.", comp_idx)

  # --- Variable results ---
  # rotation: p x p matrix, columns are PCs
  rotation <- pca_obj$rotation[, comp_idx, drop = FALSE]

  # Coordinates: correlation between variable and PC
  # For centered (possibly scaled) data: coord = rotation * sdev
  var_coord <- sweep(
    rotation, 2, sdev[comp_idx], FUN = "*"
  )
  colnames(var_coord) <- dim_names

  # Cos2: squared coordinates (quality of representation)
  var_cos2 <- var_coord^2
  colnames(var_cos2) <- dim_names

  # Contributions: (rotation^2 * 100) since rotation
  # columns are unit vectors, rotation[,k]^2 sums to 1
  var_contrib <- sweep(
    rotation^2, 2,
    rep(100, ncp), FUN = "*"
  )
  colnames(var_contrib) <- dim_names

  var_result <- list(
    coord = var_coord,
    contrib = var_contrib,
    cos2 = var_cos2
  )

  # --- Individual results ---
  scores <- pca_obj$x[, comp_idx, drop = FALSE]
  colnames(scores) <- dim_names

  # Individual coordinates (scores)
  ind_coord <- scores

  # Individual cos2: score^2 / sum(score^2 across all PCs)
  total_dist2 <- rowSums(pca_obj$x^2)
  # Avoid division by zero for rows at the origin
  total_dist2[total_dist2 == 0] <- 1
  ind_cos2 <- sweep(
    scores^2, 1, total_dist2, FUN = "/"
  )
  colnames(ind_cos2) <- dim_names

  # Individual contributions: (score^2 / (n_eff * eigenvalue)) * 100
  # prcomp uses (n-1) divisor for variance, so eigenvalue = sum(score^2)/(n-1)
  # To make contributions sum to 100: use (n-1) as divisor
  n_eff <- n - 1
  ind_contrib <- sweep(
    scores^2, 2,
    n_eff * eigenvalues[comp_idx], FUN = "/"
  ) * 100
  colnames(ind_contrib) <- dim_names

  ind_result <- list(
    coord = ind_coord,
    contrib = ind_contrib,
    cos2 = ind_cos2
  )

  list(
    eig = eig,
    var = var_result,
    ind = ind_result,
    ncp = ncp,
    call_info = list(
      n = n,
      p = p,
      ncp = ncp
    )
  )
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


#' Apply row labels from metadata to individual result matrices
#'
#' Sets rownames on ind$coord, ind$contrib, and ind$cos2 using
#' a composite label built from metadata columns. If labels are
#' not unique, appends a row number suffix.
#'
#' @param result PCA result list (modified in place via reference)
#' @param meta Data frame from build_ind_meta
#' @return The modified result (invisibly)
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

  rownames(result$ind$coord) <- labels
  rownames(result$ind$contrib) <- labels
  rownames(result$ind$cos2) <- labels

  invisible(result)
}
