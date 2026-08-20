box::use(
  openxlsx,
  rhino,
  stats,
)

box::use(
  app/logic/shared/settings[app_version],
)

# =============================================================================
# Pure logic functions for LDA/QDA result export
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Export LDA/QDA results to a formatted Excel workbook
#'
#' Sheet 1 ("LD Scores") contains metadata + LD scores for
#' all observations — ready for downstream clustering.
#' For QDA (no LD scores), sheet 1 contains metadata +
#' predicted class + posterior probabilities instead.
#' Remaining sheets hold model details and classification.
#'
#' @param lda_result LDA/QDA result list from run_lda()/run_qda()
#' @param file Path to save the Excel file
#' @param test_result Optional prediction result from
#'   run_predict() for train/test split mode
#' @param perf_result Optional list from run_plsda_perf()
#'   (with $errors and $stability) — when present and
#'   $stability is non-NULL, adds a Selected Variable
#'   Stability sheet (sPLS-DA only)
#' @return NULL (side effect: writes file)
#' @export
create_lda_excel <- function(lda_result, file,
                             test_result = NULL,
                             perf_result = NULL) {
  wb <- openxlsx$createWorkbook()
  is_lda <- lda_result$analysis_type %in%
    c("lda", "mda", "plsda", "splsda")
  is_sparse <- lda_result$analysis_type == "splsda"
  is_cv <- !is.null(lda_result$cv)
  sheet_count <- 0

  # ---------------------------------------------------------------
  # Sheet 1: LD Scores (LDA) or Posterior (QDA)
  # This is the primary sheet for downstream use
  # ---------------------------------------------------------------
  if (is_lda && !is.null(lda_result$scores)) {
    scores_df <- build_scores_sheet(lda_result)
    add_sheet(wb, "LD Scores", scores_df)
    sheet_count <- sheet_count + 1
  } else {
    # QDA or CV mode: export posterior + predicted class
    post_df <- build_posterior_sheet(
      lda_result, test_result
    )
    if (!is.null(post_df)) {
      add_sheet(wb, "Classification", post_df)
      sheet_count <- sheet_count + 1
    }
  }

  # ---------------------------------------------------------------
  # Sheet 2: Prior Probabilities
  # ---------------------------------------------------------------
  if (!is.null(lda_result$prior)) {
    prior_df <- data.frame(
      Group = names(lda_result$prior),
      Prior = round(
        as.numeric(lda_result$prior), 6
      ),
      stringsAsFactors = FALSE
    )
    add_sheet(wb, "Prior Probabilities", prior_df)
    sheet_count <- sheet_count + 1
  }

  # ---------------------------------------------------------------
  # Sheet 3: Group Means
  # ---------------------------------------------------------------
  if (!is.null(lda_result$means)) {
    means_df <- cbind(
      Group = rownames(lda_result$means),
      as.data.frame(round(lda_result$means, 6))
    )
    rownames(means_df) <- NULL
    add_sheet(wb, "Group Means", means_df)
    sheet_count <- sheet_count + 1
  }

  # ---------------------------------------------------------------
  # Sheet 4: LD Coefficients (LDA, MDA, PLS-DA/sPLS-DA loadings)
  # ---------------------------------------------------------------
  if (is_lda && !is.null(lda_result$scaling)) {
    scaling_df <- cbind(
      Variable = rownames(lda_result$scaling),
      as.data.frame(round(lda_result$scaling, 6))
    )
    rownames(scaling_df) <- NULL
    add_sheet(wb, "LD Coefficients", scaling_df)
    sheet_count <- sheet_count + 1
  }

  # ---------------------------------------------------------------
  # Sheet 4a2: VIP Scores (PLS-DA/sPLS-DA only)
  # ---------------------------------------------------------------
  if (
    lda_result$analysis_type %in% c("plsda", "splsda") &&
    !is.null(lda_result$vip)
  ) {
    vip_df <- cbind(
      Variable = rownames(lda_result$vip),
      as.data.frame(round(lda_result$vip, 6))
    )
    rownames(vip_df) <- NULL
    add_sheet(wb, "VIP Scores", vip_df)
    sheet_count <- sheet_count + 1
  }

  # ---------------------------------------------------------------
  # Sheet 4b: Selected Variables (sPLS-DA only)
  # ---------------------------------------------------------------
  if (is_sparse && !is.null(lda_result$selected_variables)) {
    sel <- lda_result$selected_variables
    rows <- lapply(names(sel), function(comp) {
      vars <- sel[[comp]]
      if (length(vars) == 0) return(NULL)
      data.frame(
        Component = comp, Variable = vars,
        stringsAsFactors = FALSE
      )
    })
    sel_df <- do.call(rbind, rows)
    if (!is.null(sel_df)) {
      rownames(sel_df) <- NULL
      add_sheet(wb, "Selected Variables", sel_df)
      sheet_count <- sheet_count + 1
    }
  }

  # ---------------------------------------------------------------
  # Sheet 4c: Selected Variable Stability (sPLS-DA, if perf() run)
  # ---------------------------------------------------------------
  if (
    is_sparse &&
    !is.null(perf_result) &&
    !is.null(perf_result$stability)
  ) {
    add_sheet(wb, "Selected Variable Stability", perf_result$stability)
    sheet_count <- sheet_count + 1
  }

  # ---------------------------------------------------------------
  # Sheet 5: Proportion of Trace (LDA only)
  # ---------------------------------------------------------------
  if (!is.null(lda_result$proportion_of_trace)) {
    add_sheet(
      wb, "Proportion of Trace",
      lda_result$proportion_of_trace
    )
    sheet_count <- sheet_count + 1
  }

  # ---------------------------------------------------------------
  # Sheet 6: Confusion Matrix
  # ---------------------------------------------------------------
  confusion <- get_best_confusion(
    lda_result, test_result
  )
  if (!is.null(confusion)) {
    cm_df <- as.data.frame.matrix(confusion$matrix)
    cm_df <- cbind(
      `True \\ Predicted` = rownames(cm_df),
      cm_df
    )
    rownames(cm_df) <- NULL
    add_sheet(wb, "Confusion Matrix", cm_df)
    sheet_count <- sheet_count + 1

    # Per-class metrics
    add_sheet(
      wb, "Per-Class Metrics",
      confusion$per_class
    )
    sheet_count <- sheet_count + 1
  }

  # ---------------------------------------------------------------
  # Sheet 7: Posterior Probabilities (if not already sheet 1)
  # ---------------------------------------------------------------
  if (is_lda && !is.null(lda_result$scores)) {
    post_df <- build_posterior_sheet(
      lda_result, test_result
    )
    if (!is.null(post_df)) {
      add_sheet(
        wb, "Posterior Probabilities", post_df
      )
      sheet_count <- sheet_count + 1
    }
  }

  # ---------------------------------------------------------------
  # Sheet 8: MDA Subclass Info (MDA only)
  # ---------------------------------------------------------------
  if (
    lda_result$analysis_type == "mda" &&
    !is.null(lda_result$sub_prior)
  ) {
    rows <- lapply(
      names(lda_result$sub_prior),
      function(grp) {
        vals <- lda_result$sub_prior[[grp]]
        data.frame(
          Group = grp,
          Subclass = names(vals),
          Prior = round(as.numeric(vals), 6),
          stringsAsFactors = FALSE
        )
      }
    )
    sp_df <- do.call(rbind, rows)
    rownames(sp_df) <- NULL
    add_sheet(wb, "Subclass Priors", sp_df)
    sheet_count <- sheet_count + 1
  }

  # ---------------------------------------------------------------
  # Sheet 9: Split Summary (train/test mode only)
  # ---------------------------------------------------------------
  if (
    !is.null(test_result) &&
    !is.null(test_result$split_summary)
  ) {
    add_sheet(
      wb, "Split Summary",
      test_result$split_summary
    )
    sheet_count <- sheet_count + 1
  }

  openxlsx$saveWorkbook(wb, file, overwrite = TRUE)

  rhino$log$info(
    "LDA export: Excel saved ",
    "({sheet_count} sheets)"
  )
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

build_scores_sheet <- function(lda_result) {
  scores <- as.data.frame(
    round(lda_result$scores, 6)
  )
  meta <- lda_result$meta

  # Build: meta | predicted class | LD scores
  df <- scores
  if (!is.null(lda_result$predicted_class)) {
    df <- cbind(
      Predicted = as.character(
        lda_result$predicted_class
      ),
      df
    )
  }

  has_real_meta <- !is.null(meta) &&
    nrow(meta) == nrow(df) &&
    !("Row" %in% names(meta) && ncol(meta) == 1)
  if (has_real_meta) {
    df <- cbind(meta, df)
  }
  rownames(df) <- NULL
  df
}


build_posterior_sheet <- function(lda_result,
                                 test_result) {
  is_cv <- !is.null(lda_result$cv)

  posterior <- if (is_cv) {
    lda_result$cv$posterior
  } else if (!is.null(test_result)) {
    test_result$posterior
  } else {
    lda_result$posterior
  }
  if (is.null(posterior)) return(NULL)

  pred_class <- if (is_cv) {
    lda_result$cv$predicted_class
  } else if (!is.null(test_result)) {
    test_result$predicted_class
  } else {
    lda_result$predicted_class
  }

  meta <- if (
    !is.null(test_result) &&
    !is.null(test_result$meta)
  ) {
    test_result$meta
  } else {
    lda_result$meta
  }

  df <- as.data.frame(round(posterior, 6))
  if (!is.null(pred_class)) {
    df <- cbind(
      Predicted = as.character(pred_class), df
    )
  }

  has_real_meta <- !is.null(meta) &&
    nrow(meta) == nrow(df) &&
    !("Row" %in% names(meta) && ncol(meta) == 1)
  if (has_real_meta) {
    df <- cbind(meta, df)
  }
  rownames(df) <- NULL
  df
}


#' Pick the least-optimistic available confusion matrix
#'
#' Priority: cross-validated (most externally valid) > held-out test >
#' resubstitution (same data used for training -- optimistic).
#'
#' @param lda_result Result list from run_lda/run_qda/run_mda
#' @param test_result Optional prediction result for train/test split mode
#' @return List with $matrix, $accuracy, $per_class, or NULL if none
#'   of the three sources is available
#' @export
get_best_confusion <- function(lda_result,
                               test_result) {
  is_cv <- !is.null(lda_result$cv)
  if (is_cv) {
    lda_result$cv$confusion
  } else if (
    !is.null(test_result) &&
    !is.null(test_result$confusion)
  ) {
    test_result$confusion
  } else {
    lda_result$confusion
  }
}


#' Create a standardized RDS bundle for LDA/QDA/MDA export
#'
#' Builds the named list that the prediction module
#' expects when loading an LDA/QDA/MDA .rds file.
#'
#' @param lda_result Result list from run_lda/run_qda/run_mda
#' @param raw_data Data frame, original data before any
#'   transforms
#' @param used_data Data frame, data actually passed to the
#'   model (after transform + scale + NA removal)
#' @param numeric_cols Character vector of measurement
#'   column names
#' @param meta_cols Character vector of metadata column
#'   names
#' @param transform_params List of per-column transform
#'   param lists (from transform_skewed), or empty list
#' @param scale_params List with $center and $scale named
#'   numeric vectors, or NULL if no scaling
#' @param settings List with skewness_correction,
#'   scale_method, prior, etc.
#' @param data_source Character, "raw" or "pca_scores"
#' @param test_result Optional prediction result from run_predict() for
#'   train/test split mode -- used to compute $confusion via the same
#'   priority as create_lda_excel() (CV > held-out test > resubstitution)
#' @return Named list (the bundle)
#' @export
create_lda_bundle <- function(lda_result, raw_data,
                              used_data, numeric_cols,
                              meta_cols,
                              transform_params = list(),
                              scale_params = NULL,
                              settings = list(),
                              data_source = "raw",
                              test_result = NULL) {
  analysis_type <- lda_result$analysis_type
  group_col <- lda_result$grouping_col

  bundle <- list(
    analysis_type = analysis_type,
    model = lda_result$model,
    raw_data = raw_data,
    used_data = used_data,
    group_col = group_col,
    numeric_cols = numeric_cols,
    meta_cols = meta_cols,
    transform_params = transform_params,
    scale_params = scale_params,
    settings = settings,
    data_source = data_source,
    app_version = app_version,
    created = Sys.time()
  )

  # For QDA: include companion LDA model and scores
  if (analysis_type == "qda") {
    bundle$lda_model <- lda_result$lda_model
    bundle$lda_scaling <- lda_result$lda_scaling
    bundle$lda_svd <- lda_result$lda_svd
    bundle$lda_scores <- lda_result$lda_scores
    bundle$lda_proportion_of_trace <-
      lda_result$lda_proportion_of_trace
  }

  # For sPLS-DA: include keepX and selected variables
  if (analysis_type == "splsda") {
    bundle$keep_x <- lda_result$keep_x
    bundle$selected_variables <- lda_result$selected_variables
  }

  # Prediction diagnostics reference stats: component-space group stats
  # for PLS-DA/sPLS-DA (the space these methods actually classify in),
  # original-measurement-space per-group stats for everything else.
  if (analysis_type %in% c("plsda", "splsda")) {
    bundle$group_component_stats <- build_group_component_stats(
      lda_result$scores, used_data[[group_col]]
    )
  } else {
    bundle$group_stats <- build_group_stats(used_data, numeric_cols, group_col)
  }

  bundle$confusion <- get_best_confusion(lda_result, test_result)
  bundle$confusion_source <- if (!is.null(lda_result$cv)) {
    "cv"
  } else if (!is.null(test_result) && !is.null(test_result$confusion)) {
    "held_out_test"
  } else if (!is.null(lda_result$confusion)) {
    "resubstitution"
  } else {
    NA_character_
  }

  rhino$log$info(
    "LDA bundle: created {toupper(analysis_type)}",
    " ({length(numeric_cols)} vars,",
    " {nrow(used_data)} obs,",
    " source='{data_source}')"
  )

  bundle
}

#' Build per-group Mahalanobis reference stats in original measurement space
#'
#' One entry per training group: per-group mean and inverse covariance,
#' used by the Prediction module to compute Mahalanobis distance and
#' typicality probability for LDA/QDA/MDA. Uses per-group (not pooled)
#' covariance, matching QDA's native geometry -- for LDA/MDA this is a
#' strict generalization with no methodological loss.
#'
#' @param used_data Data frame, training data actually used to fit the model
#' @param numeric_cols Character vector of measurement column names
#' @param group_col Character, name of the grouping column in used_data
#' @return Named list (one entry per group) of list($mean, $cov_inv), or
#'   NULL if group_col is missing or every group's covariance is singular
build_group_stats <- function(used_data, numeric_cols, group_col) {
  if (is.null(group_col) || !(group_col %in% names(used_data))) return(NULL)
  x <- as.matrix(used_data[, numeric_cols, drop = FALSE])
  groups <- used_data[[group_col]]
  group_names <- if (is.factor(groups)) levels(groups) else sort(unique(as.character(groups)))

  stats_list <- lapply(group_names, function(g) {
    idx <- as.character(groups) == g
    xg <- x[idx, , drop = FALSE]
    if (nrow(xg) < ncol(xg) + 1) return(NULL)
    cov_g <- stats$cov(xg)
    cov_inv <- tryCatch(solve(cov_g), error = function(e) NULL)
    if (is.null(cov_inv)) return(NULL)
    list(mean = colMeans(xg), cov_inv = cov_inv)
  })
  names(stats_list) <- group_names
  stats_list <- stats_list[!vapply(stats_list, is.null, logical(1))]
  if (length(stats_list) == 0) return(NULL)
  stats_list
}

#' Build per-group Mahalanobis reference stats in component-score space
#'
#' Same shape as build_group_stats(), but computed from PLS-DA/sPLS-DA
#' component scores rather than original measurement space -- that is the
#' space these methods' classification decision actually lives in.
#'
#' @param scores Data frame/matrix of training component scores
#' @param groups Vector of group labels aligned with scores' rows
#' @return Named list (one entry per group) of list($mean, $cov_inv), or
#'   NULL if scores are unavailable or every group's covariance is singular
build_group_component_stats <- function(scores, groups) {
  if (is.null(scores) || nrow(scores) == 0) return(NULL)
  x <- as.matrix(scores)
  group_names <- sort(unique(as.character(groups)))

  stats_list <- lapply(group_names, function(g) {
    idx <- as.character(groups) == g
    xg <- x[idx, , drop = FALSE]
    if (nrow(xg) < ncol(xg) + 1) return(NULL)
    cov_g <- stats$cov(xg)
    cov_inv <- tryCatch(solve(cov_g), error = function(e) NULL)
    if (is.null(cov_inv)) return(NULL)
    list(mean = colMeans(xg), cov_inv = cov_inv)
  })
  names(stats_list) <- group_names
  stats_list <- stats_list[!vapply(stats_list, is.null, logical(1))]
  if (length(stats_list) == 0) return(NULL)
  stats_list
}

add_sheet <- function(wb, sheet_name, data) {
  openxlsx$addWorksheet(wb, sheet_name)
  openxlsx$writeData(wb, sheet_name, data)
  openxlsx$setColWidths(
    wb, sheet_name,
    cols = seq_len(ncol(data)),
    widths = "auto"
  )
}
