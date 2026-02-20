box::use(
  openxlsx,
  rhino,
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
#' @return NULL (side effect: writes file)
#' @export
create_lda_excel <- function(lda_result, file,
                             test_result = NULL) {
  wb <- openxlsx$createWorkbook()
  is_lda <- lda_result$analysis_type == "lda"
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
  # Sheet 4: LD Coefficients (LDA only)
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
  # Sheet 8: Split Summary (train/test mode only)
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


add_sheet <- function(wb, sheet_name, data) {
  openxlsx$addWorksheet(wb, sheet_name)
  openxlsx$writeData(wb, sheet_name, data)
  openxlsx$setColWidths(
    wb, sheet_name,
    cols = seq_len(ncol(data)),
    widths = "auto"
  )
}
