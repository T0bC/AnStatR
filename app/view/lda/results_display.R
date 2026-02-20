box::use(
  bsicons,
  bslib,
  DT,
  shiny,
)

#' Render LDA/QDA results in accordion panels with DT tables
#'
#' Displays prior probabilities, group means, LD coefficients
#' (LDA only), proportion of trace (LDA only), classification
#' results, confusion matrix, and per-class metrics.
#'
#' @param lda_result Result list from run_lda()/run_qda()
#' @param ns Namespace function from parent module
#' @param test_result Optional prediction result from
#'   run_predict() for train/test split mode
#' @return Shiny tagList with formatted display
#' @export
render_lda_results <- function(lda_result, ns,
                               test_result = NULL) {
  type_label <- if (
    lda_result$analysis_type == "lda"
  ) "LDA" else "QDA"
  is_cv <- !is.null(lda_result$cv)
  is_split <- !is.null(test_result)

  # Summary badge
  summary_badge <- build_summary_badge(
    lda_result, type_label, is_cv, is_split,
    test_result
  )

  # Build accordion panels
  panels <- list()

  # 1. Summary & Classification accuracy
  panels[["summary"]] <- bslib$accordion_panel(
    title = shiny$tags$span(
      bsicons$bs_icon("speedometer2", class = "me-2"),
      paste(type_label, "Summary")
    ),
    value = "summary_panel",
    summary_badge
  )

  # 2. Prior probabilities
  panels[["prior"]] <- bslib$accordion_panel(
    title = shiny$tags$span(
      bsicons$bs_icon("pie-chart", class = "me-2"),
      "Prior Probabilities"
    ),
    value = "prior_panel",
    render_prior_table(lda_result$prior)
  )

  # 3. Group means
  panels[["means"]] <- bslib$accordion_panel(
    title = shiny$tags$span(
      bsicons$bs_icon("table", class = "me-2"),
      "Group Means"
    ),
    value = "means_panel",
    render_means_table(lda_result$means)
  )

  # 4. LD Coefficients (LDA only, model mode)
  if (
    lda_result$analysis_type == "lda" &&
    !is.null(lda_result$scaling)
  ) {
    panels[["scaling"]] <- bslib$accordion_panel(
      title = shiny$tags$span(
        bsicons$bs_icon(
          "arrows-expand-vertical", class = "me-2"
        ),
        "Coefficients of Linear Discriminants"
      ),
      value = "scaling_panel",
      render_scaling_table(lda_result$scaling)
    )
  }

  # 5. Proportion of trace (LDA only, model mode)
  if (!is.null(lda_result$proportion_of_trace)) {
    panels[["trace"]] <- bslib$accordion_panel(
      title = shiny$tags$span(
        bsicons$bs_icon(
          "bar-chart-line", class = "me-2"
        ),
        "Proportion of Trace"
      ),
      value = "trace_panel",
      render_trace_table(
        lda_result$proportion_of_trace
      )
    )
  }

  # 6. Confusion matrix & per-class metrics
  confusion <- get_confusion(
    lda_result, is_cv, test_result
  )
  if (!is.null(confusion)) {
    cm_label <- if (is_cv) {
      "Classification (LOO-CV)"
    } else if (is_split) {
      "Classification (Test Set)"
    } else {
      "Classification (Resubstitution)"
    }
    panels[["confusion"]] <- bslib$accordion_panel(
      title = shiny$tags$span(
        bsicons$bs_icon("grid-3x3", class = "me-2"),
        cm_label
      ),
      value = "confusion_panel",
      render_confusion(confusion)
    )
  }

  # 7. Posterior probabilities
  posterior <- get_posterior(
    lda_result, is_cv, test_result
  )
  pred_class <- get_predicted_class(
    lda_result, is_cv, test_result
  )
  meta <- get_meta(lda_result, test_result)
  if (!is.null(posterior)) {
    post_label <- if (is_cv) {
      "Posterior Probabilities (LOO-CV)"
    } else if (is_split) {
      "Posterior Probabilities (Test Set)"
    } else {
      "Posterior Probabilities (All Data)"
    }
    panels[["posterior"]] <- bslib$accordion_panel(
      title = shiny$tags$span(
        bsicons$bs_icon("percent", class = "me-2"),
        post_label
      ),
      value = "posterior_panel",
      render_posterior_table(
        posterior, pred_class, meta
      )
    )
  }

  # 8. Split summary (train/test mode only)
  if (is_split && !is.null(test_result)) {
    panels[["split"]] <- bslib$accordion_panel(
      title = shiny$tags$span(
        bsicons$bs_icon("scissors", class = "me-2"),
        "Train / Test Split"
      ),
      value = "split_panel",
      render_split_info(test_result)
    )
  }

  shiny$tagList(
    do.call(
      bslib$accordion,
      c(
        list(
          id = ns("lda_results_accordion"),
          open = "summary_panel",
          multiple = TRUE
        ),
        panels
      )
    )
  )
}


# =============================================================================
# Internal helpers
# =============================================================================

build_summary_badge <- function(lda_result, type_label,
                                is_cv, is_split,
                                test_result) {
  n <- lda_result$n
  p <- lda_result$p
  ng <- lda_result$n_groups

  confusion <- get_confusion(
    lda_result, is_cv, test_result
  )
  acc <- if (!is.null(confusion)) {
    confusion$accuracy
  } else {
    NULL
  }

  acc_badge <- if (!is.null(acc)) {
    acc_pct <- round(acc * 100, 1)
    acc_class <- if (acc >= 0.9) {
      "bg-success"
    } else if (acc >= 0.7) {
      "bg-warning text-dark"
    } else {
      "bg-danger"
    }
    acc_label <- if (is_cv) {
      "LOO-CV Accuracy"
    } else if (is_split) {
      "Test Accuracy"
    } else {
      "Resubstitution Accuracy"
    }
    shiny$tags$div(
      class = "mb-2",
      shiny$tags$span(
        class = paste("badge fs-6", acc_class),
        paste0(acc_pct, "%")
      ),
      shiny$tags$span(
        class = "ms-2 text-muted",
        acc_label
      )
    )
  }

  n_ld <- if (!is.null(lda_result$svd)) {
    length(lda_result$svd)
  } else {
    NULL
  }

  shiny$tags$div(
    acc_badge,
    shiny$tags$dl(
      class = "row mb-0",
      shiny$tags$dt(
        class = "col-sm-5", "Analysis"
      ),
      shiny$tags$dd(
        class = "col-sm-7", type_label
      ),
      shiny$tags$dt(
        class = "col-sm-5", "Observations"
      ),
      shiny$tags$dd(class = "col-sm-7", n),
      shiny$tags$dt(
        class = "col-sm-5", "Variables"
      ),
      shiny$tags$dd(class = "col-sm-7", p),
      shiny$tags$dt(
        class = "col-sm-5", "Groups"
      ),
      shiny$tags$dd(
        class = "col-sm-7",
        paste0(
          ng, " (",
          paste(
            lda_result$group_levels,
            collapse = ", "
          ),
          ")"
        )
      ),
      if (!is.null(n_ld)) shiny$tagList(
        shiny$tags$dt(
          class = "col-sm-5",
          "Discriminant axes"
        ),
        shiny$tags$dd(class = "col-sm-7", n_ld)
      )
    )
  )
}


render_prior_table <- function(prior) {
  df <- data.frame(
    Group = names(prior),
    Prior = round(as.numeric(prior), 4),
    stringsAsFactors = FALSE
  )
  make_dt(df, page_length = 20)
}


render_means_table <- function(means) {
  df <- cbind(
    Group = rownames(means),
    as.data.frame(round(means, 4))
  )
  rownames(df) <- NULL
  make_dt(df, page_length = 20)
}


render_scaling_table <- function(scaling) {
  df <- cbind(
    Variable = rownames(scaling),
    as.data.frame(round(scaling, 6))
  )
  rownames(df) <- NULL
  make_dt(df, page_length = 20)
}


render_trace_table <- function(trace_df) {
  DT$datatable(
    trace_df,
    options = list(
      pageLength = 20,
      scrollX = TRUE,
      dom = "t",
      order = list(),
      columnDefs = list(
        list(
          className = "dt-right",
          targets = seq(1, ncol(trace_df) - 1)
        )
      )
    ),
    rownames = FALSE,
    class = paste(
      "table table-sm table-striped",
      "table-hover compact"
    )
  ) |>
    DT$formatStyle(
      "Cumulative",
      backgroundColor = DT$styleInterval(
        c(0.6, 0.8),
        c("#6c757d40", "#ffc10740", "#19875440")
      ),
      fontWeight = "bold"
    )
}


render_confusion <- function(confusion) {
  # Confusion matrix as a table
  cm <- confusion$matrix
  cm_df <- as.data.frame.matrix(cm)
  cm_df <- cbind(
    `True \\ Predicted` = rownames(cm_df),
    cm_df
  )
  rownames(cm_df) <- NULL

  # Per-class metrics
  pc <- confusion$per_class

  shiny$tagList(
    shiny$tags$h6(
      class = "mt-2 mb-2", "Confusion Matrix"
    ),
    make_dt(cm_df, page_length = 20),
    shiny$tags$h6(
      class = "mt-3 mb-2", "Per-Class Metrics"
    ),
    make_dt(pc, page_length = 20),
    shiny$tags$small(
      class = "text-muted",
      paste(
        "Overall accuracy:",
        round(confusion$accuracy * 100, 1), "%"
      )
    )
  )
}


render_posterior_table <- function(posterior,
                                  pred_class,
                                  meta) {
  df <- as.data.frame(round(posterior, 4))

  # Prepend predicted class
  if (!is.null(pred_class)) {
    df <- cbind(
      Predicted = as.character(pred_class), df
    )
  }

  # Prepend metadata
  if (!is.null(meta) && nrow(meta) == nrow(df)) {
    has_real_meta <- !(
      "Row" %in% names(meta) && ncol(meta) == 1
    )
    if (has_real_meta) {
      df <- cbind(meta, df)
    }
  }
  rownames(df) <- NULL

  n_rows <- nrow(df)
  too_many <- if (n_rows > 500) {
    shiny$tags$div(
      class = "alert alert-info mb-2 py-2",
      bsicons$bs_icon(
        "info-circle-fill", class = "me-2"
      ),
      sprintf(
        "%d observations. Table is paginated.",
        n_rows
      )
    )
  }

  shiny$tagList(
    too_many,
    make_dt(df, page_length = 10)
  )
}


render_split_info <- function(test_result) {
  if (is.null(test_result$split_summary)) {
    return(NULL)
  }
  shiny$tagList(
    shiny$tags$h6(
      class = "mt-2 mb-2",
      "Stratified Split Summary"
    ),
    make_dt(
      test_result$split_summary,
      page_length = 20
    )
  )
}


# Shared DT helper
make_dt <- function(df, page_length = 10) {
  n_rows <- nrow(df)
  dom_string <- if (n_rows <= page_length) {
    "t"
  } else {
    "tip"
  }

  # Right-align numeric columns
  numeric_targets <- which(
    vapply(df, is.numeric, logical(1))
  ) - 1  # 0-indexed

  col_defs <- if (length(numeric_targets) > 0) {
    list(
      list(
        className = "dt-right",
        targets = as.list(numeric_targets)
      )
    )
  } else {
    list()
  }

  DT$datatable(
    df,
    options = list(
      pageLength = page_length,
      scrollX = TRUE,
      dom = dom_string,
      order = list(),
      columnDefs = col_defs
    ),
    rownames = FALSE,
    class = paste(
      "table table-sm table-striped",
      "table-hover compact"
    )
  )
}


# Accessors that unify CV / split / model-only paths

get_confusion <- function(lda_result, is_cv,
                          test_result) {
  if (is_cv && !is.null(lda_result$cv)) {
    lda_result$cv$confusion
  } else if (
    !is.null(test_result) &&
    !is.null(test_result$confusion)
  ) {
    test_result$confusion
  } else if (!is.null(lda_result$confusion)) {
    lda_result$confusion
  } else {
    NULL
  }
}


get_posterior <- function(lda_result, is_cv,
                          test_result) {
  if (is_cv && !is.null(lda_result$cv)) {
    lda_result$cv$posterior
  } else if (!is.null(test_result)) {
    test_result$posterior
  } else if (!is.null(lda_result$posterior)) {
    lda_result$posterior
  } else {
    NULL
  }
}


get_predicted_class <- function(lda_result, is_cv,
                                test_result) {
  if (is_cv && !is.null(lda_result$cv)) {
    lda_result$cv$predicted_class
  } else if (!is.null(test_result)) {
    test_result$predicted_class
  } else if (
    !is.null(lda_result$predicted_class)
  ) {
    lda_result$predicted_class
  } else {
    NULL
  }
}


get_meta <- function(lda_result, test_result) {
  if (!is.null(test_result) &&
      !is.null(test_result$meta)) {
    test_result$meta
  } else {
    lda_result$meta
  }
}
