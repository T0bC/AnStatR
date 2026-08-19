box::use(
  bsicons,
  bslib,
  DT,
  shiny,
)

box::use(
  app/logic/lda/dimension_eval[evaluate_dimensions],
)

#' Render LDA/QDA results in accordion panels with DT tables
#'
#' Consolidates results into three top-level panels:
#' 1. Summary & Model Details (prior, means, coefficients,
#'    proportion of trace)
#' 2. Classification Results (confusion, posterior, split)
#' 3. Download Results (Excel, RDS)
#'
#' Human-readable label for an analysis type
#'
#' Single source of truth for how the five supported methods are named
#' in the UI, so panel titles and summaries cannot drift apart.
#'
#' @param analysis_type Character, one of lda/qda/mda/plsda/splsda
#' @return Character label, e.g. "sPLS-DA"
#' @export
analysis_type_label <- function(analysis_type) {
  if (is.null(analysis_type)) return("LDA")
  switch(
    analysis_type,
    lda = "LDA",
    qda = "QDA",
    mda = "MDA",
    plsda = "PLS-DA",
    splsda = "sPLS-DA",
    "LDA"
  )
}


#' @param lda_result Result list from run_lda()/run_qda()
#' @param ns Namespace function from parent module
#' @param test_result Optional prediction result from
#'   run_predict() for train/test split mode
#' @return Shiny tagList with formatted display
#' @export
render_lda_results <- function(lda_result, ns,
                               test_result = NULL) {
  type_label <- analysis_type_label(lda_result$analysis_type)
  is_cv <- !is.null(lda_result$cv)
  is_split <- !is.null(test_result)

  # Gather classification data
  confusion <- get_confusion(
    lda_result, is_cv, test_result
  )
  posterior <- get_posterior(
    lda_result, is_cv, test_result
  )
  pred_class <- get_predicted_class(
    lda_result, is_cv, test_result
  )
  meta <- get_meta(lda_result, test_result)

  # Accuracy label for summary sub-panel title
  acc_label <- if (is_cv) {
    "LOO-CV Accuracy"
  } else if (is_split) {
    "Test Accuracy"
  } else {
    "Resubstitution Accuracy"
  }
  acc_badge <- if (!is.null(confusion)) {
    acc_pct <- round(confusion$accuracy * 100, 1)
    acc_cls <- if (confusion$accuracy >= 0.9) {
      "bg-success"
    } else if (confusion$accuracy >= 0.7) {
      "bg-warning text-dark"
    } else {
      "bg-danger"
    }
    shiny$tags$span(
      class = "ms-2",
      shiny$tags$span(
        class = paste("badge", acc_cls),
        paste0(acc_pct, "%")
      )
    )
  }

  # Build sub-panels list (dynamic, some conditional)
  sub_panels <- list()

  # 1. Summary / Accuracy
  sub_panels[[length(sub_panels) + 1]] <-
    bslib$accordion_panel(
      title = shiny$tags$span(
        bsicons$bs_icon(
          "speedometer2", class = "me-2"
        ),
        acc_label,
        acc_badge
      ),
      value = "summary_sub",
      build_summary_badge(
        lda_result, type_label, is_cv, is_split,
        test_result
      )
    )

  # 2. Prior Probabilities (not applicable to PLS-DA/sPLS-DA)
  if (!is.null(lda_result$prior)) {
    sub_panels[[length(sub_panels) + 1]] <-
      bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon(
            "pie-chart", class = "me-2"
          ),
          "Prior Probabilities"
        ),
        value = "prior_sub",
        render_prior_table(lda_result$prior)
      )
  }

  # 3. Group Means
  sub_panels[[length(sub_panels) + 1]] <-
    bslib$accordion_panel(
      title = shiny$tags$span(
        bsicons$bs_icon("table", class = "me-2"),
        "Group Means"
      ),
      value = "means_sub",
      render_means_table(lda_result$means)
    )

  # 4. LD Coefficients / Component Loadings
  # (LDA, MDA, PLS-DA/sPLS-DA, model mode)
  if (
    lda_result$analysis_type %in%
      c("lda", "mda", "plsda", "splsda") &&
    !is.null(lda_result$scaling)
  ) {
    coef_title <- switch(
      lda_result$analysis_type,
      mda = "Discriminant Coefficients",
      plsda = "Component Loadings",
      splsda = "Component Loadings",
      "Coefficients of Linear Discriminants"
    )
    sub_panels[[length(sub_panels) + 1]] <-
      bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon(
            "arrows-expand-vertical",
            class = "me-2"
          ),
          coef_title
        ),
        value = "scaling_sub",
        render_scaling_table(lda_result$scaling)
      )
  }

  # 4b2. VIP Scores (PLS-DA/sPLS-DA only)
  if (
    lda_result$analysis_type %in% c("plsda", "splsda") &&
    !is.null(lda_result$vip)
  ) {
    sub_panels[[length(sub_panels) + 1]] <-
      bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon(
            "sort-numeric-down", class = "me-2"
          ),
          "VIP Scores"
        ),
        value = "vip_sub",
        render_vip_table(lda_result$vip)
      )
  }

  # 4c. Selected Variables (sPLS-DA only)
  if (
    lda_result$analysis_type == "splsda" &&
    !is.null(lda_result$selected_variables)
  ) {
    sub_panels[[length(sub_panels) + 1]] <-
      bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon(
            "check2-square", class = "me-2"
          ),
          "Selected Variables",
          # Flag an untuned keepX so a UI default is never mistaken
          # for a cross-validated variable selection.
          if (isTRUE(lda_result$keepx_tuned)) {
            shiny$tags$span(
              class = "badge bg-success ms-2",
              "keepX tuned"
            )
          } else {
            shiny$tags$span(
              class = "badge bg-warning text-dark ms-2",
              "keepX not tuned"
            )
          }
        ),
        value = "selected_vars_sub",
        render_selected_variables(
          lda_result$selected_variables,
          keepx_tuned = isTRUE(lda_result$keepx_tuned)
        )
      )
  }

  # 4b. MDA Subclass Information (MDA only)
  if (
    lda_result$analysis_type == "mda" &&
    !is.null(lda_result$sub_prior)
  ) {
    sub_panels[[length(sub_panels) + 1]] <-
      bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon(
            "diagram-3", class = "me-2"
          ),
          "MDA Subclass Information"
        ),
        value = "mda_sub",
        render_mda_subclass_info(lda_result)
      )
  }

  # 5. Proportion of Trace / Explained Variance (model mode)
  if (!is.null(lda_result$proportion_of_trace)) {
    trace_title <- if (
      lda_result$analysis_type %in% c("plsda", "splsda")
    ) {
      "Explained Variance"
    } else {
      "Proportion of Trace"
    }
    sub_panels[[length(sub_panels) + 1]] <-
      bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon(
            "bar-chart-line", class = "me-2"
          ),
          trace_title
        ),
        value = "trace_sub",
        render_trace_table(
          lda_result$proportion_of_trace,
          analysis_type = lda_result$analysis_type
        )
      )
  }

  # 5b. Dimension Evaluation (ANOVA)
  has_scores <- !is.null(lda_result$scores) ||
    (!is.null(lda_result$lda_scores) &&
      lda_result$analysis_type == "qda")
  if (has_scores && !is_cv) {
    dim_eval_res <- evaluate_dimensions(lda_result)
    if (isTRUE(dim_eval_res$success)) {
      qda_note <- if (
        lda_result$analysis_type == "qda"
      ) {
        shiny$tags$small(
          class = "text-info d-block mb-2",
          paste0(
            "Based on companion LDA projection ",
            "(QDA has no linear discriminant axes)."
          )
        )
      }
      sub_panels[[length(sub_panels) + 1]] <-
        bslib$accordion_panel(
          title = shiny$tags$span(
            bsicons$bs_icon(
              "clipboard-data", class = "me-2"
            ),
            "Dimension Evaluation (ANOVA)"
          ),
          value = "dim_eval_sub",
          shiny$tagList(
            qda_note,
            render_dim_eval_table(dim_eval_res$result)
          )
        )
    }
  }

  # 6. Confusion Matrix
  if (!is.null(confusion)) {
    sub_panels[[length(sub_panels) + 1]] <-
      bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon(
            "grid-3x3", class = "me-2"
          ),
          "Confusion Matrix"
        ),
        value = "confusion_sub",
        render_confusion(confusion),
        # PLS-DA/sPLS-DA classify using every component, but the
        # scores plot shows only two axes. With ncomp > 2 the plot
        # can look cleanly separated while the matrix disagrees.
        if (
          lda_result$analysis_type %in% c("plsda", "splsda") &&
            !is.null(lda_result$ncomp) &&
            lda_result$ncomp > 2
        ) {
          shiny$tags$small(
            class = "text-muted mt-2 d-block",
            paste0(
              "Classification uses all ", lda_result$ncomp,
              " components; the Scores Plot shows only the two",
              " selected ones. Groups that look cleanly separated",
              " in the plot may still be misclassified here (and",
              " vice versa) — the plot is a 2D shadow of a ",
              lda_result$ncomp, "-dimensional model."
            )
          )
        }
      )
  }

  # 7. Posterior Probabilities
  if (!is.null(posterior)) {
    post_label <- if (is_cv) {
      "Posterior Probabilities (LOO-CV)"
    } else if (is_split) {
      "Posterior Probabilities (Test Set)"
    } else {
      "Posterior Probabilities (All Data)"
    }
    sub_panels[[length(sub_panels) + 1]] <-
      bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon(
            "percent", class = "me-2"
          ),
          post_label
        ),
        value = "posterior_sub",
        render_posterior_table(
          posterior, pred_class, meta
        )
      )
  }

  # 8. Split Summary (train/test mode only)
  if (is_split && !is.null(test_result)) {
    sub_panels[[length(sub_panels) + 1]] <-
      bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon(
            "scissors", class = "me-2"
          ),
          "Train / Test Split"
        ),
        value = "split_sub",
        render_split_info(test_result)
      )
  }

  # 9. Download Results
  sub_panels[[length(sub_panels) + 1]] <-
    bslib$accordion_panel(
      title = shiny$tags$span(
        bsicons$bs_icon(
          "download", class = "me-2"
        ),
        "Download Results"
      ),
      value = "downloads_sub",
      render_download_buttons(ns)
    )

  # Return nested accordion (like PCA pattern)
  shiny$tagList(
    do.call(
      bslib$accordion,
      c(
        list(
          id = ns("lda_results_accordion"),
          open = "summary_sub",
          multiple = TRUE
        ),
        unname(sub_panels)
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
  } else if (!is.null(lda_result$ncomp)) {
    lda_result$ncomp
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


render_download_buttons <- function(ns) {
  shiny$tags$div(
    class = "d-flex flex-column gap-2",

    # Excel download
    shiny$tags$a(
      id = ns("download_lda_excel"),
      class = paste(
        "btn btn-outline-primary",
        "shiny-download-link"
      ),
      href = "",
      target = "_blank",
      download = NA,
      bsicons$bs_icon(
        "file-earmark-excel", class = "me-2"
      ),
      "Download Excel (All Results)"
    ),

    # RDS download
    shiny$tags$a(
      id = ns("download_lda_rds"),
      class = paste(
        "btn btn-outline-secondary",
        "shiny-download-link"
      ),
      href = "",
      target = "_blank",
      download = NA,
      bsicons$bs_icon(
        "file-earmark-code", class = "me-2"
      ),
      "Download RDS (LDA/QDA Object)"
    ),

    shiny$tags$small(
      class = "text-muted mt-2",
      paste(
        "Excel sheet 1 contains LD scores",
        "(or posterior probabilities for QDA)",
        "with metadata — ready for downstream",
        "clustering. The RDS file contains the",
        "full result for use in R",
        "(load with readRDS())."
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
  # Transposed to variables-as-rows: there are almost always far more
  # measurement variables than groups, so groups-as-columns keeps the
  # table narrow and lets DT paginate the variables instead of forcing
  # horizontal scrolling that pushes the row label off-screen.
  # This also matches the Coefficients/VIP tables' orientation.
  t_means <- t(as.matrix(means))
  df <- cbind(
    Variable = rownames(t_means),
    as.data.frame(round(t_means, 4))
  )
  rownames(df) <- NULL

  shiny$tagList(
    make_dt(df, page_length = 20),
    shiny$tags$small(
      class = "text-muted mt-2 d-block",
      paste(
        "Rows are measurement variables, columns are groups.",
        "Each cell is that group's mean for that variable, in the",
        "units the model was fitted on (scaled units if scaling",
        "was applied)."
      )
    )
  )
}


render_scaling_table <- function(scaling) {
  df <- cbind(
    Variable = rownames(scaling),
    as.data.frame(round(scaling, 6))
  )
  rownames(df) <- NULL
  make_dt(df, page_length = 10)
}


render_vip_table <- function(vip_df) {
  df <- cbind(
    Variable = rownames(vip_df),
    as.data.frame(round(vip_df, 4))
  )
  rownames(df) <- NULL

  # Sort by first component's VIP, descending
  if (ncol(df) >= 2) {
    df <- df[order(-df[[2]]), ]
    rownames(df) <- NULL
  }

  dt <- make_dt(df, page_length = 10)
  # Highlight every component column, not just Comp1 — the caption
  # below claims "VIP > 1 (highlighted)" without qualification.
  if (ncol(vip_df) >= 1) {
    dt <- dt |>
      DT$formatStyle(
        colnames(vip_df),
        backgroundColor = DT$styleInterval(
          c(1),
          c("#6c757d40", "#19875440")
        ),
        fontWeight = "bold"
      )
  }

  shiny$tagList(
    dt,
    shiny$tags$small(
      class = "text-muted mt-2 d-block",
      paste(
        "Variable Importance in Projection (VIP): aggregates",
        "each variable's contribution across all components,",
        "weighted by the variance each component explains.",
        "Unlike per-component loadings, VIP gives one",
        "importance score per variable across the whole model.",
        "Variables with VIP > 1 (highlighted) are considered",
        "above-average contributors to the model's overall",
        "group separation — the conventional threshold used",
        "in the PLS-DA/sPLS-DA literature. For sPLS-DA, VIP",
        "is computed on the same fitted model as the Selected",
        "Variables list, so a variable with zero loading on",
        "every component (never selected) will always show",
        "VIP = 0 here; a high-VIP variable that is missing from",
        "the Selected Variables list on a specific component",
        "simply means it was selected on a different component."
      )
    )
  )
}


render_selected_variables <- function(selected_variables,
                                     keepx_tuned = FALSE) {
  rows <- lapply(names(selected_variables), function(comp) {
    vars <- selected_variables[[comp]]
    if (length(vars) == 0) return(NULL)
    data.frame(
      Component = comp, Variable = vars,
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  if (is.null(df)) {
    return(shiny$tags$p(
      class = "text-muted small",
      "No variables were selected for any component."
    ))
  }
  rownames(df) <- NULL

  # Point-of-decision nudge: this list is only as trustworthy as the
  # keepX that produced it, so say so right where it is read.
  tuning_note <- if (keepx_tuned) {
    shiny$tags$div(
      class = "alert alert-success py-2 small mb-2",
      shiny$tags$strong("keepX was tuned. "),
      "These counts were chosen by cross-validation, so this",
      " selection reflects the data rather than a default."
    )
  } else {
    shiny$tags$div(
      class = "alert alert-warning py-2 small mb-2",
      shiny$tags$strong("keepX was not tuned. "),
      "The number of variables kept per component came from the",
      " values in the sidebar, not from the data. Before reporting",
      " this list, run ",
      shiny$tags$strong("Optimise variable selection"),
      " in the Analysis Settings tab to let cross-validation choose",
      " how many variables each component should keep."
    )
  }

  shiny$tagList(
    tuning_note,
    make_dt(df, page_length = 20),
    shiny$tags$small(
      class = "text-muted mt-2 d-block",
      paste(
        "These are the measurement columns with a",
        "nonzero loading on each component after sparse",
        "selection — the parameters most responsible",
        "for group separation on that component.",
        "Highly correlated variables may share selection",
        "credit somewhat arbitrarily; inspect correlated",
        "groups together rather than trusting a single",
        "variable in isolation."
      )
    )
  )
}


render_mda_subclass_info <- function(lda_result) {
  parts <- list()

  # Subclass priors (list of named vectors per group)
  sub_prior <- lda_result$sub_prior
  if (!is.null(sub_prior) && is.list(sub_prior)) {
    rows <- lapply(
      names(sub_prior), function(grp) {
        vals <- sub_prior[[grp]]
        data.frame(
          Group = grp,
          Subclass = names(vals),
          Prior = round(as.numeric(vals), 4),
          stringsAsFactors = FALSE
        )
      }
    )
    sp_df <- do.call(rbind, rows)
    rownames(sp_df) <- NULL
    parts[[length(parts) + 1]] <- shiny$tagList(
      shiny$tags$h6(
        class = "mt-2 mb-2",
        "Subclass Priors"
      ),
      make_dt(sp_df, page_length = 20)
    )
  }

  # Model summary info
  info_items <- list()
  if (!is.null(lda_result$dimension)) {
    info_items[[length(info_items) + 1]] <-
      shiny$tags$li(paste(
        "Dimension:", lda_result$dimension
      ))
  }
  if (!is.null(lda_result$subclasses)) {
    info_items[[length(info_items) + 1]] <-
      shiny$tags$li(paste(
        "Subclasses per group:",
        lda_result$subclasses
      ))
  }
  if (!is.null(lda_result$deviance)) {
    info_items[[length(info_items) + 1]] <-
      shiny$tags$li(paste(
        "Deviance:",
        round(lda_result$deviance, 3)
      ))
  }
  if (length(info_items) > 0) {
    parts[[length(parts) + 1]] <- shiny$tags$div(
      class = "mt-2",
      shiny$tags$h6("Model Details"),
      shiny$tags$ul(info_items)
    )
  }

  do.call(shiny$tagList, parts)
}


render_trace_table <- function(trace_df, analysis_type = NULL) {
  dt <- DT$datatable(
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

  # The grey/yellow/green thresholds are calibrated for LDA/MDA
  # between-group variance. For PLS-DA/sPLS-DA the same column is
  # X-variance, where a low value need not mean weak separation.
  if (!is.null(analysis_type) &&
        analysis_type %in% c("plsda", "splsda")) {
    return(shiny$tagList(
      dt,
      shiny$tags$small(
        class = "text-muted mt-2 d-block",
        paste(
          "For PLS-DA/sPLS-DA these values describe variance",
          "explained in the measurement variables by each",
          "component, not between-group variance as in LDA.",
          "A low proportion does not necessarily mean weak group",
          "separation on that component, so the colour thresholds",
          "should be read with caution here. Cross-check against",
          "the Component Diagnostics (perf) error rate and the",
          "Dimension Evaluation (ANOVA) table for a",
          "group-separation-specific view."
        )
      )
    ))
  }

  dt
}


render_dim_eval_table <- function(dim_eval_df) {
  # Rename columns for display
  display_df <- dim_eval_df
  colnames(display_df) <- c(
    "Dimension", "F", "p-value",
    "R\u00b2 (%)", "Sig."
  )

  dt <- DT$datatable(
    display_df,
    options = list(
      pageLength = 20,
      scrollX = TRUE,
      dom = "t",
      order = list(),
      columnDefs = list(
        list(
          className = "dt-right",
          targets = c(1, 2, 3)
        ),
        list(
          className = "dt-center",
          targets = 4
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
      "R\u00b2 (%)",
      backgroundColor = DT$styleInterval(
        c(10, 25),
        c("#6c757d40", "#ffc10740", "#19875440")
      ),
      fontWeight = "bold"
    )

  # R2 ranks the axes by how much group separation each one carries,
  # which is exactly the question "which axes should I plot?".
  # Recommend the top two so the user does not have to read the
  # table and translate it into Plotting Controls settings by hand.
  recommendation <- NULL
  if (nrow(dim_eval_df) >= 2 && !all(is.na(dim_eval_df$R2))) {
    ord <- order(-dim_eval_df$R2)
    best <- dim_eval_df$Dimension[ord][1:2]
    best_r2 <- dim_eval_df$R2[ord][1:2]
    shown_default <- setequal(best, dim_eval_df$Dimension[1:2])
    recommendation <- shiny$tags$div(
      class = "alert alert-info py-2 small mt-2 mb-0",
      shiny$tags$strong("Best axes to plot: "),
      sprintf(
        "%s (R² %.1f%%) and %s (R² %.1f%%). ",
        best[1], best_r2[1], best[2], best_r2[2]
      ),
      "These two axes carry the most group separation. ",
      if (shown_default) {
        "The Scores Plot shows them by default."
      } else {
        paste0(
          "The Scores Plot defaults to the first two axes, so set ",
          "Dim.X and Dim.Y in the Plotting Controls tab to these ",
          "to see the clearest separation."
        )
      }
    )
  }

  shiny$tagList(
    dt,
    shiny$tags$small(
      class = "text-muted mt-2 d-block",
      paste0(
        "One-way ANOVA per dimension: ",
        "F and R\u00b2 measure how well the ",
        "grouping variable explains variance ",
        "in each discriminant axis. ",
        "Higher R² means that axis separates ",
        "the groups more strongly. ",
        "Significance: *** p<0.001, ** p<0.01, ",
        "* p<0.05, . p<0.1"
      )
    ),
    recommendation
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
