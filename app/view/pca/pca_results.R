box::use(
  bsicons,
  bslib,
  DT,
  shiny,
)

box::use(
  app/logic/pca/pca_stats[
    compute_var_coord, compute_var_contrib, compute_var_cos2,
    compute_ind_contrib, compute_ind_cos2
  ],
)

#' Render PCA results in collapsible accordion panels
#'
#' Displays eigenvalues, variable results (coordinates,
#' contributions, cos2), individual results, and download
#' buttons inside a bslib accordion.
#'
#' @param pca_result PCA result list from run_pca()
#'   (the $result field, not the wrapper)
#' @param ns Namespace function for download button IDs
#' @param display_ncp Number of dimensions to show in
#'   variable/individual tables. NULL shows all.
#'   Eigenvalue table always shows all components.
#'   Downloads always include all components.
#' @return Shiny tagList with formatted PCA display
#' @export
render_pca_results <- function(pca_result, ns,
                               display_ncp = NULL) {
  variance <- pca_result$variance
  total_dims <- ncol(pca_result$loadings)
  has_contrib <- pca_result$analysis_type != "ipca"

  # Determine effective display limit
  eff_display <- if (
    is.null(display_ncp) || display_ncp >= total_dims
  ) {
    total_dims
  } else {
    display_ncp
  }
  is_truncated <- eff_display < total_dims

  # Limit variable/individual matrices to display dims
  var_display <- limit_var_dims(
    pca_result$loadings, pca_result$scores,
    eff_display, has_contrib
  )
  ind_display <- limit_ind_dims(
    pca_result$scores, pca_result$ind_meta,
    eff_display, has_contrib
  )

  # Info banner when truncated
  truncation_note <- if (is_truncated) {
    shiny$tags$div(
      class = "alert alert-info mb-3 py-2",
      bsicons$bs_icon(
        "info-circle-fill", class = "me-2"
      ),
      sprintf(
        paste(
          "Showing first %d of %d dimensions",
          "(based on optimal components + 2).",
          "Full results available in downloads."
        ),
        eff_display, total_dims
      )
    )
  }

  shiny$tagList(
    truncation_note,
    bslib$accordion(
      id = ns("pca_results_accordion"),
      open = "eigenvalues",
      multiple = TRUE,

      # Eigenvalues panel (always all components)
      bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon(
            "bar-chart-line", class = "me-2"
          ),
          "Eigenvalues & Variance"
        ),
        value = "eigenvalues",
        render_eigenvalues_table(variance, has_contrib)
      ),

      # Variable Results panel (display_ncp dims)
      bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon(
            "diagram-3", class = "me-2"
          ),
          "Variable Results"
        ),
        value = "variable_results",
        render_variable_results(var_display)
      ),

      # Selected Variables panel (sPCA only)
      if (
        pca_result$analysis_type == "spca" &&
        !is.null(pca_result$selected_variables)
      ) {
        bslib$accordion_panel(
          title = shiny$tags$span(
            bsicons$bs_icon(
              "check2-square", class = "me-2"
            ),
            "Selected Variables",
            if (isTRUE(pca_result$keepx_tuned)) {
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
          value = "selected_variables",
          render_selected_variables(
            pca_result$selected_variables,
            keepx_tuned = isTRUE(pca_result$keepx_tuned)
          )
        )
      },

      # Individual Results panel (display_ncp dims)
      bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon("people", class = "me-2"),
          "Individual Results"
        ),
        value = "individual_results",
        render_individual_results(ind_display)
      ),

      # Downloads panel
      bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon(
            "download", class = "me-2"
          ),
          "Download Results"
        ),
        value = "downloads",
        render_download_buttons(ns)
      )
    )
  )
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Render the sPCA Selected Variables table with a tuning note
#'
#' @param selected_variables Named list, one character vector of
#'   non-zero-loading variable names per component (from
#'   run_pca()'s $selected_variables, sPCA only)
#' @param keepx_tuned Logical, whether the keepX values used were
#'   produced by a completed Optimise variable selection run
#' @return Shiny tagList
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
    DT$datatable(
      df,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = "tip",
        order = list()
      ),
      rownames = FALSE,
      class = paste(
        "table table-sm table-striped",
        "table-hover compact"
      )
    )
  )
}


limit_var_dims <- function(loadings, scores, ncp, has_contrib) {
  dim_cols <- paste0("Dim.", seq_len(ncp))
  loadings_d <- loadings[, dim_cols, drop = FALSE]
  result <- list(coord = loadings_d, has_contrib = has_contrib)
  if (has_contrib) {
    var_coord <- compute_var_coord(loadings, scores)[
      , dim_cols, drop = FALSE
    ]
    result$coord <- var_coord
    result$contrib <- compute_var_contrib(loadings)[
      , dim_cols, drop = FALSE
    ]
    result$cos2 <- compute_var_cos2(var_coord)
  }
  result
}


limit_ind_dims <- function(scores, meta, ncp, has_contrib) {
  dim_cols <- paste0("Dim.", seq_len(ncp))
  scores_d <- scores[, dim_cols, drop = FALSE]
  result <- list(
    coord = scores_d, meta = meta, has_contrib = has_contrib
  )
  if (has_contrib) {
    result$contrib <- compute_ind_contrib(scores)[
      , dim_cols, drop = FALSE
    ]
    result$cos2 <- compute_ind_cos2(scores, scores_d)
  }
  result
}


render_eigenvalues_table <- function(variance, has_contrib) {
  eig_df <- as.data.frame(variance)
  eig_df <- cbind(
    Component = rownames(eig_df),
    round(eig_df[, c("variance_percent",
                      "cumulative_variance_percent")], 3)
  )
  rownames(eig_df) <- NULL
  cum_label <- if (has_contrib) {
    "Cumulative (%)"
  } else {
    "Cumulative (%, not ranked)"
  }
  names(eig_df) <- c("Component", "Variance (%)", cum_label)

  n_rows <- nrow(eig_df)
  dom_string <- if (n_rows <= 10) "t" else "tip"

  dt <- DT$datatable(
    eig_df,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = dom_string,
      order = list(),
      columnDefs = list(
        list(
          className = "dt-right",
          targets = seq(1, ncol(eig_df) - 1)
        )
      )
    ),
    rownames = FALSE,
    class = paste(
      "table table-sm table-striped",
      "table-hover compact"
    )
  )

  if (has_contrib) {
    dt <- dt |>
      DT$formatStyle(
        cum_label,
        backgroundColor = DT$styleInterval(
          c(60, 80),
          c("#6c757d40", "#ffc10740", "#19875440")
        ),
        fontWeight = "bold"
      )
  }

  dt
}



render_variable_results <- function(var) {
  contrib_section <- if (var$has_contrib) {
    shiny$tagList(
      shiny$tags$h6(
        class = "mt-2 mb-2", "Contributions (%)"
      ),
      render_sortable_table(var$contrib, "Variable"),
      shiny$tags$h6(
        class = "mt-3 mb-2", "Cos2 (Quality)"
      ),
      render_matrix_table(var$cos2, "Variable")
    )
  } else {
    shiny$tags$div(
      class = "alert alert-secondary mb-2 py-2",
      bsicons$bs_icon(
        "info-circle-fill", class = "me-2"
      ),
      paste(
        "Contribution % and cos2 are not applicable to",
        "IPCA — independent components are not ranked",
        "by variance."
      )
    )
  }

  coord_title <- if (var$has_contrib) {
    "Coordinates"
  } else {
    "Loadings"
  }

  shiny$tagList(
    contrib_section,

    shiny$tags$h6(
      class = "mt-3 mb-2", coord_title
    ),
    render_matrix_table(var$coord, "Variable")
  )
}


render_individual_results <- function(ind) {
  n_ind <- nrow(ind$coord)
  meta <- ind$meta

  too_many_warning <- NULL
  if (n_ind > 500) {
    too_many_warning <- shiny$tags$div(
      class = "alert alert-info mb-2",
      bsicons$bs_icon(
        "info-circle-fill", class = "me-2"
      ),
      sprintf(
        paste(
          "Individual results contain %d",
          "observations. Tables are paginated.",
          "Download the Excel file for full data."
        ),
        n_ind
      )
    )
  }

  contrib_section <- if (ind$has_contrib) {
    shiny$tagList(
      shiny$tags$h6(
        class = "mt-2 mb-2", "Contributions (%)"
      ),
      render_ind_sortable_table(
        ind$contrib, meta
      ),
      shiny$tags$h6(
        class = "mt-3 mb-2", "Cos2 (Quality)"
      ),
      render_ind_sortable_table(
        ind$cos2, meta
      )
    )
  } else {
    shiny$tags$div(
      class = "alert alert-secondary mb-2 py-2",
      bsicons$bs_icon(
        "info-circle-fill", class = "me-2"
      ),
      paste(
        "Contribution % and cos2 are not applicable to",
        "IPCA — independent components are not ranked",
        "by variance."
      )
    )
  }

  shiny$tagList(
    too_many_warning,
    contrib_section,

    shiny$tags$h6(
      class = "mt-3 mb-2", "Scores"
    ),
    render_ind_sortable_table(
      ind$coord, meta
    )
  )
}


render_sortable_table <- function(mat,
                                  row_label = "Item") {
  df <- as.data.frame(mat)
  df <- cbind(Item = rownames(df), round(df, 4))
  rownames(df) <- NULL
  names(df)[1] <- row_label

  n_rows <- nrow(df)
  dom_string <- if (n_rows <= 10) "t" else "tip"

  DT$datatable(
    df,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = dom_string,
      order = list(),
      columnDefs = list(
        list(
          className = "dt-right",
          targets = seq(1, ncol(df) - 1)
        )
      )
    ),
    rownames = FALSE,
    class = paste(
      "table table-sm table-striped",
      "table-hover compact"
    )
  )
}


render_ind_sortable_table <- function(mat, meta) {
  df <- as.data.frame(round(mat, 4))
  has_real_meta <- !is.null(meta) &&
    nrow(meta) == nrow(df) &&
    !("Row" %in% names(meta) && ncol(meta) == 1)

  if (has_real_meta) {
    # Prepend metadata columns for sorting/filtering
    df <- cbind(meta, df)
    rownames(df) <- NULL
    n_meta <- ncol(meta)
  } else {
    df <- cbind(
      Individual = rownames(df), df
    )
    rownames(df) <- NULL
    n_meta <- 1
  }

  n_rows <- nrow(df)
  dom_string <- if (n_rows <= 10) "t" else "tip"

  # Numeric columns start after metadata columns
  numeric_targets <- seq(
    n_meta, ncol(df) - 1
  )

  DT$datatable(
    df,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = dom_string,
      order = list(),
      columnDefs = list(
        list(
          className = "dt-right",
          targets = as.list(numeric_targets)
        )
      )
    ),
    rownames = FALSE,
    class = paste(
      "table table-sm table-striped",
      "table-hover compact"
    )
  )
}


render_matrix_table <- function(mat,
                                row_label = "Item") {
  df <- as.data.frame(mat)
  df <- cbind(Item = rownames(df), round(df, 4))
  rownames(df) <- NULL
  names(df)[1] <- row_label

  shiny$tags$div(
    class = "table-responsive",
    style = "max-height: 300px; overflow-y: auto;",
    shiny$tags$table(
      class = "table table-sm table-striped table-hover",
      shiny$tags$thead(
        class = "sticky-top bg-white",
        shiny$tags$tr(
          lapply(names(df), function(col) {
            cls <- if (col != row_label) {
              "text-end"
            } else {
              ""
            }
            shiny$tags$th(class = cls, col)
          })
        )
      ),
      shiny$tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          row <- df[i, ]
          shiny$tags$tr(
            shiny$tags$td(row[[1]]),
            lapply(
              seq(2, ncol(df)),
              function(j) {
                shiny$tags$td(
                  class = "text-end",
                  sprintf("%.4f", row[[j]])
                )
              }
            )
          )
        })
      )
    )
  )
}


render_download_buttons <- function(ns) {
  shiny$tags$div(
    class = "d-flex flex-column gap-2",

    # Excel download
    shiny$tags$a(
      id = ns("download_pca_excel"),
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
      id = ns("download_pca_rds"),
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
      "Download RDS (PCA Object)"
    ),

    shiny$tags$small(
      class = "text-muted mt-2",
      paste(
        "The RDS file contains the full PCA",
        "result for use in R",
        "(load with readRDS())."
      )
    )
  )
}
