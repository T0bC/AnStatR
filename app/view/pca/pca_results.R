box::use(
  bsicons,
  bslib,
  DT,
  shiny,
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
#' @return Shiny tagList with formatted PCA display
#' @export
render_pca_results <- function(pca_result, ns) {
  eig <- pca_result$eig

  shiny$tagList(
    bslib$accordion(
      id = ns("pca_results_accordion"),
      open = "eigenvalues",
      multiple = TRUE,

      # Eigenvalues panel
      bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon(
            "bar-chart-line", class = "me-2"
          ),
          "Eigenvalues & Variance"
        ),
        value = "eigenvalues",
        render_eigenvalues_table(eig)
      ),

      # Variable Results panel
      bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon(
            "diagram-3", class = "me-2"
          ),
          "Variable Results"
        ),
        value = "variable_results",
        render_variable_results(pca_result$var)
      ),

      # Individual Results panel
      bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon("people", class = "me-2"),
          "Individual Results"
        ),
        value = "individual_results",
        render_individual_results(pca_result$ind)
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

render_eigenvalues_table <- function(eig) {
  eig_df <- as.data.frame(eig)
  eig_df <- cbind(
    Component = rownames(eig_df),
    round(eig_df, 3)
  )
  rownames(eig_df) <- NULL
  names(eig_df) <- c(
    "Component", "Eigenvalue",
    "Variance (%)", "Cumulative (%)"
  )

  shiny$tags$div(
    class = "table-responsive",
    shiny$tags$table(
      class = "table table-sm table-striped",
      shiny$tags$thead(
        shiny$tags$tr(
          lapply(names(eig_df), function(col) {
            cls <- if (col != "Component") {
              "text-end"
            } else {
              ""
            }
            shiny$tags$th(class = cls, col)
          })
        )
      ),
      shiny$tags$tbody(
        lapply(seq_len(nrow(eig_df)), function(i) {
          row <- eig_df[i, ]
          shiny$tags$tr(
            shiny$tags$td(row$Component),
            shiny$tags$td(
              class = "text-end",
              sprintf("%.3f", row$Eigenvalue)
            ),
            shiny$tags$td(
              class = "text-end",
              sprintf(
                "%.2f%%", row$`Variance (%)`
              )
            ),
            shiny$tags$td(
              class = "text-end",
              shiny$tags$span(
                class = variance_badge_class(
                  row$`Cumulative (%)`
                ),
                sprintf(
                  "%.2f%%",
                  row$`Cumulative (%)`
                )
              )
            )
          )
        })
      )
    )
  )
}


variance_badge_class <- function(cum_var) {
  if (cum_var >= 80) "badge bg-success"
  else if (cum_var >= 60) "badge bg-warning text-dark"
  else "badge bg-secondary"
}


render_variable_results <- function(var) {
  shiny$tagList(
    # Contributions (most important for interpretation)
    shiny$tags$h6(
      class = "mt-2 mb-2", "Contributions (%)"
    ),
    render_sortable_table(var$contrib, "Variable"),

    # Coordinates
    shiny$tags$h6(
      class = "mt-3 mb-2", "Coordinates"
    ),
    render_matrix_table(var$coord, "Variable"),

    # Cos2 (quality of representation)
    shiny$tags$h6(
      class = "mt-3 mb-2", "Cos2 (Quality)"
    ),
    render_matrix_table(var$cos2, "Variable")
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

  shiny$tagList(
    too_many_warning,

    # Contributions
    shiny$tags$h6(
      class = "mt-2 mb-2", "Contributions (%)"
    ),
    render_ind_sortable_table(
      ind$contrib, meta
    ),

    # Coordinates
    shiny$tags$h6(
      class = "mt-3 mb-2", "Coordinates"
    ),
    render_ind_sortable_table(
      ind$coord, meta
    ),

    # Cos2
    shiny$tags$h6(
      class = "mt-3 mb-2", "Cos2 (Quality)"
    ),
    render_ind_sortable_table(
      ind$cos2, meta
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
