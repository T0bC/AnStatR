box::use(
  bsicons,
  bslib,
  shiny,
)

#' Render NA summary info banner
#'
#' Shows an informational alert at the top of the PCA results
#' summarising how many rows were removed due to NAs and which
#' measurement columns contained NAs (with counts and percentages).
#' Returns NULL if no rows were removed.
#'
#' @param na_result List from clean_na_rows() with $rows_removed,
#'   $rows_before, $rows_after, $na_summary
#' @return Shiny tags object or NULL
#' @export
render_na_summary <- function(na_result) {
  if (na_result$rows_removed == 0) return(NULL)

  pct <- round(
    na_result$rows_removed / na_result$rows_before * 100, 1
  )

  # Build per-column detail rows
  na_df <- na_result$na_summary
  col_rows <- lapply(seq_len(nrow(na_df)), function(i) {
    row <- na_df[i, ]
    badge_class <- if (row$na_percent >= 30) {
      "bg-danger"
    } else if (row$na_percent >= 10) {
      "bg-warning text-dark"
    } else {
      "bg-secondary"
    }
    shiny$tags$tr(
      shiny$tags$td(shiny$tags$code(row$column)),
      shiny$tags$td(
        class = "text-end",
        as.character(row$na_count)
      ),
      shiny$tags$td(
        class = "text-end",
        shiny$tags$span(
          class = paste("badge", badge_class),
          paste0(row$na_percent, "%")
        )
      )
    )
  })

  shiny$tags$div(
    class = "alert alert-info",
    role = "alert",
    shiny$tags$div(
      class = "d-flex align-items-center mb-2",
      bsicons$bs_icon("info-circle-fill", class = "me-2"),
      shiny$tags$strong(
        paste0(
          na_result$rows_removed, " of ",
          na_result$rows_before,
          " rows removed (", pct, "%)"
        )
      ),
      shiny$tags$span(
        class = "text-muted ms-2",
        paste0(
          na_result$rows_after,
          " rows remaining for analysis"
        )
      )
    ),
    shiny$tags$details(
      class = "mt-1",
      shiny$tags$summary(
        class = "small",
        "Columns containing NAs"
      ),
      shiny$tags$table(
        class = "table table-sm table-striped mb-0 mt-1",
        shiny$tags$thead(
          shiny$tags$tr(
            shiny$tags$th("Column"),
            shiny$tags$th(class = "text-end", "NA count"),
            shiny$tags$th(class = "text-end", "NA %")
          )
        ),
        shiny$tags$tbody(col_rows)
      ),
      shiny$tags$p(
        class = "text-muted small mt-2 mb-0",
        paste(
          "Columns with high NA percentages reduce",
          "your dataset significantly.",
          "Consider deselecting them."
        )
      )
    )
  )
}
