box::use(
  bsicons,
  bslib,
  shiny,
)

#' Render NA summary info banner
#'
#' Shows an informational alert at the top of the PCA results
#' summarising how many rows were removed due to NAs in
#' measurement columns and which descriptive columns also
#' contain NAs (informational only, no row removal).
#' Returns NULL if no NAs were found anywhere.
#'
#' @param na_result List from clean_na_rows() with $rows_removed,
#'   $rows_before, $rows_after, $na_summary, $meta_na_summary
#' @return Shiny tags object or NULL
#' @export
render_na_summary <- function(na_result) {
  has_meas_na <- nrow(na_result$na_summary) > 0
  has_meta_na <- !is.null(na_result$meta_na_summary) &&
    nrow(na_result$meta_na_summary) > 0

  if (!has_meas_na && !has_meta_na) return(NULL)

  # Header: row removal info
  header <- if (na_result$rows_removed > 0) {
    pct <- round(
      na_result$rows_removed / na_result$rows_before * 100,
      1
    )
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
    )
  } else {
    shiny$tags$div(
      class = "d-flex align-items-center mb-2",
      bsicons$bs_icon("info-circle-fill", class = "me-2"),
      shiny$tags$strong("No rows removed"),
      shiny$tags$span(
        class = "text-muted ms-2",
        paste0(
          na_result$rows_before,
          " rows used for analysis"
        )
      )
    )
  }

  # Measurement columns NA detail
  meas_section <- if (has_meas_na) {
    shiny$tags$details(
      class = "mt-1",
      shiny$tags$summary(
        class = "small",
        paste0(
          "Measurement columns with NAs (",
          nrow(na_result$na_summary), ")"
        )
      ),
      na_table(na_result$na_summary),
      shiny$tags$p(
        class = "text-muted small mt-2 mb-0",
        paste(
          "Rows with NAs in measurement columns",
          "are removed before analysis.",
          "Columns with high NA percentages reduce",
          "your dataset significantly.",
          "Consider deselecting them."
        )
      )
    )
  }

  # Descriptive columns NA detail (informational)
  meta_section <- if (has_meta_na) {
    shiny$tags$details(
      class = "mt-1",
      shiny$tags$summary(
        class = "small",
        paste0(
          "Descriptive columns with NAs (",
          nrow(na_result$meta_na_summary), ")"
        )
      ),
      na_table(na_result$meta_na_summary),
      shiny$tags$p(
        class = "text-muted small mt-2 mb-0",
        paste(
          "NAs in descriptive columns do not cause",
          "row removal but may affect grouping",
          "or labelling in plots."
        )
      )
    )
  }

  shiny$tags$div(
    class = "alert alert-info",
    role = "alert",
    header,
    meas_section,
    meta_section
  )
}

# =============================================================================
# Internal helper (not exported)
# =============================================================================

na_table <- function(na_df) {
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
  )
}
