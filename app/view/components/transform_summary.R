box::use(
  bsicons,
  shiny,
)

#' Render skewness transformation summary info banner
#'
#' Shows an informational alert summarising which measurement
#' columns were transformed to correct skewness, including
#' the direction (left/right), method used, and skewness
#' before/after. Returns NULL if no columns were transformed.
#'
#' @param transform_result List from transform_skewed()$result
#'   with $transformed_cols (data frame) and $skipped_cols
#'   (character vector).
#' @param n_total Integer, total number of measurement columns
#' @return Shiny tags object or NULL
#' @export
render_transform_summary <- function(transform_result,
                                     n_total) {
  has_transformed <- !is.null(
    transform_result$transformed_cols
  ) && nrow(transform_result$transformed_cols) > 0
  has_skipped <- length(transform_result$skipped_cols) > 0

  if (!has_transformed && !has_skipped) return(NULL)

  n_transformed <- if (has_transformed) {
    nrow(transform_result$transformed_cols)
  } else {
    0L
  }

  # Header
  header <- shiny$tags$div(
    class = "d-flex align-items-center mb-2",
    bsicons$bs_icon("info-circle-fill", class = "me-2"),
    shiny$tags$strong(
      paste0(
        n_transformed, " of ", n_total,
        " measurement column",
        if (n_total != 1) "s" else "",
        " transformed (skewness correction)"
      )
    )
  )

  # Transformed columns detail table
  transformed_section <- if (has_transformed) {
    shiny$tags$details(
      class = "mt-1",
      shiny$tags$summary(
        class = "small",
        paste0(
          "Transformed columns (",
          n_transformed, ")"
        )
      ),
      transform_table(transform_result$transformed_cols),
      shiny$tags$p(
        class = "text-muted small mt-2 mb-0",
        shiny$tags$em(
          "Please verify the transformations in the ",
          shiny$tags$strong("Load Data"),
          " \u2192 ",
          shiny$tags$strong("Data Preview"),
          " panel to ensure the skewness was",
          " detected correctly."
        )
      )
    )
  }

  # Skipped columns detail
  skipped_section <- if (has_skipped) {
    shiny$tags$details(
      class = "mt-1",
      shiny$tags$summary(
        class = "small",
        paste0(
          "Skipped columns (",
          length(transform_result$skipped_cols), ")"
        )
      ),
      shiny$tags$p(
        class = "text-muted small mt-1 mb-0",
        paste(
          "The following columns were skewed but",
          "could not be transformed:",
          paste(
            transform_result$skipped_cols,
            collapse = ", "
          )
        )
      )
    )
  }

  shiny$tags$div(
    class = "alert alert-info",
    role = "alert",
    header,
    transformed_section,
    skipped_section
  )
}

# =============================================================================
# Internal helper (not exported)
# =============================================================================

transform_table <- function(transformed_df) {
  col_rows <- lapply(
    seq_len(nrow(transformed_df)),
    function(i) {
      row <- transformed_df[i, ]
      dir_badge <- if (row$direction == "right") {
        shiny$tags$span(
          class = "badge bg-warning text-dark",
          "right-skewed"
        )
      } else {
        shiny$tags$span(
          class = "badge bg-info",
          "left-skewed"
        )
      }
      shiny$tags$tr(
        shiny$tags$td(shiny$tags$code(row$column)),
        shiny$tags$td(dir_badge),
        shiny$tags$td(
          class = "text-end",
          as.character(row$skewness_before)
        ),
        shiny$tags$td(
          class = "text-end",
          as.character(row$skewness_after)
        ),
        shiny$tags$td(
          shiny$tags$small(
            class = "text-muted",
            row$method_used
          )
        )
      )
    }
  )

  shiny$tags$table(
    class = "table table-sm table-striped mb-0 mt-1",
    shiny$tags$thead(
      shiny$tags$tr(
        shiny$tags$th("Column"),
        shiny$tags$th("Direction"),
        shiny$tags$th(class = "text-end", "Before"),
        shiny$tags$th(class = "text-end", "After"),
        shiny$tags$th("Method")
      )
    ),
    shiny$tags$tbody(col_rows)
  )
}
