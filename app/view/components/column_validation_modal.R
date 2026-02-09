box::use(
  bsicons,
  shiny,
)

#' Create modal dialog for column naming convention warnings
#'
#' Displays which columns are descriptive, measurement, and ambiguous
#' based on the result of column_utils$validate_column_naming().
#'
#' @param validation_result List returned by column_utils$validate_column_naming()
#' @return A shiny modalDialog object
#' @export
create_modal <- function(validation_result) {
  shiny$modalDialog(
    title = shiny$tags$span(
      bsicons$bs_icon(
        "exclamation-triangle-fill",
        class = "text-warning"
      ),
      " Column Naming Convention Warning"
    ),
    size = "l",
    easyClose = TRUE,
    footer = shiny$modalButton("Understood"),

    shiny$tags$div(
      shiny$tags$p(
        "Your data contains columns that don't strictly",
        " follow the expected naming conventions."
      ),
      shiny$tags$hr(),

      shiny$tags$h5("Expected Conventions:"),
      shiny$tags$ul(
        shiny$tags$li(
          shiny$tags$strong("Descriptive columns: "),
          "UPPERCASE letters and underscores only",
          " (e.g., SPECIES, SAMPLE_ID, DIET)"
        ),
        shiny$tags$li(
          shiny$tags$strong("Measurement columns: "),
          "Mixed case with numbers",
          " (e.g., Asfc, epLsar, Sq1, HAsfc9)"
        )
      ),

      shiny$tags$hr(),
      shiny$tags$h5("Ambiguous Columns Found:"),
      shiny$tags$p(
        class = "text-warning",
        paste(
          validation_result$ambiguous_cols,
          collapse = ", "
        )
      ),
      shiny$tags$p(
        shiny$tags$em(
          "These columns match the uppercase pattern",
          " but contain numbers. They will be treated",
          " as descriptive columns if they have fewer",
          " than 20 unique values."
        )
      ),

      if (length(validation_result$descriptive_cols) > 0) {
        shiny$tags$div(
          shiny$tags$h5("Detected Descriptive Columns:"),
          shiny$tags$p(
            class = "text-success",
            paste(
              validation_result$descriptive_cols,
              collapse = ", "
            )
          )
        )
      },

      if (length(validation_result$measurement_cols) > 0) {
        shiny$tags$div(
          shiny$tags$h5("Detected Measurement Columns:"),
          shiny$tags$p(
            class = "text-info",
            paste(
              validation_result$measurement_cols,
              collapse = ", "
            )
          )
        )
      }
    )
  )
}
