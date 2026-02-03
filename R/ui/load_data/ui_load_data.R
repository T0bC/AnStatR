#' @export
UI_load_data <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        title = "Instructions",
        shiny::includeMarkdown("docs/load_data/instructions.md"),
        shiny::fileInput(
          inputId = ns("data_file"),
          label = "Upload dataset",
          multiple = FALSE,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv",
            ".xlsx"
          )
        ),
        shiny::helpText("Accepted formats: CSV or XLSX (single file)."),
        bslib::accordion(
          id = ns("csv_settings_collapse"),
          open = FALSE,
          bslib::accordion_panel(
            title = "CSV settings",
            value = "csv_settings_panel",
            shiny::helpText(
              "The following settings only apply to CSV uploads.",
              "XLSX files ignore these options."
            ),
            shiny::checkboxInput(
              inputId = ns("csv_has_header"),
              label = "CSV includes header row",
              value = TRUE
            ),
            shiny::radioButtons(
              inputId = ns("csv_delimiter"),
              label = "Delimiter",
              choices = c(
                ", (comma)" = ",",
                "; (semicolon)" = ";",
                "Tab" = "\t"
              ),
              selected = ","
            ),
            shiny::radioButtons(
              inputId = ns("csv_quote"),
              label = "Quote character",
              choices = c(
                "None" = "",
                "Double quote (\"\")" = '"',
                "Single quote (')" = "'"
              ),
              selected = '"'
            )
          )
        )
      ),
      shiny::uiOutput(ns("main_content"))
  )
}
