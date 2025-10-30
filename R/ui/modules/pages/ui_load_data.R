UI_load_data <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h4("Load Data"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
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
        shinyBS::bsCollapse(
          id = ns("csv_settings_collapse"),
          open = NULL,
          shinyBS::bsCollapsePanel(
            title = "CSV settings",
            value = ns("csv_settings_panel"),
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
      shiny::mainPanel(
        shiny::uiOutput(ns("main_content"))
      )
    )
  )
}
