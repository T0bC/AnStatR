UI_load_data <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h4("Load Data"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
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
        shiny::actionButton(ns("load_btn"), "Load", class = "btn-primary")
      ),
      shiny::mainPanel(
        shiny::p("TODO: Display loaded data here.")
      )
    )
  )
}
