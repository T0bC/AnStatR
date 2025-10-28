UI_load_data <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h4("Load Data"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput(ns("data_file"), "Upload dataset"),
        shiny::actionButton(ns("load_btn"), "Load", class = "btn-primary")
      ),
      shiny::mainPanel(
        shiny::p("TODO: Display loaded data here.")
      )
    )
  )
}
