box::use(
  bslib,
  shiny,
)

box::use(
  app/view/load_data,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  bslib$page_navbar(
    id = ns("active_page"),
    title = "TexAn 2.0",
    bslib$nav_panel(
      title = "Load Data",
      value = "load_data",
      load_data$ui(ns("load_data"))
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    load_data_result <- load_data$server("load_data")
  })
}
