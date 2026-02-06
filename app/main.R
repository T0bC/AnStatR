box::use(
  bsicons,
  bslib,
  shiny,
)

box::use(
  app/logic/settings,
  app/view/load_data,
  app/view/settings_modal,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  bslib$page_navbar(
    id = ns("active_page"),
    title = "TexAn 2.0",
    theme = settings$get_default_theme(),
    bslib$nav_panel(
      title = shiny$tagList(
        bsicons$bs_icon("file-earmark-arrow-up"), "Load Data"
      ),
      value = "load_data",
      load_data$ui(ns("load_data"))
    ),
    bslib$nav_spacer(),
    bslib$nav_item(
      settings_modal$ui(ns("settings"))
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    load_data_result <- load_data$server("load_data")
    settings_modal$server("settings")
  })
}
