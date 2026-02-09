box::use(
  bsicons,
  bslib,
  shiny,
)

box::use(
  app/logic/settings,
  app/view/help_modal,
  app/view/load_data,
  app/view/median,
  app/view/plotting,
  app/view/settings_modal,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  bslib$page_navbar(
    id = ns("active_page"),
    title = "TexAn 2.0",
    theme = settings$get_default_theme(),
    header = help_modal$panel(ns("help")),
    bslib$nav_panel(
      title = shiny$tagList(
        bsicons$bs_icon("file-earmark-arrow-up"), "Load Data"
      ),
      value = "load_data",
      load_data$ui(ns("load_data"))
    ),
    bslib$nav_panel(
      title = shiny$tagList(
        bsicons$bs_icon("calculator"), "Median"
      ),
      value = "median",
      median$ui(ns("median"))
    ),
    bslib$nav_panel(
      title = shiny$tagList(
        bsicons$bs_icon("graph-up"), "Plotting"
      ),
      value = "plotting",
      plotting$ui(ns("plotting"))
    ),
    bslib$nav_spacer(),
    bslib$nav_item(
      help_modal$ui(ns("help"))
    ),
    bslib$nav_item(
      settings_modal$ui(ns("settings"))
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    load_data_result <- load_data$server("load_data")
    median_result <- median$server(
      "median",
      input_data = load_data_result$data,
      data_version = load_data_result$version
    )
    # Plotting receives median results if available, otherwise the original data
    plotting_data <- shiny$reactive({
      median_result() %||% load_data_result$data()
    })
    plotting$server(
      "plotting",
      input_data = plotting_data,
      data_version = load_data_result$version
    )
    help_modal$server("help", active_page = shiny$reactive(input$active_page))
    settings_modal$server("settings")

    # Tabs that should only be visible once data is loaded.
    # Add future tab values here to auto-hide them until data exists.
    data_dependent_tabs <- c("median", "plotting")

    shiny$observe({
      has_data <- !is.null(load_data_result$data())
      toggle <- if (has_data) bslib$nav_show else bslib$nav_hide
      for (tab in data_dependent_tabs) {
        toggle("active_page", target = tab)
      }
    })
  })
}
