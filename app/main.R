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
  app/view/pca,
  app/view/plotting,
  app/view/settings_modal,
  app/view/statistics,
  app/view/summary,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  bslib$page_navbar(
    id = ns("active_page"),
    title = "TexAn 2.0",
    theme = settings$get_default_theme(),
    header = shiny$tagList(
      shiny$tags$head(
        shiny$tags$script(src = "static/js/disabled_tabs.js"),
        shiny$tags$script(src = "static/js/plot_resize.js")
      ),
      help_modal$panel(ns("help"))
    ),
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
    bslib$nav_panel(
      title = shiny$tagList(
        bsicons$bs_icon("table"), "Summary"
      ),
      value = "summary",
      summary$ui(ns("summary"))
    ),
    bslib$nav_panel(
      title = shiny$tagList(
        bsicons$bs_icon("bar-chart-line"), "Statistics"
      ),
      value = "statistics",
      statistics$ui(ns("statistics"))
    ),
    bslib$nav_panel(
      title = shiny$tagList(
        bsicons$bs_icon("bar-chart-steps"), "PCA"
      ),
      value = "pca",
      pca$ui(ns("pca"))
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
    plotting_result <- plotting$server(
      "plotting",
      input_data = plotting_data,
      data_version = load_data_result$version
    )
    summary$server(
      "summary",
      input_data = plotting_data,
      data_version = load_data_result$version,
      plotting_x_axis = plotting_result$x_axis,
      plotting_measures = plotting_result$measure_cols
    )
    statistics$server(
      "statistics",
      input_data = plotting_data,
      data_version = load_data_result$version
    )
    pca$server(
      "pca",
      input_data = plotting_data,
      data_version = load_data_result$version
    )
    help_modal$server("help", active_page = shiny$reactive(input$active_page))
    settings_modal$server("settings")

    # --- Tab visibility ---
    # All tabs except Load Data are hidden on startup.
    # Median + Plotting: visible once data is loaded.
    # Summary: visible once the user has selected X-axis columns
    #          in the Plotting tab.

    shiny$observe({
      has_data <- !is.null(load_data_result$data())
      toggle <- if (has_data) bslib$nav_show else bslib$nav_hide
      toggle("active_page", target = "median")
      toggle("active_page", target = "plotting")
      toggle("active_page", target = "statistics")
      toggle("active_page", target = "pca")
    })

    # --- Statistics tab: grayed out with lock until plotting selections exist ---
    shiny$observe({
      measures <- plotting_result$measure_cols()
      x_axis <- plotting_result$x_axis()

      has_selections <- length(measures) > 0 && length(x_axis) > 0

      session$sendCustomMessage("tab_disabled_state", list(
        tab     = "statistics",
        enabled = has_selections,
        reason  = paste(
          "Select measurement and X-axis columns in the",
          "<strong>Plotting</strong> tab first to unlock",
          "the Statistics tab."
        )
      ))
    })

    shiny$observe({
      x_axis <- plotting_result$x_axis()
      has_x <- !is.null(x_axis) && length(x_axis) > 0
      toggle <- if (has_x) bslib$nav_show else bslib$nav_hide
      toggle("active_page", target = "summary")
    })
  })
}
