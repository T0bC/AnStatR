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
    help_modal$server("help", active_page = shiny$reactive(input$active_page))
    settings_modal$server("settings")

    # --- Tab visibility: tiered prerequisites ---

    # Track whether the median tab has been visited at least once
    median_tab_activated <- shiny$reactiveVal(FALSE)

    # Reset when new data is loaded
    shiny$observeEvent(load_data_result$version(), {
      median_tab_activated(FALSE)
    }, ignoreInit = TRUE)

    # Mark median as activated when user visits it
    shiny$observeEvent(input$active_page, {
      if (identical(input$active_page, "median")) {
        median_tab_activated(TRUE)
      }
    })

    # Tier 1: Median — visible when data is loaded
    # Tier 2: Plotting, Summary — visible when data loaded AND median visited
    shiny$observe({
      has_data <- !is.null(load_data_result$data())
      median_visited <- median_tab_activated()

      # Median: show when data exists
      toggle_median <- if (has_data) bslib$nav_show else bslib$nav_hide
      toggle_median("active_page", target = "median")

      # Downstream tabs: require data + median visited
      downstream_tabs <- c("plotting", "summary")
      toggle_downstream <- if (has_data && median_visited) {
        bslib$nav_show
      } else {
        bslib$nav_hide
      }
      for (tab in downstream_tabs) {
        toggle_downstream("active_page", target = tab)
      }
    })

    # --- Summary tab: disable when plotting has no selections ---
    shiny$observe({
      measures <- plotting_result$measure_cols()
      x_axis <- plotting_result$x_axis()

      has_selections <- length(measures) > 0 && length(x_axis) > 0

      session$sendCustomMessage("tab_disabled_state", list(
        tab     = "summary",
        enabled = has_selections,
        reason  = paste(
          "Select measurement and X-axis columns in the",
          "<strong>Plotting</strong> tab first to unlock",
          "Summary Statistics."
        )
      ))
    })
  })
}
