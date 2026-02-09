box::use(
  bsicons,
  bslib,
  shiny,
)

box::use(
  app/logic/error_handling,
  app/view/components/sidebar_tabs,
  app/view/error_display,
  app/view/plotting/data_selection,
  app/view/plotting/filter,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  sidebar_tabs$tab_layout(
    ns = ns,
    sidebar_id = "sidebar_tabs",
    tabs = list(
      data_selection$tab_ui(ns),
      filter$tab_ui(ns)
    ),
    main_content = shiny$uiOutput(ns("main_content"))
  )
}

#' @export
server <- function(id, input_data, data_version) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    last_error <- shiny$reactiveVal(NULL)
    result <- shiny$reactiveVal(NULL)

    # Reset state when new data is loaded
    shiny$observeEvent(data_version(), {
      result(NULL)
      last_error(NULL)
    }, ignoreInit = TRUE)

    # Delegate to sub-module servers
    data_selection$tab_server(
      input, output, session, input_data, data_version
    )
    filter_result <- filter$tab_server(
      input, output, session, input_data, data_version
    )

    # Main content: placeholder, error, or results
    output$main_content <- shiny$renderUI({
      err <- last_error()
      if (error_handling$is_app_error(err)) {
        error_display$error_alert_structured(err, type = "danger")
      } else if (is.null(result())) {
        shiny$tags$div(
          class = "d-flex align-items-center justify-content-center",
          style = "min-height: 400px;",
          shiny$tags$div(
            class = "text-center text-muted",
            shiny$tags$h4("Plotting"),
            shiny$tags$p(
              "Select descriptive and measurement",
              " columns to get started."
            )
          )
        )
      } else {
        bslib$accordion(
          id = ns("results_accordion"),
          open = "result_panel_1",
          multiple = TRUE,
          bslib$accordion_panel(
            title = "Results",
            value = "result_panel_1",
            icon = bsicons$bs_icon("table"),
            shiny$tags$p("Result content goes here.")
          )
        )
      }
    })

    # Return for downstream modules (or invisible(NULL) if none)
    invisible(NULL)
  })
}
