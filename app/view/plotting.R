box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
)

box::use(
  app/logic/error_handling,
  app/logic/plotting,
  app/view/components/sidebar_tabs,
  app/view/error_display,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  sidebar_tabs$tab_layout(
    ns = ns,
    sidebar_id = "sidebar_tabs",
    tabs = list(
      sidebar_tabs$create_tab(
        icon = "sliders",
        tooltip_text = "Configuration",
        value = "config_tab",
        shiny$h6(class = "text-muted mb-3", "Configuration"),
        shiny$selectizeInput(
          inputId = ns("input1"),
          label = shiny$tags$span(
            "Select columns ",
            bslib$tooltip(
              bsicons$bs_icon("info-circle", class = "text-muted"),
              "Choose columns for plotting."
            )
          ),
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Select...")
        )
      )
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

    # Update input choices when data changes
    shiny$observe({
      data <- input_data()
      if (is.null(data)) return()
      shiny$updateSelectizeInput(
        session, "input1",
        choices = names(data),
        selected = NULL
      )
    })

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
              "Configure options and run the analysis."
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
