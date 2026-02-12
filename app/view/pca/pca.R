box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
)

box::use(
  app/logic/error_handling,
  app/logic/pca/pca[validate_inputs, run_analysis],
  app/view/components/sidebar_tabs,
  app/view/error_display,
  app/view/pca/actions,
  app/view/pca/data_selection,
  app/view/pca/plotting_controls,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  sidebar_tabs$tab_layout(
    ns = ns,
    sidebar_id = "sidebar_tabs",
    tabs = list(
      data_selection$tab_ui(ns),
      plotting_controls$tab_ui(ns),
      actions$tab_ui(ns)
    ),
    main_content = shiny$uiOutput(ns("main_content")),
    action_button = shiny$tagList(
      shiny$actionButton(
        inputId = ns("compute_pca_button"),
        label = "Compute PCA",
        class = "btn-primary btn-sm w-100",
        icon = bsicons$bs_icon("calculator")
      )
    )
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
      rhino$log$info("PCA: state reset for new data")
    }, ignoreInit = TRUE)

    # Delegate to sub-module servers
    data_selection$tab_server(
      input, output, session,
      input_data = input_data,
      data_version = data_version
    )

    # Main content: placeholder, error, or results
    output$main_content <- shiny$renderUI({
      err <- last_error()
      if (error_handling$is_app_error(err)) {
        return(
          error_display$error_alert_structured(
            err, type = "danger"
          )
        )
      }

      if (is.null(result())) {
        return(
          bslib$card(
            bslib$card_header("PCA Results"),
            bslib$card_body(
              class = paste(
                "d-flex align-items-center",
                "justify-content-center"
              ),
              style = "min-height: 300px;",
              shiny$tags$div(
                class = "text-center text-muted",
                shiny$tags$p(
                  bsicons$bs_icon(
                    "bar-chart-steps",
                    size = "3em",
                    class = "mb-3"
                  )
                ),
                shiny$tags$p(
                  "Configure options in the sidebar",
                  " and click ",
                  shiny$tags$strong("Compute PCA"),
                  " to run the analysis."
                )
              )
            )
          )
        )
      }

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
    })

    # Return for downstream modules
    invisible(NULL)
  })
}
