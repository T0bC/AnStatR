box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
)

box::use(
  app/view/cluster/clustering_settings,
  app/view/cluster/data_selection,
  app/view/cluster/display_options,
  app/logic/cluster,
  app/logic/error_handling,
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
      data_selection$tab_ui(ns),
      clustering_settings$tab_ui(ns),
      display_options$tab_ui(ns)
    ),
    main_content = shiny$uiOutput(ns("main_content")),
    action_button = shiny$actionButton(
      inputId = ns("run_clustering"),
      label = "Run Clustering",
      class = "btn-primary btn-sm w-100",
      icon = bsicons$bs_icon("pie-chart")
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
      rhino$log$info("Cluster: state reset for new data")
    }, ignoreInit = TRUE)

    # Delegate to sub-module servers
    data_selection$tab_server(
      input, output, session,
      input_data = input_data,
      data_version = data_version
    )

    clustering_settings$tab_server(
      input, output, session,
      input_data = input_data,
      data_version = data_version
    )

    display_options$tab_server(
      input, output, session,
      input_data = input_data,
      data_version = data_version
    )

    # Handle Run Clustering button
    shiny$observeEvent(input$run_clustering, {
      last_error(NULL)
      result(NULL)

      data <- input_data()
      selected_columns <- input$measureVar
      n_clusters <- input$n_clusters
      algorithm <- input$algorithm

      # Validate inputs
      validation <- cluster$validate_inputs(selected_columns, data)
      if (!validation$valid) {
        last_error(validation$error)
        return()
      }

      # Run clustering analysis
      clustering_result <- cluster$run_clustering(
        data, selected_columns, n_clusters, algorithm
      )

      if (clustering_result$success) {
        result(clustering_result$result)
        rhino$log$info(
          "Cluster: clustering completed successfully"
        )
      } else {
        last_error(clustering_result$error)
      }
    })

    # Main content: placeholder, error, or results
    output$main_content <- shiny$renderUI({
      err <- last_error()
      if (error_handling$is_app_error(err)) {
        return(error_display$error_alert_structured(err, type = "danger"))
      }

      if (is.null(result())) {
        return(
          shiny$tags$div(
            class = "d-flex align-items-center justify-content-center",
            style = "min-height: 400px;",
            shiny$tags$div(
              class = "text-center text-muted",
              shiny$tags$h4("Cluster Analysis"),
              shiny$tags$p(
                "Configure options and run the clustering analysis."
              )
            )
          )
        )
      }

      # Results placeholder
      bslib$accordion(
        id = ns("results_accordion"),
        open = "cluster_results",
        multiple = TRUE,
        bslib$accordion_panel(
          title = shiny$tags$span(
            bsicons$bs_icon("pie-chart", class = "me-1"),
            "Cluster Results"
          ),
          value = "cluster_results",
          shiny$tags$div(
            class = "text-center p-4",
            shiny$tags$p("Cluster analysis results will be displayed here."),
            shiny$tags$div(
              class = "row g-2",
              shiny$tags$div(
                class = "col-md-6",
                shiny$tags$p(
                  "Algorithm: ", shiny$tags$code(result()$algorithm),
                  ", Clusters: ", shiny$tags$code(result()$n_clusters)
                )
              ),
              shiny$tags$div(
                class = "col-md-6",
                shiny$tags$p(
                  "Metric: ", shiny$tags$code(input$cluster_metric),
                  ", Method: ", shiny$tags$code(input$cluster_method)
                )
              )
            ),
            shiny$tags$p(
              "Data points clustered: ", shiny$tags$code(nrow(result()$data))
            )
          )
        )
      )
    })

    # Return for downstream modules (or invisible(NULL) if none)
    invisible(NULL)
  })
}
