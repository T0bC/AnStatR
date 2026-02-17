box::use(
  bsicons,
  bslib,
  ggplot2,
  ggiraph,
  rhino,
  shiny,
)

box::use(
  app/logic/cluster,
  app/logic/error_handling,
  app/view/cluster/clustering_settings,
  app/view/cluster/data_selection,
  app/view/cluster/display_options,
  app/view/cluster/distance_matrix,
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
    distance_result <- shiny$reactiveVal(NULL)

    # Reset state when new data is loaded
    shiny$observeEvent(data_version(), {
      result(NULL)
      last_error(NULL)
      distance_result(NULL)
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

    # Delegate distance matrix rendering
    distance_state <- distance_matrix$render_output(
      input, output, session,
      distance_result = distance_result
    )

    # Handle Run Clustering button
    shiny$observeEvent(input$run_clustering, {
      last_error(NULL)
      result(NULL)
      distance_result(NULL)

      data <- input_data()
      selected_columns <- input$measureVar
      n_clusters <- input$n_clusters
      algorithm <- input$algorithm
      cluster_metric <- input$cluster_metric
      scale_method <- input$scale_method

      # Validate inputs
      validation <- cluster$validate_inputs(selected_columns, data)
      if (!validation$valid) {
        last_error(validation$error)
        return()
      }

      # Compute distance matrix
      rhino$log$info(
        "Cluster: computing distance matrix",
        " ({length(selected_columns)} columns,",
        " {nrow(data)} samples, {cluster_metric} metric)"
      )
      dist_res <- cluster$compute_distance_matrix(
        data, selected_columns, cluster_metric, scale_method
      )
      distance_result(dist_res)

      if (!dist_res$success) {
        last_error(dist_res$error)
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

      # Distance matrix panel content
      dist_res <- distance_result()
      dist_content <- if (
        !is.null(dist_res) && !dist_res$success
      ) {
        error_display$error_alert_structured(
          dist_res$error, type = "danger"
        )
      } else if (
        !is.null(dist_res) && dist_res$success
      ) {
        ggiraph$girafeOutput(
          ns("distance_matrix_plot"), height = "500px"
        )
      }

      dist_panel <- if (!is.null(dist_content)) {
        bslib$accordion_panel(
          title = shiny$tags$span(
            bsicons$bs_icon("grid-3x3", class = "me-1"),
            "Distance Matrix"
          ),
          value = "distance_panel",
          dist_content,
          download_buttons(ns, "distance")
        )
      }

      # Results placeholder
      bslib$accordion(
        id = ns("results_accordion"),
        open = "distance_panel",
        multiple = TRUE,
        dist_panel,
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

    # Register plot download handlers
    register_plot_downloads(
      output, input, "distance",
      distance_state$plot, "Distance_Matrix"
    )

    # Return for downstream modules (or invisible(NULL) if none)
    invisible(NULL)
  })
}

#' Create SVG + PNG download buttons for an accordion panel
#'
#' @param ns Namespace function
#' @param id_prefix Character, e.g. "distance", "biplot"
#' @return tagList with two download buttons
download_buttons <- function(ns, id_prefix) {
  shiny$tags$div(
    class = "d-flex gap-2 mt-2",
    shiny$downloadButton(
      ns(paste0(id_prefix, "_dl_svg")),
      label = shiny$tags$span(
        bsicons$bs_icon("filetype-svg", class = "me-1"),
        "SVG"
      ),
      class = "btn btn-outline-secondary btn-sm"
    ),
    shiny$downloadButton(
      ns(paste0(id_prefix, "_dl_png")),
      label = shiny$tags$span(
        bsicons$bs_icon("filetype-png", class = "me-1"),
        "PNG"
      ),
      class = "btn btn-outline-secondary btn-sm"
    )
  )
}

#' Register SVG and PNG download handlers for a plot
#'
#' @param output Shiny output object
#' @param input Shiny input object
#' @param id_prefix Character, e.g. "distance", "biplot"
#' @param plot_reactive reactiveVal returning a ggplot
#' @param filename_base Character, base name for the file
register_plot_downloads <- function(output, input,
                                    id_prefix,
                                    plot_reactive,
                                    filename_base) {
  output[[paste0(id_prefix, "_dl_svg")]] <- 
    shiny$downloadHandler(
      filename = function() {
        paste0(filename_base, "_", Sys.Date(), ".svg")
      },
      content = function(file) {
        p <- plot_reactive()
        shiny$req(p)
        w <- input$width %||% 16
        h <- input$height %||% 10
        ggplot2$ggsave(
          file, plot = p, device = "svg",
          width = w, height = h, units = "cm"
        )
        rhino$log$info(
          "Download: SVG '{filename_base}'"
        )
      }
    )

  output[[paste0(id_prefix, "_dl_png")]] <- 
    shiny$downloadHandler(
      filename = function() {
        paste0(filename_base, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- plot_reactive()
        shiny$req(p)
        w <- input$width %||% 16
        h <- input$height %||% 10
        ggplot2$ggsave(
          file, plot = p, device = "png",
          width = w, height = h,
          units = "cm", dpi = 600
        )
        rhino$log$info(
          "Download: PNG '{filename_base}'"
        )
      }
    )
}
