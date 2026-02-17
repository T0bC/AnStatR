box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
)

box::use(
  app/logic/cluster,
  app/logic/error_handling,
  app/logic/pca/na_handling[clean_na_rows],
  app/logic/pca/scaling[scale_data],
  app/view/cluster/clustering_settings,
  app/view/cluster/data_selection,
  app/view/cluster/display_options,
  app/view/cluster/hopkins,
  app/view/components/sidebar_tabs,
  app/view/error_display,
  app/view/pca/na_summary,
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
    hopkins_result <- shiny$reactiveVal(NULL)
    na_info <- shiny$reactiveVal(NULL)

    # Reset state when new data is loaded
    shiny$observeEvent(data_version(), {
      result(NULL)
      last_error(NULL)
      hopkins_result(NULL)
      na_info(NULL)
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

    # Delegate Hopkins statistic rendering
    hopkins$render_output(
      input, output, session,
      hopkins_result = hopkins_result
    )

    # Handle Run Clustering button
    shiny$observeEvent(input$run_clustering, {
      last_error(NULL)
      result(NULL)
      hopkins_result(NULL)

      data <- input_data()
      measure_cols <- input$measureVar
      n_clusters <- input$n_clusters
      algorithm <- input$algorithm
      cluster_metric <- input$cluster_metric
      scale_method <- input$scale_method

      # Validate inputs
      validation <- cluster$validate_inputs(measure_cols, data)
      if (!validation$valid) {
        last_error(validation$error)
        return()
      }

      # Clean NAs in measurement columns (following PCA pattern)
      meta_cols <- input$metaData
      if (is.null(meta_cols)) meta_cols <- character(0)
      
      rhino$log$info(
        "Cluster: cleaning NA rows",
        " ({length(measure_cols)} measurement columns)"
      )
      na_result <- clean_na_rows(
        data, measure_cols, meta_cols
      )
      na_info(na_result)
      cleaned_data <- na_result$data

      if (nrow(cleaned_data) < 2) {
        last_error(error_handling$simple_error(
          message = paste(
            "After removing rows with missing values,",
            "fewer than 2 rows remain.",
            "Consider deselecting columns with many NAs."
          ),
          operation_name = "Cluster Data Preparation",
          context = list(
            rows_before = na_result$rows_before,
            rows_removed = na_result$rows_removed,
            rows_after = na_result$rows_after
          )
        ))
        return()
      }

      # Scale data based on user selection (following PCA pattern)
      analysis_data <- cleaned_data
      if (!is.null(scale_method) && scale_method != "none") {
        do_center <- scale_method %in%
          c("scale_center", "center_only")
        do_scale <- scale_method == "scale_center"
        
        rhino$log$info(
          "Cluster: scaling data",
          " (center={do_center}, scale={do_scale})"
        )
        scale_res <- scale_data(
          cleaned_data, measure_cols,
          center = do_center, scale = do_scale
        )
        if (!scale_res$success) {
          last_error(scale_res$error)
          return()
        }
        analysis_data <- scale_res$result
      }

      # Compute Hopkins statistic on prepared data
      rhino$log$info(
        "Cluster: computing Hopkins statistic",
        " ({length(measure_cols)} columns,",
        " {nrow(analysis_data)} samples)"
      )
      h_res <- cluster$compute_hopkins(
        analysis_data, measure_cols
      )
      hopkins_result(h_res)

      if (!h_res$success) {
        last_error(h_res$error)
        return()
      }

      # Run clustering analysis on prepared data
      clustering_result <- cluster$run_clustering(
        analysis_data, measure_cols, n_clusters, algorithm
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

      # NA summary banner
      na_res <- na_info()
      na_banner <- if (!is.null(na_res)) {
        na_summary$render_na_summary(na_res)
      }

      # Hopkins clusterability panel content
      h_res <- hopkins_result()
      hopkins_panel <- if (!is.null(h_res)) {
        hopkins_title <- if (isTRUE(h_res$success)) {
          interp <- h_res$result$interpretation
          badge_class <- switch(
            interp$level,
            success = "bg-success",
            warning = "bg-warning text-dark",
            danger  = "bg-danger",
            "bg-secondary"
          )
          shiny$tags$span(
            bsicons$bs_icon(
              "clipboard-data", class = "me-1"
            ),
            "Clusterability (Hopkins)",
            shiny$tags$span(
              class = "mx-1", "\u2014"
            ),
            shiny$tags$span(
              class = paste("badge", badge_class),
              sprintf("%.4f", h_res$result$H)
            ),
            shiny$tags$small(
              class = "text-muted ms-1",
              interp$label
            )
          )
        } else {
          shiny$tags$span(
            bsicons$bs_icon(
              "clipboard-data", class = "me-1"
            ),
            "Clusterability (Hopkins)"
          )
        }
        bslib$accordion_panel(
          title = hopkins_title,
          value = "hopkins_panel",
          shiny$uiOutput(ns("hopkins_panel"))
        )
      }

      # Results placeholder
      shiny$tagList(
        na_banner,
        bslib$accordion(
          id = ns("results_accordion"),
          open = "hopkins_panel",
          multiple = TRUE,
          hopkins_panel,
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
      )
    })

    # Return for downstream modules (or invisible(NULL) if none)
    invisible(NULL)
  })
}
