box::use(
  bsicons,
  bslib,
  ggiraph,
  ggplot2,
  rhino,
  shiny,
)

box::use(
  app/logic/cluster,
  app/logic/error_handling,
  app/logic/pca/na_handling[clean_na_rows],
  app/logic/pca/scaling[scale_data],
  app/view/cluster/cluster_results,
  app/view/cluster/clustering_settings,
  app/view/cluster/data_selection,
  app/view/cluster/display_options,
  app/view/cluster/hopkins,
  app/view/cluster/optimal_clusters,
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
    optimal_result <- shiny$reactiveVal(NULL)
    last_optimal_plot <- shiny$reactiveVal(NULL)
    na_info <- shiny$reactiveVal(NULL)
    user_modified_k <- shiny$reactiveVal(FALSE)
    updating_k_programmatically <- shiny$reactiveVal(FALSE)

    # Reset state when new data is loaded
    shiny$observeEvent(data_version(), {
      result(NULL)
      last_error(NULL)
      hopkins_result(NULL)
      optimal_result(NULL)
      last_optimal_plot(NULL)
      na_info(NULL)
      user_modified_k(FALSE)
      updating_k_programmatically(TRUE)
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

    # Track user vs programmatic changes to n_clusters
    shiny$observeEvent(input$n_clusters, {
      if (updating_k_programmatically()) {
        updating_k_programmatically(FALSE)
      } else {
        user_modified_k(TRUE)
      }
    }, ignoreInit = TRUE)

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
      optimal_result(NULL)

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

      # Compute optimal number of clusters
      rhino$log$info(
        "Cluster: computing optimal clusters",
        " ({length(measure_cols)} columns,",
        " {nrow(analysis_data)} samples)"
      )
      opt_res <- cluster$compute_optimal_clusters(
        analysis_data, measure_cols
      )
      optimal_result(opt_res)

      # Auto-fill n_clusters from optimal median if user
      # hasn't manually changed the value
      if (
        isTRUE(opt_res$success) &&
        !user_modified_k()
      ) {
        median_k <- opt_res$result$summary$median_k
        updating_k_programmatically(TRUE)
        shiny$updateNumericInput(
          session, "n_clusters",
          value = median_k
        )
        n_clusters <- median_k
        rhino$log$info(
          "Cluster: auto-set n_clusters={median_k}",
          " from optimal median"
        )
      }

      # Run clustering analysis on prepared data
      cluster_method <- input$cluster_method
      clustering_result <- cluster$run_clustering(
        analysis_data, measure_cols, n_clusters,
        algorithm = algorithm,
        metric = cluster_metric,
        method = cluster_method
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

      # Hopkins clusterability panel
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

      # Optimal clusters panel
      opt_res <- optimal_result()
      opt_content <- if (
        !is.null(opt_res) && !opt_res$success
      ) {
        error_display$error_alert_structured(
          opt_res$error, type = "danger"
        )
      } else if (!is.null(opt_res)) {
        optimal_clusters$render_optimal_clusters(
          opt_res$result, ns
        )
      } else {
        NULL
      }

      opt_panel <- if (!is.null(opt_content)) {
        opt_title <- if (
          !is.null(opt_res) &&
          isTRUE(opt_res$success) &&
          !is.null(opt_res$result$summary$median_k)
        ) {
          shiny$tags$span(
            bsicons$bs_icon(
              "sliders", class = "me-1"
            ),
            "Optimal Number of Clusters",
            shiny$tags$span(
              class = "mx-1", "\u2014"
            ),
            shiny$tags$span(
              class = "badge bg-primary",
              opt_res$result$summary$median_k
            )
          )
        } else {
          shiny$tags$span(
            bsicons$bs_icon(
              "sliders", class = "me-1"
            ),
            "Optimal Number of Clusters"
          )
        }
        bslib$accordion_panel(
          title = opt_title,
          value = "optimal_panel",
          opt_content,
          download_buttons(ns, "optimal")
        )
      }

      # Cluster results panel
      res <- result()
      cluster_results_panel <- if (!is.null(res)) {
        algo_label <- switch(
          res$details$variant,
          kmeans = "K-Means",
          pam    = "K-Means (PAM)",
          hclust = "Hierarchical",
          dbscan = "DBSCAN",
          res$algorithm
        )
        results_title <- shiny$tags$span(
          bsicons$bs_icon(
            "pie-chart", class = "me-1"
          ),
          "Cluster Results",
          shiny$tags$span(
            class = "mx-1", "\u2014"
          ),
          shiny$tags$span(
            class = "badge bg-success me-1",
            algo_label
          ),
          shiny$tags$span(
            class = "badge bg-primary",
            paste0("k=", res$n_clusters)
          )
        )
        bslib$accordion_panel(
          title = results_title,
          value = "cluster_results",
          cluster_results$render_cluster_results(res)
        )
      }

      shiny$tagList(
        na_banner,
        bslib$accordion(
          id = ns("results_accordion"),
          open = "hopkins_panel",
          multiple = TRUE,
          hopkins_panel,
          opt_panel,
          cluster_results_panel
        )
      )
    })

    # Render optimal clusters plot
    output$optimal_clusters_plot <- ggiraph$renderGirafe({
      opt_res <- optimal_result()
      if (is.null(opt_res)) return(NULL)
      if (!opt_res$success) return(NULL)
      last_optimal_plot(
        cluster$create_optimal_clusters_ggplot(
          opt_res$result
        )
      )
      optimal_clusters$render_optimal_girafe(
        opt_res$result
      )
    })

    # Register plot download handlers
    register_plot_downloads(
      output, input, "optimal",
      last_optimal_plot, "Optimal_Clusters"
    )

    # Return for downstream modules (or invisible(NULL) if none)
    invisible(NULL)
  })
}

#' Create SVG + PNG download buttons for an accordion panel
#'
#' @param ns Namespace function
#' @param id_prefix Character, e.g. "optimal", "biplot"
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
#' @param id_prefix Character, e.g. "optimal", "biplot"
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
