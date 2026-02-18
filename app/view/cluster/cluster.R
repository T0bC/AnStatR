box::use(
  bsicons,
  bslib,
  DT,
  ggiraph,
  ggplot2,
  openxlsx,
  plotly,
  rhino,
  shinycssloaders,
  shiny,
)

box::use(
  app/logic/cluster,
  app/logic/error_handling,
  app/logic/pca/na_handling[clean_na_rows],
  app/logic/pca/scaling[scale_data],
  app/view/cluster/cluster_biplot,
  app/view/cluster/cluster_results,
  app/view/cluster/clustering_settings,
  app/view/cluster/data_selection,
  app/view/cluster/heatmap,
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
    membership_data <- shiny$reactiveVal(NULL)
    cluster_summary <- shiny$reactiveVal(NULL)
    hopkins_result <- shiny$reactiveVal(NULL)
    optimal_result <- shiny$reactiveVal(NULL)
    last_optimal_plot <- shiny$reactiveVal(NULL)
    analysis_data_store <- shiny$reactiveVal(NULL)
    cleaned_data_store <- shiny$reactiveVal(NULL)
    measure_cols_store <- shiny$reactiveVal(NULL)
    na_info <- shiny$reactiveVal(NULL)
    user_modified_k <- shiny$reactiveVal(FALSE)
    updating_k_programmatically <- shiny$reactiveVal(FALSE)

    # Reset state when new data is loaded
    shiny$observeEvent(data_version(), {
      result(NULL)
      membership_data(NULL)
      cluster_summary(NULL)
      last_error(NULL)
      hopkins_result(NULL)
      optimal_result(NULL)
      last_optimal_plot(NULL)
      analysis_data_store(NULL)
      cleaned_data_store(NULL)
      measure_cols_store(NULL)
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

      shiny$withProgress(
        message = "Running Cluster Analysis",
        value = 0, {

        # Step 1: Clean NAs
        shiny$incProgress(
          0.05,
          detail = "Cleaning missing values..."
        )
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
        cleaned_data_store(cleaned_data)

        if (nrow(cleaned_data) < 2) {
          last_error(error_handling$simple_error(
            message = paste(
              "After removing rows with missing",
              "values, fewer than 2 rows remain.",
              "Consider deselecting columns",
              "with many NAs."
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

        # Step 2: Scale data
        shiny$incProgress(
          0.10,
          detail = "Scaling data..."
        )
        analysis_data <- cleaned_data
        if (!is.null(scale_method) &&
            scale_method != "none") {
          do_center <- scale_method %in%
            c("scale_center", "center_only")
          do_scale <- scale_method == "scale_center"

          rhino$log$info(
            "Cluster: scaling data",
            " (center={do_center},",
            " scale={do_scale})"
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

        # Step 3: Hopkins statistic
        shiny$incProgress(
          0.10,
          detail = "Computing Hopkins statistic..."
        )
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

        # Step 4: Optimal number of clusters
        # This step involves bootstrapping and can
        # take a long time. Show a persistent
        # notification so the user sees activity
        # even while the progress bar is frozen.
        shiny$incProgress(
          0.15,
          detail = paste(
            "Computing optimal number",
            "of clusters (bootstrapping,",
            "this may take a moment)..."
          )
        )
        opt_note_id <- shiny$showNotification(
          shiny$tagList(
            shiny$tags$div(
              class = paste(
                "d-flex align-items-center",
                "gap-2"
              ),
              shiny$tags$div(
                class = paste(
                  "spinner-border",
                  "spinner-border-sm",
                  "text-primary"
                ),
                role = "status"
              ),
              shiny$tags$span(
                paste(
                  "Computing optimal clusters",
                  "(bootstrapping",
                  nrow(analysis_data),
                  "samples)...",
                  "This may take a moment."
                )
              )
            )
          ),
          duration = NULL,
          closeButton = FALSE,
          type = "message"
        )
        rhino$log$info(
          "Cluster: computing optimal clusters",
          " ({length(measure_cols)} columns,",
          " {nrow(analysis_data)} samples)"
        )
        opt_res <- cluster$compute_optimal_clusters(
          analysis_data, measure_cols
        )
        shiny$removeNotification(opt_note_id)
        optimal_result(opt_res)

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
            "Cluster: auto-set",
            " n_clusters={median_k}",
            " from optimal median"
          )
        }

        # Step 5: Run clustering
        shiny$incProgress(
          0.30,
          detail = "Running clustering algorithm..."
        )
        cluster_method <- input$cluster_method
        clustering_result <- cluster$run_clustering(
          analysis_data, measure_cols, n_clusters,
          algorithm = algorithm,
          metric = cluster_metric,
          method = cluster_method
        )

        if (clustering_result$success) {
          # Step 6: Build results
          shiny$incProgress(
            0.20,
            detail = "Building results..."
          )
          result(clustering_result$result)
          analysis_data_store(analysis_data)
          measure_cols_store(measure_cols)

          keep_cols <- c(meta_cols, measure_cols)
          md <- cleaned_data[
            , keep_cols, drop = FALSE
          ]
          md$Cluster <- clustering_result$result$clusters
          membership_data(md)

          raw_numeric <- as.matrix(
            cleaned_data[
              , measure_cols, drop = FALSE
            ]
          )
          cluster_summary(
            cluster$compute_cluster_summary(
              raw_numeric,
              clustering_result$result$clusters
            )
          )

          # Update biplot dimension choices
          n_dims <- min(
            length(measure_cols),
            nrow(analysis_data) - 1
          )
          dim_choices <- paste0(
            "Dim.", seq_len(n_dims)
          )
          for (dim_id in c(
            "clusterBiplotDimX",
            "clusterBiplotDimY"
          )) {
            current <- input[[dim_id]]
            sel <- if (!is.null(current) &&
                       current %in% dim_choices) {
              current
            } else {
              dim_choices[min(
                which(dim_id == c(
                  "clusterBiplotDimX",
                  "clusterBiplotDimY"
                )),
                length(dim_choices)
              )]
            }
            shiny$updateSelectizeInput(
              session, dim_id,
              choices = dim_choices,
              selected = sel
            )
          }

          shiny$incProgress(
            0.10,
            detail = "Done!"
          )
          rhino$log$info(
            "Cluster: completed successfully"
          )
        } else {
          last_error(clustering_result$error)
        }
      }) # end withProgress
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
          cluster_results$render_cluster_results(
            res, ns,
            cluster_summary = cluster_summary()
          )
        )
      }

      # Cluster heatmap panel
      heatmap_panel <- if (!is.null(res)) {
        heatmap_title <- shiny$tags$span(
          bsicons$bs_icon(
            "grid-3x3-gap", class = "me-1"
          ),
          "Cluster Heatmap"
        )
        heatmap_content <- heatmap$render_heatmap_content(
          res, ns
        )
        bslib$accordion_panel(
          title = heatmap_title,
          value = "heatmap_panel",
          heatmap_content,
          heatmap_download_button(ns)
        )
      }

      # Cluster biplot panel
      biplot_err <- biplot_state$error()
      biplot_panel <- if (!is.null(res)) {
        biplot_title <- shiny$tags$span(
          bsicons$bs_icon(
            "diagram-2", class = "me-1"
          ),
          "Cluster Biplot"
        )
        biplot_content <- if (
          error_handling$is_app_error(biplot_err)
        ) {
          error_display$error_alert_structured(
            biplot_err, type = "danger"
          )
        } else {
          cluster_biplot$render_biplot_content(
            res, ns
          )
        }
        bslib$accordion_panel(
          title = biplot_title,
          value = "cluster_biplot_panel",
          biplot_content,
          download_buttons(ns, "cluster_biplot")
        )
      }

      shiny$tagList(
        na_banner,
        bslib$accordion(
          id = ns("results_accordion"),
          open = "cluster_biplot_panel",
          multiple = TRUE,
          hopkins_panel,
          opt_panel,
          biplot_panel,
          cluster_results_panel,
          heatmap_panel
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

    # Delegate heatmap rendering
    heatmap$render_output(
      input, output, session,
      cluster_result_rv = result,
      membership_data_rv = membership_data,
      analysis_data_rv = analysis_data_store,
      measure_cols_rv = measure_cols_store
    )

    # Delegate cluster biplot rendering
    biplot_state <- cluster_biplot$render_output(
      input, output, session,
      cluster_result_rv = result,
      membership_data_rv = membership_data,
      analysis_data_rv = analysis_data_store,
      cleaned_data_rv = cleaned_data_store,
      measure_cols_rv = measure_cols_store
    )

    # Register cluster biplot download handlers
    register_plot_downloads(
      output, input, "cluster_biplot",
      biplot_state$plot, "Cluster_Biplot"
    )

    # Helper: build current heatmap from reactive state
    build_heatmap_for_download <- function() {
      res <- result()
      shiny$req(res)

      ad <- analysis_data_store()
      mc <- measure_cols_store()
      shiny$req(ad, mc)

      show_labels <- isTRUE(input$showLabels)
      seriation <- input$seriation %||% "OLO"

      custom_labels <- NULL
      label_col <- input$labelColumn
      if (show_labels && !is.null(label_col) &&
          nzchar(label_col)) {
        md <- membership_data()
        if (!is.null(md) &&
            label_col %in% names(md)) {
          custom_labels <- as.character(
            md[[label_col]]
          )
        }
      }

      row_side_colors_df <- NULL
      side_cols <- input$rowSideColors
      if (!is.null(side_cols) &&
          length(side_cols) > 0) {
        md <- membership_data()
        if (!is.null(md)) {
          valid_cols <- intersect(
            side_cols, names(md)
          )
          if (length(valid_cols) > 0) {
            row_side_colors_df <- md[
              , valid_cols, drop = FALSE
            ]
          }
        }
      }

      cluster$create_cluster_heatmap(
        res,
        data = ad,
        measure_cols = mc,
        show_labels = show_labels,
        custom_labels = custom_labels,
        seriation = seriation,
        row_side_colors_df = row_side_colors_df,
        scale_heatmap = "none"
      )
    }

    # Heatmap PNG download handler (via kaleido)
    output$heatmap_dl_png <- shiny$downloadHandler(
      filename = function() {
        paste0(
          "Cluster_Heatmap_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".png"
        )
      },
      content = function(file) {
        hm_result <- build_heatmap_for_download()
        shiny$req(isTRUE(hm_result$success))
        # Bypass plotly$save_image because its
        # newKaleidoScope passes tempfile paths with
        # Windows backslashes into a Python string,
        # causing a SyntaxError (\U unicode escape).
        # We replicate the kaleido v1 logic here with
        # forward-slash normalised paths.
        kaleido <- reticulate::import("kaleido")
        fig_data <- plotly$plotly_build(
          hm_result$result
        )$x[c("data", "layout", "config")]
        fig_json <- jsonlite::toJSON(
          fig_data, auto_unbox = TRUE, force = TRUE
        )
        tmp_json <- tempfile(fileext = ".json")
        on.exit(unlink(tmp_json), add = TRUE)
        writeLines(fig_json, tmp_json)
        json_path <- gsub("\\\\", "/", tmp_json)
        load_json <- sprintf(
          "import json; fig = json.load(open('%s'))",
          json_path
        )
        reticulate::py_run_string(load_json)
        tmp_png <- tempfile(fileext = ".png")
        on.exit(unlink(tmp_png), add = TRUE)
        png_path <- gsub("\\\\", "/", tmp_png)
        opts <- list(
          format = "png",
          width = reticulate::r_to_py(1200L),
          height = reticulate::r_to_py(1200L),
          scale = reticulate::r_to_py(2L)
        )
        plotlyjs <- gsub(
          "\\\\", "/",
          system.file(
            "htmlwidgets/lib/plotlyjs",
            "plotly-latest.min.js",
            package = "plotly"
          )
        )
        kopts <- list(plotlyjs = plotlyjs)
        kaleido$write_fig_sync(
          reticulate::py$fig, png_path,
          opts = opts, kopts = kopts
        )
        file.copy(tmp_png, file, overwrite = TRUE)
        rhino$log$info(
          "Download: Cluster Heatmap PNG"
        )
      }
    )

    # Heatmap SVG download handler (via kaleido)
    output$heatmap_dl_svg <- shiny$downloadHandler(
      filename = function() {
        paste0(
          "Cluster_Heatmap_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".svg"
        )
      },
      content = function(file) {
        hm_result <- build_heatmap_for_download()
        shiny$req(isTRUE(hm_result$success))
        kaleido <- reticulate::import("kaleido")
        fig_data <- plotly$plotly_build(
          hm_result$result
        )$x[c("data", "layout", "config")]
        fig_json <- jsonlite::toJSON(
          fig_data, auto_unbox = TRUE, force = TRUE
        )
        tmp_json <- tempfile(fileext = ".json")
        on.exit(unlink(tmp_json), add = TRUE)
        writeLines(fig_json, tmp_json)
        json_path <- gsub("\\\\", "/", tmp_json)
        load_json <- sprintf(
          "import json; fig = json.load(open('%s'))",
          json_path
        )
        reticulate::py_run_string(load_json)
        tmp_svg <- tempfile(fileext = ".svg")
        on.exit(unlink(tmp_svg), add = TRUE)
        svg_path <- gsub("\\\\", "/", tmp_svg)
        opts <- list(
          format = "svg",
          width = reticulate::r_to_py(1200L),
          height = reticulate::r_to_py(1200L),
          scale = reticulate::r_to_py(2L)
        )
        plotlyjs <- gsub(
          "\\\\", "/",
          system.file(
            "htmlwidgets/lib/plotlyjs",
            "plotly-latest.min.js",
            package = "plotly"
          )
        )
        kopts <- list(plotlyjs = plotlyjs)
        kaleido$write_fig_sync(
          reticulate::py$fig, svg_path,
          opts = opts, kopts = kopts
        )
        file.copy(tmp_svg, file, overwrite = TRUE)
        rhino$log$info(
          "Download: Cluster Heatmap SVG"
        )
      }
    )

    # Render membership DT table with colored cluster badges
    output$membership_table <- DT$renderDataTable({
      md <- membership_data()
      shiny$req(md)
      cluster_results$render_membership_dt(md)
    })

    # Download handler: Excel with Membership + Profile sheets
    output$cluster_dl_excel <- shiny$downloadHandler(
      filename = function() {
        paste0(
          "cluster_results_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".xlsx"
        )
      },
      content = function(file) {
        md <- membership_data()
        cs <- cluster_summary()
        shiny$req(md, cs)

        # Build profile sheet
        profile_df <- as.data.frame(
          round(cs$means, 4)
        )
        profile_df <- cbind(
          Cluster = cs$cluster_ids,
          n = cs$n_per_cluster,
          profile_df
        )

        wb <- openxlsx$createWorkbook()
        openxlsx$addWorksheet(wb, "Membership")
        openxlsx$writeData(wb, "Membership", md)
        openxlsx$addWorksheet(wb, "Cluster Profile")
        openxlsx$writeData(
          wb, "Cluster Profile", profile_df
        )
        openxlsx$saveWorkbook(wb, file)
        rhino$log$info(
          "Download: Cluster results Excel"
        )
      }
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

#' Create download buttons for the heatmap panel
#'
#' @param ns Namespace function
#' @return tagList with a PNG download button
heatmap_download_button <- function(ns) {
  shiny$tags$div(
    class = "d-flex gap-2 mt-2",
    shiny$downloadButton(
      ns("heatmap_dl_svg"),
      label = shiny$tags$span(
        bsicons$bs_icon(
          "filetype-svg", class = "me-1"
        ),
        "SVG"
      ),
      class = "btn btn-outline-secondary btn-sm"
    ),
    shiny$downloadButton(
      ns("heatmap_dl_png"),
      label = shiny$tags$span(
        bsicons$bs_icon(
          "filetype-png", class = "me-1"
        ),
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
