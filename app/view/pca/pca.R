box::use(
  bsicons,
  bslib,
  ggiraph,
  rhino,
  shiny,
)

box::use(
  app/logic/error_handling,
  app/logic/pca/correlation_plot[compute_correlation_data],
  app/logic/pca/kmo[calculate_kmo],
  app/logic/pca/na_handling[clean_na_rows],
  app/logic/pca/optimal_components[calculate_optimal_components],
  app/logic/pca/pca[validate_inputs, run_pca],
  app/logic/pca/pca_export[create_pca_excel],
  app/logic/pca/scaling[scale_data],
  app/view/components/sidebar_tabs,
  app/view/error_display,
  app/view/pca/actions,
  app/view/pca/correlation_plot[render_output],
  app/view/pca/data_selection,
  app/view/pca/kmo_results,
  app/view/pca/na_summary,
  app/view/pca/optimal_components,
  app/view/pca/pca_results,
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
    correlation_result <- shiny$reactiveVal(NULL)
    kmo_result <- shiny$reactiveVal(NULL)
    optimal_result <- shiny$reactiveVal(NULL)
    pca_result <- shiny$reactiveVal(NULL)
    na_info <- shiny$reactiveVal(NULL)

    # Reset state when new data is loaded
    shiny$observeEvent(data_version(), {
      result(NULL)
      last_error(NULL)
      correlation_result(NULL)
      kmo_result(NULL)
      optimal_result(NULL)
      pca_result(NULL)
      na_info(NULL)
      rhino$log$info("PCA: state reset for new data")
    }, ignoreInit = TRUE)

    # Delegate to sub-module servers
    data_selection$tab_server(
      input, output, session,
      input_data = input_data,
      data_version = data_version
    )

    # Delegate correlation plot rendering
    render_output(
      input, output, session,
      correlation_result = correlation_result
    )

    # Handle Compute PCA button
    shiny$observeEvent(input$compute_pca_button, {
      last_error(NULL)
      result(NULL)
      correlation_result(NULL)
      kmo_result(NULL)
      optimal_result(NULL)
      pca_result(NULL)
      na_info(NULL)

      data <- input_data()
      measure_cols <- input$measureVar

      # Validate inputs
      validation <- validate_inputs(measure_cols, data)
      if (!validation$valid) {
        last_error(validation$error)
        return()
      }

      # Clean NAs in measurement columns
      meta_cols <- input$metaData
      if (is.null(meta_cols)) meta_cols <- character(0)
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
            "Consider deselecting columns with",
            "many NAs."
          ),
          operation_name = "PCA Data Preparation",
          context = list(
            rows_before = na_result$rows_before,
            rows_removed = na_result$rows_removed,
            rows_after = na_result$rows_after
          )
        ))
        return()
      }

      # Scale data based on user selection
      analysis_data <- cleaned_data
      scale_method <- input$scale_method
      if (!is.null(scale_method) && scale_method != "none") {
        do_center <- scale_method %in%
          c("scale_center", "center_only")
        do_scale <- scale_method == "scale_center"
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

      rhino$log$info(
        "PCA: computing correlation plot",
        " ({length(measure_cols)} columns,",
        " {nrow(analysis_data)} rows)"
      )

      # Compute correlation on prepared data
      corr_res <- compute_correlation_data(
        analysis_data, measure_cols
      )
      correlation_result(corr_res)

      if (!corr_res$success) {
        last_error(corr_res$error)
        return()
      }

      # Compute KMO measure on prepared data
      rhino$log$info(
        "PCA: computing KMO measure",
        " ({length(measure_cols)} columns)"
      )
      numeric_subset <- analysis_data[
        , measure_cols, drop = FALSE
      ]
      kmo_res <- calculate_kmo(numeric_subset)
      kmo_result(kmo_res)

      # Compute optimal number of components
      rhino$log$info(
        "PCA: computing optimal components",
        " ({length(measure_cols)} columns)"
      )
      is_scaled <- !is.null(scale_method) &&
        scale_method == "scale_center"
      opt_res <- calculate_optimal_components(
        numeric_subset, scale = is_scaled
      )
      optimal_result(opt_res)

      # Run PCA
      rhino$log$info(
        "PCA: running PCA",
        " ({length(measure_cols)} columns,",
        " {nrow(analysis_data)} rows)"
      )
      pca_res <- run_pca(
        analysis_data, measure_cols,
        meta_cols = meta_cols
      )
      pca_result(pca_res)

      # Mark that we have results to display
      result(TRUE)
    })

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

      # NA summary banner
      na_res <- na_info()
      na_banner <- if (!is.null(na_res)) {
        na_summary$render_na_summary(na_res)
      }

      corr_res <- correlation_result()
      corr_content <- if (
        !is.null(corr_res) && !corr_res$success
      ) {
        error_display$error_alert_structured(
          corr_res$error, type = "danger"
        )
      } else {
        ggiraph$girafeOutput(
          ns("correlation_plot"), height = "500px"
        )
      }

      # KMO panel content
      kmo_res <- kmo_result()
      kmo_content <- if (
        !is.null(kmo_res) && !kmo_res$success
      ) {
        error_display$error_alert_structured(
          kmo_res$error, type = "danger"
        )
      } else if (!is.null(kmo_res)) {
        kmo_results$render_kmo_results(kmo_res$result)
      } else {
        NULL
      }

      kmo_panel <- if (!is.null(kmo_content)) {
        bslib$accordion_panel(
          title = shiny$tags$span(
            bsicons$bs_icon(
              "speedometer2", class = "me-1"
            ),
            "KMO Measure"
          ),
          value = "kmo_panel",
          kmo_content
        )
      }

      # Optimal components panel content
      opt_res <- optimal_result()
      opt_content <- if (
        !is.null(opt_res) && !opt_res$success
      ) {
        error_display$error_alert_structured(
          opt_res$error, type = "danger"
        )
      } else if (!is.null(opt_res)) {
        optimal_components$render_optimal_components(
          opt_res$result, ns
        )
      } else {
        NULL
      }

      opt_panel <- if (!is.null(opt_content)) {
        bslib$accordion_panel(
          title = shiny$tags$span(
            bsicons$bs_icon(
              "sliders", class = "me-1"
            ),
            "Optimal Number of Components"
          ),
          value = "optimal_panel",
          opt_content
        )
      }

      # PCA results panel content
      pca_res <- pca_result()
      pca_content <- if (
        !is.null(pca_res) && !pca_res$success
      ) {
        error_display$error_alert_structured(
          pca_res$error, type = "danger"
        )
      } else if (
        !is.null(pca_res) && pca_res$success
      ) {
        pca_results$render_pca_results(
          pca_res$result, ns
        )
      } else {
        NULL
      }

      pca_panel <- if (!is.null(pca_content)) {
        bslib$accordion_panel(
          title = shiny$tags$span(
            bsicons$bs_icon(
              "bar-chart-line", class = "me-1"
            ),
            "PCA Results"
          ),
          value = "pca_panel",
          pca_content
        )
      }

      shiny$tagList(
        na_banner,
        bslib$accordion(
          id = ns("results_accordion"),
          open = "correlation_panel",
          multiple = TRUE,
          bslib$accordion_panel(
            title = shiny$tags$span(
              bsicons$bs_icon(
                "grid-3x3", class = "me-1"
              ),
              "Correlation Matrix"
            ),
            value = "correlation_panel",
            corr_content
          ),
          kmo_panel,
          opt_panel,
          pca_panel
        )
      )
    })

    # Render optimal components scree plot
    output$optimal_scree_plot <- ggiraph$renderGirafe({
      opt_res <- optimal_result()
      if (is.null(opt_res)) return(NULL)
      if (!opt_res$success) return(NULL)
      optimal_components$render_scree_girafe(
        opt_res$result
      )
    })

    # Download handler: Excel export
    output$download_pca_excel <- shiny$downloadHandler(
      filename = function() {
        paste0(
          "pca_results_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".xlsx"
        )
      },
      content = function(file) {
        pca_res <- pca_result()
        shiny$req(pca_res)
        shiny$req(pca_res$success)
        create_pca_excel(pca_res$result, file)
      }
    )

    # Download handler: RDS export
    output$download_pca_rds <- shiny$downloadHandler(
      filename = function() {
        paste0(
          "pca_object_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".rds"
        )
      },
      content = function(file) {
        pca_res <- pca_result()
        shiny$req(pca_res)
        shiny$req(pca_res$success)
        saveRDS(pca_res$result, file)
      }
    )

    # Return for downstream modules
    invisible(NULL)
  })
}
