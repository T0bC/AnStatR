box::use(
  bsicons,
  bslib,
  ggiraph,
  ggplot2,
  plotly,
  rhino,
  shiny,
)

box::use(
  app/logic/shared/error_handling,
  app/logic/pca/correlation_plot[compute_correlation_data],
  app/logic/pca/kmo[calculate_kmo, kmo_badge_class, kmo_interpretation],
  app/logic/preprocessing/na_handling[clean_na_rows],
  app/logic/pca/optimal_components[calculate_optimal_components],
  app/logic/pca/pca[
    validate_inputs, run_pca, extract_variance_explained
  ],
  app/logic/pca/scaling[residualize_data],
  app/logic/pca/tune_plot[create_tune_spca_plot],
  app/logic/pca/pca_export[create_pca_excel, create_pca_bundle],
  app/logic/preprocessing/skewness_transform[
    detect_skewness, transform_skewed
  ],
  app/view/components/sidebar_tabs,
  app/view/shared/error_display,
  app/view/pca/analysis_settings,
  app/view/pca/biplot,
  app/view/pca/biplot3d,
  app/view/pca/correlation_plot[render_output],
  app/view/pca/eigencorplot,
  app/view/pca/ind_contrib,
  app/view/pca/var_contrib_jitter,
  app/view/pca/data_selection,
  app/view/pca/kmo_results,
  app/view/shared/preprocessing_summary,
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
      analysis_settings$tab_ui(ns),
      plotting_controls$tab_ui(ns)
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
server <- function(id, input_data, data_version,
                   recommended_parameters = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    last_error <- shiny$reactiveVal(NULL)
    result <- shiny$reactiveVal(NULL)
    correlation_result <- shiny$reactiveVal(NULL)
    kmo_result <- shiny$reactiveVal(NULL)
    optimal_result <- shiny$reactiveVal(NULL)
    pca_result <- shiny$reactiveVal(NULL)
    na_info <- shiny$reactiveVal(NULL)
    transform_info <- shiny$reactiveVal(NULL)
    skewness_info <- shiny$reactiveVal(NULL)
    bundle_data <- shiny$reactiveVal(NULL)

    # Reset state when new data is loaded
    shiny$observeEvent(data_version(), {
      result(NULL)
      last_error(NULL)
      correlation_result(NULL)
      kmo_result(NULL)
      optimal_result(NULL)
      pca_result(NULL)
      na_info(NULL)
      transform_info(NULL)
      skewness_info(NULL)
      bundle_data(NULL)
      rhino$log$info("PCA: state reset for new data")
    }, ignoreInit = TRUE)

    # Delegate to sub-module servers
    data_selection$tab_server(
      input, output, session,
      input_data = input_data,
      data_version = data_version,
      recommended_parameters = recommended_parameters
    )

    analysis_settings_state <- analysis_settings$tab_server(
      input, output, session,
      data_version = data_version,
      input_data = input_data
    )

    # Delegate correlation plot rendering
    corr_state <- render_output(
      input, output, session,
      correlation_result = correlation_result
    )

    # Delegate biplot rendering
    biplot_state <- biplot$render_output(
      input, output, session,
      pca_result = pca_result
    )

    # Delegate 3D biplot rendering
    biplot3d_state <- biplot3d$render_output(
      input, output, session,
      pca_result = pca_result
    )

    # Reactive: display_ncp for downstream renderers
    display_ncp <- shiny$reactive({
      compute_display_ncp(
        optimal_result(), pca_result()
      )
    })

    # Delegate variable contribution jitter plot rendering
    var_contrib_jitter_state <- var_contrib_jitter$render_output(
      input, output, session,
      pca_result = pca_result,
      display_ncp = display_ncp
    )

    # Delegate individual contribution plot rendering
    ind_contrib_state <- ind_contrib$render_output(
      input, output, session,
      pca_result = pca_result,
      display_ncp = display_ncp
    )

    # Delegate eigencorrelation plot rendering
    eigencor_state <- eigencorplot$render_output(
      input, output, session,
      pca_result = pca_result,
      display_ncp = display_ncp
    )

    # Register plot download handlers
    register_plot_downloads(
      output, input, "corr",
      corr_state$plot, "Correlation_Matrix"
    )
    register_plot_downloads(
      output, input, "biplot",
      biplot_state$plot, "Biplot"
    )
    register_plot_downloads(
      output, input, "var_contrib",
      var_contrib_jitter_state$plot, "Variable_Contributions"
    )
    register_plot_downloads(
      output, input, "ind_contrib",
      ind_contrib_state$plot, "Individual_Contributions"
    )
    register_plot_downloads(
      output, input, "eigencor",
      eigencor_state$plot, "Dimension_Metadata_Correlation"
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
      transform_info(NULL)
      bundle_data(NULL)

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

      # Always detect skewness (for info banner)
      skew_result <- detect_skewness(
        cleaned_data, measure_cols
      )
      skewness_info(skew_result)

      # Apply normalization only if enabled
      if (isTRUE(input$correct_skewness)) {
        if (any(skew_result$is_skewed)) {
          transform_res <- transform_skewed(
            cleaned_data, measure_cols, skew_result
          )
          if (transform_res$success) {
            cleaned_data <- transform_res$result$data
            transform_info(transform_res$result)
          } else {
            rhino$log$warn(
              "PCA: skewness correction failed,",
              " proceeding with untransformed data"
            )
          }
        }
      }

      # Residualize by a confound column, before scaling
      residualize_col <- input$residualizeCol
      if (!is.null(residualize_col) &&
          length(residualize_col) > 0 &&
          nzchar(residualize_col)) {
        rhino$log$info(
          "PCA: residualizing by '{residualize_col}'"
        )
        resid_res <- residualize_data(
          cleaned_data, measure_cols, residualize_col
        )
        if (!resid_res$success) {
          last_error(resid_res$error)
          return()
        }
        cleaned_data <- resid_res$result
      }

      # Determine scaling params for PCA
      analysis_data <- cleaned_data
      scale_method <- input$scale_method
      do_center <- !is.null(scale_method) &&
        scale_method %in% c("scale_center", "center_only")
      do_scale <- !is.null(scale_method) &&
        scale_method == "scale_center"

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

      # Run PCA / sPCA / IPCA depending on the Analysis
      # Settings tab's selection
      analysis_type <- input$analysis_type %||% "pca"
      ncp <- switch(
        analysis_type,
        spca = input$spca_ncomp %||% 2,
        ipca = input$ipca_ncomp %||% 2,
        NULL
      )
      keep_x <- if (analysis_type == "spca") {
        ncomp <- input$spca_ncomp %||% 2
        vapply(
          seq_len(ncomp),
          function(i) {
            val <- input[[paste0("spca_keepx_", i)]]
            if (is.null(val) || is.na(val)) 10 else val
          },
          numeric(1)
        )
      } else {
        NULL
      }
      ipca_mode <- input$ipca_mode %||% "deflation"

      rhino$log$info(
        "PCA: running {toupper(analysis_type)}",
        " ({length(measure_cols)} columns,",
        " {nrow(analysis_data)} rows)"
      )
      pca_res <- run_pca(
        analysis_data, measure_cols,
        meta_cols = meta_cols,
        center = do_center,
        scale. = do_scale,
        ncp = ncp,
        analysis_type = analysis_type,
        keep_x = keep_x,
        ipca_mode = ipca_mode
      )

      # Record whether the keepX values actually used came from a
      # completed auto-tune run, so the results panel can flag an
      # untuned selection rather than letting a UI default look
      # like a cross-validated result.
      if (analysis_type == "spca" && isTRUE(pca_res$success)) {
        tuned <- analysis_settings_state$keepx_tuned()
        pca_res$result$keepx_tuned <- !is.null(tuned) &&
          length(tuned) == length(keep_x) &&
          isTRUE(all(tuned == keep_x))
      }

      pca_result(pca_res)

      # Store bundle data for RDS export
      if (pca_res$success) {
        tf_info <- transform_info()
        t_params <- if (
          !is.null(tf_info) &&
          !is.null(tf_info$transform_params)
        ) {
          tf_info$transform_params
        } else {
          list()
        }
        bundle_data(list(
          raw_data = na_result$data,
          used_data = analysis_data,
          numeric_cols = measure_cols,
          meta_cols = meta_cols,
          transform_params = t_params,
          settings = list(
            skewness_correction = isTRUE(
              input$correct_skewness
            ),
            scale_method = scale_method %||% "none",
            residualize_col = residualize_col %||% "none"
          )
        ))
      }

      # Update dimension dropdowns to match actual components
      if (pca_res$success) {
        dim_choices <- colnames(pca_res$result$loadings)
        dim_ids <- c("dimX", "dimY", "dimZ")
        used <- character(0)
        selections <- list()
        for (i in seq_along(dim_ids)) {
          dim_id <- dim_ids[i]
          current <- input[[dim_id]]
          # Keep the current selection only if it is a valid
          # choice AND not already claimed by an earlier axis,
          # so dimX/dimY/dimZ never collide on the same value.
          sel <- if (!is.null(current) &&
                     current %in% dim_choices &&
                     !(current %in% used)) {
            current
          } else {
            remaining <- setdiff(dim_choices, used)
            if (length(remaining) > 0) {
              remaining[[min(i, length(remaining))]]
            } else {
              dim_choices[min(i, length(dim_choices))]
            }
          }
          used <- c(used, sel)
          selections[[dim_id]] <- sel
        }
        for (dim_id in dim_ids) {
          shiny$updateSelectizeInput(
            session, dim_id,
            choices = dim_choices,
            selected = selections[[dim_id]]
          )
        }

        # Update GroupBiplot choices from metadata
        meta <- pca_res$result$ind_meta
        if (!is.null(meta) &&
            !("Row" %in% names(meta) &&
              ncol(meta) == 1)) {
          shiny$updateSelectizeInput(
            session, "GroupBiplot",
            choices = names(meta),
            selected = input$GroupBiplot
          )
        }
      }

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

      # Preprocessing summary banner (NA + skewness)
      na_res <- na_info()
      tf_res <- transform_info()
      preprocess_banner <- preprocessing_summary$render_na_summary(
        na_res,
        transform_result = tf_res,
        n_measure_cols = length(input$measureVar)
      )

      # Skewness warning (when normalization disabled but skewed cols exist)
      skew_warning <- if (
        !isTRUE(input$correct_skewness) &&
        !is.null(skewness_info())
      ) {
        preprocessing_summary$render_skewness_warning(
          skewness_info(),
          n_measure_cols = length(input$measureVar)
        )
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
        kmo_title <- if (
          !is.null(kmo_res) && isTRUE(kmo_res$success)
        ) {
          overall <- kmo_res$result$overall
          shiny$tags$span(
            bsicons$bs_icon(
              "speedometer2", class = "me-1"
            ),
            "KMO Measure",
            shiny$tags$span(class = "mx-1", "\u2014"),
            shiny$tags$span(
              class = paste(
                "badge", kmo_badge_class(overall)
              ),
              sprintf("%.3f", overall)
            ),
            shiny$tags$small(
              class = "text-muted ms-1",
              kmo_interpretation(overall)
            )
          )
        } else {
          shiny$tags$span(
            bsicons$bs_icon(
              "speedometer2", class = "me-1"
            ),
            "KMO Measure"
          )
        }
        bslib$accordion_panel(
          title = kmo_title,
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
          opt_res$result, ns,
          variance_info = extract_variance_explained(pca_result),
          analysis_type = input$analysis_type %||% "pca"
        )
      } else {
        NULL
      }

      opt_panel <- if (!is.null(opt_content)) {
        opt_title <- if (
          !is.null(opt_res) && isTRUE(opt_res$success) &&
          !is.null(opt_res$result$summary$median_ncp)
        ) {
          shiny$tags$span(
            bsicons$bs_icon(
              "sliders", class = "me-1"
            ),
            "Optimal Number of Components",
            shiny$tags$span(class = "mx-1", "\u2014"),
            shiny$tags$span(
              class = "badge bg-primary",
              opt_res$result$summary$median_ncp
            )
          )
        } else {
          shiny$tags$span(
            bsicons$bs_icon(
              "sliders", class = "me-1"
            ),
            "Optimal Number of Components"
          )
        }
        bslib$accordion_panel(
          title = opt_title,
          value = "optimal_panel",
          opt_content
        )
      }

      # Compute display_ncp from optimal result
      # Show optimal median + 2 extra dims for context
      display_ncp <- compute_display_ncp(
        opt_res, pca_result()
      )

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
          pca_res$result, ns,
          display_ncp = display_ncp
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

      # Biplot panel content
      biplot_content <- if (
        !is.null(pca_res) && isTRUE(pca_res$success)
      ) {
        ggiraph$girafeOutput(
          ns("biplot"), height = "500px"
        )
      }

      biplot_panel <- if (!is.null(biplot_content)) {
        bslib$accordion_panel(
          title = shiny$tags$span(
            bsicons$bs_icon(
              "diagram-2", class = "me-1"
            ),
            "Biplot"
          ),
          value = "biplot_panel",
          biplot_content,
          download_buttons(ns, "biplot")
        )
      }

      # 3D Biplot panel content
      biplot3d_err <- biplot3d_state$error()
      biplot3d_content <- if (
        error_handling$is_app_error(biplot3d_err)
      ) {
        error_display$error_alert_structured(
          biplot3d_err, type = "danger"
        )
      } else if (
        !is.null(pca_res) &&
        isTRUE(pca_res$success) &&
        ncol(pca_res$result$loadings) >= 3
      ) {
        plotly$plotlyOutput(
          ns("biplot3d"), height = "600px"
        )
      }

      biplot3d_panel <- if (
        !is.null(biplot3d_content)
      ) {
        bslib$accordion_panel(
          title = shiny$tags$span(
            bsicons$bs_icon(
              "badge-3d", class = "me-1"
            ),
            "3D Biplot"
          ),
          value = "biplot3d_panel",
          biplot3d_content
        )
      }

      # Contribution %/cos2 are not meaningful for IPCA
      # (independent components are not variance-ranked) —
      # matches the has_contrib gating in pca_results.R.
      is_ipca <- !is.null(pca_res) && isTRUE(pca_res$success) &&
        identical(pca_res$result$analysis_type, "ipca")
      not_applicable_ipca <- shiny$tags$div(
        class = "alert alert-secondary mb-2 py-2",
        bsicons$bs_icon(
          "info-circle-fill", class = "me-2"
        ),
        paste(
          "Contribution % and cos2 are not applicable to",
          "IPCA — independent components are not ranked",
          "by variance."
        )
      )

      # Variable contribution jitter plot panel
      var_contrib_jitter_content <- if (is_ipca) {
        not_applicable_ipca
      } else if (
        !is.null(pca_res) && isTRUE(pca_res$success)
      ) {
        shiny$tagList(
          ggiraph$girafeOutput(
            ns("var_contrib_jitter"), height = "auto"
          ),
          shiny$uiOutput(ns("var_contrib_jitter_caption"))
        )
      }

      var_contrib_jitter_panel <- if (
        !is.null(var_contrib_jitter_content)
      ) {
        bslib$accordion_panel(
          title = shiny$tags$span(
            bsicons$bs_icon(
              "diagram-3", class = "me-1"
            ),
            "Variable Contributions"
          ),
          value = "var_contrib_jitter_panel",
          var_contrib_jitter_content,
          if (!is_ipca) download_buttons(ns, "var_contrib")
        )
      }

      # Individual contribution jitter plot panel
      ind_contrib_content <- if (is_ipca) {
        not_applicable_ipca
      } else if (
        !is.null(pca_res) && isTRUE(pca_res$success)
      ) {
        shiny$uiOutput(ns("ind_contrib_container"))
      }

      ind_contrib_panel <- if (
        !is.null(ind_contrib_content)
      ) {
        bslib$accordion_panel(
          title = shiny$tags$span(
            bsicons$bs_icon(
              "people-fill", class = "me-1"
            ),
            "Individual Contributions"
          ),
          value = "ind_contrib_panel",
          ind_contrib_content,
          if (!is_ipca) download_buttons(ns, "ind_contrib")
        )
      }

      # Eigencorrelation panel: PC dims vs metadata
      has_real_meta <- if (
        !is.null(pca_res) && isTRUE(pca_res$success)
      ) {
        meta <- pca_res$result$ind_meta
        !is.null(meta) &&
          !("Row" %in% names(meta) && ncol(meta) == 1)
      } else {
        FALSE
      }

      eigencor_content <- if (
        !is.null(pca_res) &&
        isTRUE(pca_res$success) &&
        has_real_meta
      ) {
        shiny$uiOutput(ns("eigencorplot_container"))
      }

      eigencor_panel <- if (
        !is.null(eigencor_content)
      ) {
        bslib$accordion_panel(
          title = shiny$tags$span(
            bsicons$bs_icon(
              "grid-1x2-fill", class = "me-1"
            ),
            "Dimension\u2013Metadata Correlation"
          ),
          value = "eigencor_panel",
          eigencor_content,
          download_buttons(ns, "eigencor")
        )
      }

      # Evidence behind the tuned keepX. Only meaningful once the
      # user has actually run the tuning, so absent otherwise.
      tune_details <- analysis_settings_state$tune_details()
      tune_panel <- if (
        !is.null(tune_details) &&
        !is.null(tune_details$cor_comp) &&
        identical(input$analysis_type, "spca")
      ) {
        bslib$accordion_panel(
          title = shiny$tags$span(
            bsicons$bs_icon("magic", class = "me-1"),
            "keepX Tuning Evidence"
          ),
          value = "tune_panel",
          ggiraph$girafeOutput(
            ns("tune_spca_plot"), height = "400px"
          ),
          render_tune_settings_note(tune_details$settings)
        )
      }

      shiny$tagList(
        preprocess_banner,
        skew_warning,
        bslib$accordion(
          id = ns("results_accordion"),
          open = "biplot_panel",
          multiple = TRUE,
          bslib$accordion_panel(
            title = shiny$tags$span(
              bsicons$bs_icon(
                "grid-3x3", class = "me-1"
              ),
              "Correlation Matrix"
            ),
            value = "correlation_panel",
            corr_content,
            download_buttons(ns, "corr")
          ),
          kmo_panel,
          opt_panel,
          pca_panel,
          biplot_panel,
          biplot3d_panel,
          var_contrib_jitter_panel,
          ind_contrib_panel,
          eigencor_panel,
          tune_panel
        )
      )
    })

    # keepX stability curve from tune.spca()'s correlation output
    output$tune_spca_plot <- ggiraph$renderGirafe({
      details <- analysis_settings_state$tune_details()
      shiny$req(details, details$cor_comp)
      plot_res <- create_tune_spca_plot(
        details$cor_comp,
        grid = details$settings$grid,
        chosen = details$keep_x
      )
      shiny$req(isTRUE(plot_res$success))
      plot_res$result
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
          "pca_bundle_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".rds"
        )
      },
      content = function(file) {
        pca_res <- pca_result()
        shiny$req(pca_res)
        shiny$req(pca_res$success)
        bd <- bundle_data()
        shiny$req(bd)
        bundle <- create_pca_bundle(
          pca_result = pca_res$result,
          raw_data = bd$raw_data,
          used_data = bd$used_data,
          numeric_cols = bd$numeric_cols,
          meta_cols = bd$meta_cols,
          transform_params = bd$transform_params,
          settings = bd$settings
        )
        saveRDS(bundle, file)
      }
    )

    # Return PCA result for downstream modules (e.g. LDA)
    pca_result
  })
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Describe the CV settings a keepX tuning run actually used
#'
#' Turns the recorded settings into the sentence a user can put in
#' a methods section, so the chosen keepX is reportable rather than
#' just a number that appeared in a box.
#'
#' @param settings List with $folds, $repeats, $grid, or NULL
#' @return Shiny tag, or NULL
render_tune_settings_note <- function(settings) {
  if (is.null(settings)) return(NULL)
  shiny$tags$small(
    class = "text-muted d-block mt-2",
    paste0(
      "keepX selected by ", settings$repeats, " repeat(s) of ",
      settings$folds, "-fold cross-validation over candidates [",
      paste(settings$grid, collapse = ", "), "], ",
      "maximising the correlation between the cross-validated ",
      "and full-data component (mixOmics::tune.spca). ",
      "Where the curve is already flat, a smaller keepX gives a ",
      "shorter variable list at no real cost to stability."
    )
  )
}

#' Compute display_ncp: how many dimensions to show in UI
#'
#' Uses the optimal components median recommendation + 2 extra
#' dimensions for context. Falls back to 5 if optimal result
#' is unavailable. Clamped to the actual number of components
#' in the PCA result.
#'
#' @param opt_res Optimal components result (may be NULL or failed)
#' @param pca_res PCA result wrapper (may be NULL or failed)
#' @return Integer, number of dimensions to display
compute_display_ncp <- function(opt_res, pca_res) {
  default_display <- 5
  extra_dims <- 2
  min_display <- 3

  # Get median recommendation from optimal result
  recommended <- if (
    !is.null(opt_res) && isTRUE(opt_res$success) &&
    !is.null(opt_res$result$summary$median_ncp)
  ) {
    opt_res$result$summary$median_ncp
  } else {
    NULL
  }

  display <- if (!is.null(recommended)) {
    max(recommended + extra_dims, min_display)
  } else {
    default_display
  }

  # Clamp to actual number of components
  if (!is.null(pca_res) && isTRUE(pca_res$success)) {
    total_dims <- ncol(pca_res$result$loadings)
    display <- min(display, total_dims)
  }

  as.integer(display)
}

#' Create SVG + PNG download buttons for an accordion panel
#'
#' @param ns Namespace function
#' @param id_prefix Character, e.g. "corr", "biplot"
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

#' Register SVG + PNG download handlers for a plot
#'
#' @param output Shiny output object
#' @param input Shiny input object
#' @param id_prefix Character, e.g. "corr", "biplot"
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
