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
  app/logic/error_handling,
  app/logic/pca/correlation_plot[compute_correlation_data],
  app/logic/pca/kmo[calculate_kmo, kmo_badge_class, kmo_interpretation],
  app/logic/pca/na_handling[clean_na_rows],
  app/logic/pca/optimal_components[calculate_optimal_components],
  app/logic/pca/pca[validate_inputs, run_pca],
  app/logic/pca/pca_export[create_pca_excel],
  app/logic/pca/scaling[scale_data],
  app/view/components/sidebar_tabs,
  app/view/error_display,
  app/view/pca/actions,
  app/view/pca/biplot,
  app/view/pca/biplot3d,
  app/view/pca/correlation_plot[render_output],
  app/view/pca/eigencorplot,
  app/view/pca/ind_contrib,
  app/view/pca/var_contrib,
  app/view/pca/var_contrib_jitter,
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

    # Delegate variable contribution chart rendering
    var_contrib_state <- var_contrib$render_output(
      input, output, session,
      pca_result = pca_result,
      display_ncp = display_ncp
    )

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
      var_contrib_state$plot, "Variable_Contributions"
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

      # Update dimension dropdowns to match actual components
      if (pca_res$success) {
        dim_choices <- colnames(pca_res$result$var$coord)
        for (dim_id in c("dimX", "dimY", "dimZ")) {
          current <- input[[dim_id]]
          sel <- if (!is.null(current) &&
                     current %in% dim_choices) {
            current
          } else {
            dim_choices[min(
              which(dim_id == c("dimX", "dimY", "dimZ")),
              length(dim_choices)
            )]
          }
          shiny$updateSelectizeInput(
            session, dim_id,
            choices = dim_choices,
            selected = sel
          )
        }

        # Update GroupBiplot choices from metadata
        meta <- pca_res$result$ind$meta
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
          opt_res$result, ns
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
        ncol(pca_res$result$var$coord) >= 3
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

      # Variable contribution chart panel
      var_contrib_content <- if (
        !is.null(pca_res) && isTRUE(pca_res$success)
      ) {
        ggiraph$girafeOutput(
          ns("var_contrib_circles"), height = "auto"
        )
      }

      var_contrib_panel <- if (
        !is.null(var_contrib_content)
      ) {
        bslib$accordion_panel(
          title = shiny$tags$span(
            bsicons$bs_icon(
              "bar-chart-fill", class = "me-1"
            ),
            "Variable Contributions"
          ),
          value = "var_contrib_panel",
          var_contrib_content,
          download_buttons(ns, "var_contrib")
        )
      }

      # Variable contribution jitter plot panel (TEST)
      var_contrib_jitter_content <- if (
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
              "people-fill", class = "me-1"
            ),
            "Variable Contributions TEST"
          ),
          value = "var_contrib_jitter_panel",
          var_contrib_jitter_content
        )
      }

      # Individual contribution jitter plot panel
      ind_contrib_content <- if (
        !is.null(pca_res) && isTRUE(pca_res$success)
      ) {
        ggiraph$girafeOutput(
          ns("ind_contrib_plot"), height = "auto"
        )
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
          download_buttons(ns, "ind_contrib")
        )
      }

      # Eigencorrelation panel: PC dims vs metadata
      has_real_meta <- if (
        !is.null(pca_res) && isTRUE(pca_res$success)
      ) {
        meta <- pca_res$result$ind$meta
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
        ggiraph$girafeOutput(
          ns("eigencorplot"), height = "auto"
        )
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

      shiny$tagList(
        na_banner,
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
          var_contrib_panel,
          var_contrib_jitter_panel,
          ind_contrib_panel,
          eigencor_panel
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


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

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
    total_dims <- ncol(pca_res$result$var$coord)
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
