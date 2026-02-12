box::use(
  bsicons,
  bslib,
  ggiraph,
  rhino,
  shiny,
)

box::use(
  app/logic/error_handling,
  app/logic/statistics/robust_tests,
  app/logic/statistics/validate,
  app/view/components/sidebar_tabs,
  app/view/error_display,
  app/view/statistics/adjustments,
  app/view/statistics/bootstrap,
  app/view/statistics/options,
)

# --- Private helper: render omnibus result as UI ---
render_omnibus_result <- function(result, x_axis, approach) {
  if (is.null(result)) {
    return(shiny$tags$div(
      class = "text-muted small px-2",
      "No omnibus result available."
    ))
  }

  if (error_handling$is_app_error(result)) {
    return(error_display$error_alert_structured(
      result, type = "warning"
    ))
  }

  if (is.data.frame(result) && nrow(result) > 0) {
    header_label <- if (approach == "robust") {
      paste0(
        "Robust ",
        length(x_axis),
        "-Way Trimmed Means [ANOVA]",
        " \u2014 Welch-Yuen"
      )
    } else {
      paste0(
        "Classical ",
        length(x_axis),
        "-Way ANOVA"
      )
    }

    return(shiny$tags$div(
      class = "px-2 pt-2",
      shiny$tags$h6(
        class = "text-muted mb-2",
        bsicons$bs_icon("table", class = "me-1"),
        header_label
      ),
      shiny$tags$div(
        class = "table-responsive",
        shiny$tags$table(
          class = paste(
            "table table-sm table-striped",
            "table-hover mb-0"
          ),
          shiny$tags$thead(
            shiny$tags$tr(
              lapply(names(result), function(col) {
                shiny$tags$th(
                  class = "small",
                  gsub("_", " ", col)
                )
              })
            )
          ),
          shiny$tags$tbody(
            lapply(seq_len(nrow(result)), function(i) {
              shiny$tags$tr(
                lapply(
                  names(result),
                  function(col) {
                    shiny$tags$td(
                      class = "small",
                      as.character(result[i, col])
                    )
                  }
                )
              )
            })
          )
        )
      )
    ))
  }

  shiny$tags$div(
    class = "text-muted small px-2",
    "Unexpected result format."
  )
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  sidebar_tabs$tab_layout(
    ns = ns,
    sidebar_id = "sidebar_tabs",
    tabs = list(
      options$tab_ui(ns),
      bootstrap$tab_ui(ns),
      adjustments$tab_ui(ns)
    ),
    main_content = shiny$tags$div(
      class = "scrollable-content",
      shiny$uiOutput(ns("main_content"))
    ),
    enable_responsive_plots = TRUE,
    results_id = "main_content",
    action_button = shiny$tagList(
      shiny$actionButton(
        inputId = ns("compute_button"),
        label = "Compute Statistics",
        class = "btn-primary btn-sm w-100",
        icon = bsicons$bs_icon("calculator")
      ),
      shiny$tags$div(
        class = "small text-muted mt-2",
        "Computation may take some time."
      )
    )
  )
}

#' @export
server <- function(id, input_data, data_version,
                   plotting_x_axis = NULL,
                   plotting_measures = NULL,
                   plotting_trim_percent = NULL,
                   plotting_plot_objects = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    last_error <- shiny$reactiveVal(NULL)
    computation_results <- shiny$reactiveVal(NULL)
    computation_status <- shiny$reactiveVal("idle")
    # Snapshot of plot objects captured at compute time
    snapshotted_plots <- shiny$reactiveVal(NULL)

    # --- Window size for responsive ggiraph sizing ---
    window_size <- shiny$reactiveVal(
      list(width = 800, height = 300)
    )
    shiny$observe({
      ws <- input$windowSize
      if (!is.null(ws) && !is.null(ws$width)) {
        window_size(ws)
      }
    })

    # --- Reset state on new data ---
    shiny$observeEvent(data_version(), {
      computation_results(NULL)
      computation_status("idle")
      snapshotted_plots(NULL)
      last_error(NULL)
      rhino$log$info("Statistics: state reset for new data")
    }, ignoreInit = TRUE)

    # --- Delegate to sub-module servers ---
    options$tab_server(
      input, output, session,
      plotting_x_axis = plotting_x_axis,
      plotting_trim_percent = plotting_trim_percent
    )

    # --- Collect statistics parameters ---
    stats_params <- shiny$reactive({
      list(
        test_approach =
          input$test_approach %||% "robust",
        use_bootstrap =
          input$use_bootstrap %||% FALSE,
        boot_samples =
          input$boot_samples %||% 599,
        boot_sample_size =
          input$boot_sample_size,
        p_val_cor_method =
          input$p_val_cor_method %||% "bonferroni",
        show_additional_output =
          input$show_additional_output %||% FALSE,
        use_scientific_notation =
          input$use_scientific_notation %||% FALSE,
        filter_p_values =
          input$filter_p_values %||% FALSE
      )
    })

    # --- Handle compute button click ---
    shiny$observeEvent(input$compute_button, {
      data <- input_data()
      measures <- if (!is.null(plotting_measures)) {
        plotting_measures()
      } else {
        NULL
      }
      x_cols <- if (!is.null(plotting_x_axis)) {
        plotting_x_axis()
      } else {
        NULL
      }
      params <- stats_params()

      if (is.null(data) || nrow(data) == 0) {
        computation_status("error")
        computation_results(list(
          error = paste(
            "No data available. Please load data",
            "and configure the Plotting tab first."
          )
        ))
        return()
      }

      if (length(measures) == 0) {
        computation_status("error")
        computation_results(list(
          error = paste(
            "No measurement columns selected.",
            "Please select measurements in the",
            "Plotting tab."
          )
        ))
        return()
      }

      if (length(x_cols) == 0) {
        computation_status("error")
        computation_results(list(
          error = paste(
            "No X-axis columns selected.",
            "Please select X-axis in the",
            "Plotting tab."
          )
        ))
        return()
      }

      # Snapshot current plot objects from plotting tab
      cached_plots <- if (!is.null(plotting_plot_objects)) {
        plotting_plot_objects()
      } else {
        NULL
      }

      if (is.null(cached_plots) || length(cached_plots) == 0) {
        computation_status("error")
        computation_results(list(
          error = paste(
            "No plots available. Please ensure",
            "the Plotting tab has generated plots",
            "before computing statistics."
          )
        ))
        return()
      }

      computation_status("computing")
      rhino$log$info(
        "Statistics: computing for ",
        "{length(measures)} measure(s), ",
        "{length(x_cols)}-way design"
      )

      tr_val <- if (!is.null(plotting_trim_percent)) {
        (plotting_trim_percent() %||% 0) / 100
      } else {
        0
      }

      # Store snapshot so plots don't change if user
      # modifies the Plotting tab after clicking Compute
      snapshotted_plots(cached_plots)

      # --- Run omnibus tests per measurement ---
      n_ways <- length(x_cols)
      omnibus_results <- lapply(measures, function(m) {
        if (params$test_approach == "robust") {
          if (n_ways == 1) {
            robust_tests$perform_t1way(
              df = data,
              x_axis = x_cols,
              measure_col = m,
              tr_value = tr_val,
              use_bootstrap = params$use_bootstrap,
              boot_samples = params$boot_samples,
              boot_sample_size = params$boot_sample_size
            )
          } else if (n_ways == 2) {
            robust_tests$perform_t2way(
              df = data,
              x_axis = x_cols,
              measure_col = m,
              tr_value = tr_val,
              use_bootstrap = params$use_bootstrap,
              boot_samples = params$boot_samples,
              boot_sample_size = params$boot_sample_size
            )
          } else if (n_ways == 3) {
            robust_tests$perform_t3way(
              df = data,
              x_axis = x_cols,
              measure_col = m,
              tr_value = tr_val,
              use_bootstrap = params$use_bootstrap,
              boot_samples = params$boot_samples,
              boot_sample_size = params$boot_sample_size
            )
          } else {
            error_handling$simple_error(
              message = paste0(
                n_ways,
                "-way robust test is not supported."
              ),
              operation_name = "statistics_compute"
            )
          }
        } else {
          # Parametric tests not yet implemented
          error_handling$simple_error(
            message = paste(
              "Parametric tests not yet implemented."
            ),
            operation_name = "statistics_compute"
          )
        }
      })
      names(omnibus_results) <- measures

      computation_results(list(
        measures = measures,
        x_axis = x_cols,
        params = params,
        trim_value = tr_val,
        omnibus = omnibus_results,
        timestamp = Sys.time()
      ))
      computation_status("done")

      rhino$log$info("Statistics: computation complete")
    })

    # --- Register ggiraph outputs when results arrive ---
    shiny$observeEvent(snapshotted_plots(), {
      plots <- snapshotted_plots()
      shiny$req(plots)

      lapply(names(plots), function(measure) {
        local({
          local_measure <- measure
          safe_id <- make.names(local_measure)
          output_id <- paste0("stat_plot_", safe_id)

          output[[output_id]] <- ggiraph$renderGirafe({
            p <- snapshotted_plots()[[local_measure]]
            shiny$req(p)

            ws <- window_size()
            w_svg <- max(4, ws$width / 100)
            # ~35% of viewport height in inches (96 dpi)
            h_svg <- max(3.5, (ws$height * 0.35) / 96)

            ggiraph$girafe(
              ggobj = p,
              width_svg = w_svg,
              height_svg = h_svg,
              options = list(
                ggiraph$opts_hover(
                  css = paste(
                    "fill-opacity:1;",
                    "stroke-width:2;"
                  )
                ),
                ggiraph$opts_tooltip(
                  css = paste(
                    "background-color:white;",
                    "padding:8px;",
                    "border-radius:4px;",
                    "border:1px solid #ccc;",
                    "font-size:12px;"
                  ),
                  use_fill = FALSE
                ),
                ggiraph$opts_selection(
                  type = "none"
                )
              )
            )
          })
        })
      })
    }, ignoreNULL = TRUE)

    # --- Main content: placeholder, error, or results ---
    output$main_content <- shiny$renderUI({
      err <- last_error()
      if (error_handling$is_app_error(err)) {
        return(
          error_display$error_alert_structured(
            err, type = "danger"
          )
        )
      }

      status <- computation_status()
      results <- computation_results()

      # Idle / no results yet
      if (status == "idle" || is.null(results)) {
        return(
          bslib$card(
            bslib$card_header(
              "Statistical Test Results"
            ),
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
                    "calculator",
                    size = "3em",
                    class = "mb-3"
                  )
                ),
                shiny$tags$p(
                  "Configure options in the sidebar",
                  " and click ",
                  shiny$tags$strong(
                    "Compute Statistics"
                  ),
                  " to run the analysis."
                ),
                shiny$tags$p(
                  class = "small",
                  paste(
                    "Data selection, filtering, and",
                    "trimming are inherited from the",
                    "Plotting tab."
                  )
                )
              )
            )
          )
        )
      }

      # Error state from computation
      if (status == "error" && !is.null(results$error)) {
        return(
          bslib$card(
            bslib$card_header(
              class = "bg-danger text-white",
              "Error"
            ),
            bslib$card_body(
              shiny$tags$div(
                class = "alert alert-danger",
                bsicons$bs_icon(
                  "exclamation-triangle"
                ),
                " ",
                results$error
              )
            )
          )
        )
      }

      # Results state — per-measurement cards with plots
      if (status == "done") {
        measures <- results$measures
        plots <- snapshotted_plots()

        # Build one card per measurement
        measurement_cards <- lapply(
          measures,
          function(m) {
            safe_id <- make.names(m)
            output_id <- paste0("stat_plot_", safe_id)
            has_plot <- !is.null(plots) &&
              m %in% names(plots)

            plot_ui <- if (has_plot) {
              shiny$tags$div(
                class = paste(
                  "mb-3 border-bottom pb-3",
                  "responsive-plot"
                ),
                ggiraph$girafeOutput(
                  ns(output_id),
                  height = "auto",
                  width = "100%"
                )
              )
            } else {
              shiny$tags$div(
                class = paste(
                  "mb-3 border-bottom pb-3",
                  "text-center text-muted py-3"
                ),
                shiny$tags$p(
                  "No plot available for this",
                  " measurement."
                )
              )
            }

            bslib$card(
              class = "mb-3 plot-card",
              bslib$card_header(
                class = paste(
                  "py-2 d-flex",
                  "justify-content-between",
                  "align-items-center"
                ),
                shiny$tags$span(
                  bsicons$bs_icon(
                    "graph-up", class = "me-2"
                  ),
                  m
                ),
                shiny$tags$span(
                  class = "badge bg-secondary",
                  paste0(
                    length(results$x_axis),
                    "-way"
                  )
                )
              ),
              bslib$card_body(
                class = "p-2 plot-card-body",
                plot_ui,
                # Omnibus test results
                render_omnibus_result(
                  results$omnibus[[m]],
                  results$x_axis,
                  results$params$test_approach
                )
              )
            )
          }
        )

        return(
          shiny$tagList(
            shiny$tags$div(
              class = paste(
                "d-flex justify-content-between",
                "align-items-center mb-3"
              ),
              shiny$tags$h5(
                class = "mb-0",
                "Statistical Test Results"
              ),
              shiny$tags$small(
                class = "text-muted",
                paste(
                  "Computed:",
                  format(
                    results$timestamp,
                    "%H:%M:%S"
                  )
                )
              )
            ),
            shiny$tags$div(
              class = "alert alert-info py-2",
              shiny$tags$small(
                shiny$tags$strong(
                  "Configuration: "
                ),
                paste0(
                  length(results$measures),
                  " measurement(s), ",
                  length(results$x_axis),
                  "-way design, ",
                  "Bootstrap: ",
                  ifelse(
                    results$params$use_bootstrap,
                    "Yes", "No"
                  ),
                  ", P-adjustment: ",
                  results$params$p_val_cor_method
                )
              )
            ),
            measurement_cards
          )
        )
      }
    })

    # Return for downstream modules
    invisible(NULL)
  })
}
