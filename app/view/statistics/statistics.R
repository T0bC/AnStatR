box::use(
  bsicons,
  bslib,
  ggiraph,
  rhino,
  shiny,
  stats,
)

box::use(
  app/logic/shared/error_handling,
  app/logic/statistics/nonparametric_posthoc,
  app/logic/statistics/nonparametric_tests,
  app/logic/statistics/parametric_posthoc,
  app/logic/statistics/parametric_tests,
  app/logic/statistics/report,
  app/logic/statistics/robust_posthoc,
  app/logic/statistics/robust_tests,
  app/logic/statistics/validate,
  app/view/components/sidebar_tabs,
  app/view/shared/error_display,
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
    n_ways <- length(x_axis)
    header_label <- if (approach == "robust") {
      paste0(
        "Robust ", n_ways, "-Way ANOVA",
        " \u2014 Trimmed Means (t", n_ways, "way)"
      )
    } else if (approach == "nonparametric") {
      if (n_ways == 1) {
        "Non-Parametric 1-Way \u2014 Kruskal-Wallis"
      } else {
        paste0(
          "Non-Parametric ", n_ways,
          "-Way \u2014 Aligned Rank Transform"
        )
      }
    } else {
      paste0(
        "Classical ", n_ways, "-Way ANOVA"
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

# --- Private helper: build a compact HTML table from a data frame ---
build_posthoc_table <- function(df) {
  shiny$tags$table(
    class = "table table-sm table-striped table-hover mb-0",
    shiny$tags$thead(
      shiny$tags$tr(
        lapply(names(df), function(col) {
          shiny$tags$th(class = "small", col)
        })
      )
    ),
    shiny$tags$tbody(
      lapply(seq_len(nrow(df)), function(i) {
        shiny$tags$tr(
          lapply(names(df), function(col) {
            shiny$tags$td(
              class = "small",
              as.character(df[i, col])
            )
          })
        )
      })
    )
  )
}

# --- Private helper: resolve plot key from measure name ---
# Plots are keyed by raw column name; when normalization is
# active the measure name has a _normalized suffix.
resolve_plot_key <- function(measure) {
  sub("_normalized$", "", measure)
}

# --- Private helper: exclude outlier/trimmed rows for a measure ---
filter_excluded_rows <- function(df, measure_col) {
  # Strip _normalized suffix for flag column lookup
  base_col <- sub("_normalized$", "", measure_col)
  outlier_col <- paste0(base_col, "_outlier")
  trimmed_col <- paste0(base_col, "_trimmed")

  keep <- rep(TRUE, nrow(df))
  if (outlier_col %in% names(df)) {
    keep <- keep & !df[[outlier_col]]
  }
  if (trimmed_col %in% names(df)) {
    keep <- keep & !df[[trimmed_col]]
  }
  df[keep, , drop = FALSE]
}

# --- Private helper: render post-hoc result as UI ---
render_posthoc_result <- function(result, x_axis, params) {
  if (is.null(result)) return(NULL)

  if (error_handling$is_app_error(result)) {
    return(shiny$tags$div(
      class = "mt-3",
      shiny$tags$h6(
        class = "text-primary mb-2",
        bsicons$bs_icon("table", class = "me-1"),
        "Combined Pairwise Comparisons"
      ),
      error_display$error_alert_structured(
        result, type = "warning"
      )
    ))
  }

  if (is.data.frame(result) && nrow(result) > 0) {
    display_df <- result

    # Detect which prefix set is present
    has_lincon <- any(grepl("^Lincon\\.", names(display_df)))
    has_tukey <- any(grepl("^Tukey\\.", names(display_df)))
    has_dunn <- any(grepl("^Dunn\\.", names(display_df)))
    has_wilcox <- any(grepl("^Wilcox\\.", names(display_df)))
    has_art <- any(grepl("^ART\\.", names(display_df)))

    # Determine the p-adjusted column for filtering
    p_adj_col <- if (has_lincon) {
      "Lincon.p.adjusted"
    } else if (has_tukey) {
      "Tukey.p.adjusted"
    } else if (has_dunn) {
      "Dunn.p.adjusted"
    } else if (has_wilcox) {
      "Wilcox.p.adjusted"
    } else if (has_art) {
      "ART.p.adjusted"
    } else {
      NULL
    }

    if (isTRUE(params$filter_p_values) &&
        !is.null(p_adj_col) &&
        p_adj_col %in% names(display_df) &&
        is.numeric(display_df[[p_adj_col]])) {
      display_df <- display_df[
        display_df[[p_adj_col]] < 0.07, ,
        drop = FALSE
      ]
    }

    if (nrow(display_df) == 0) {
      return(shiny$tags$div(
        class = "mt-3 text-muted small px-2",
        "No significant pairwise comparisons found."
      ))
    }

    # Determine left/right panel prefixes and labels
    if (has_lincon) {
      left_prefix <- "Lincon"
      left_label <- "Lincon"
      right_prefix <- "Cliff"
      right_label <- "Cliff's Delta"
    } else if (has_dunn) {
      left_prefix <- "Dunn"
      left_label <- "Dunn's Test"
      right_prefix <- "Cliff"
      right_label <- "Cliff's Delta"
    } else if (has_wilcox) {
      left_prefix <- "Wilcox"
      left_label <- "Pairwise Wilcoxon"
      right_prefix <- "Cliff"
      right_label <- "Cliff's Delta"
    } else if (has_art) {
      left_prefix <- "ART"
      left_label <- "ART Contrasts"
      right_prefix <- "ART.d"
      right_label <- "ART Cohen's d"
    } else {
      left_prefix <- "Tukey"
      left_label <- "Tukey HSD"
      right_prefix <- "Cohen"
      right_label <- "Cohen's d"
    }

    # For ART results, left/right both start with "ART." so
    # we split by explicit column names instead of prefix regex.
    if (has_art) {
      art_left_names <- c(
        "ART.estimate", "ART.SE", "ART.df",
        "ART.t.ratio", "ART.p.value", "ART.p.adjusted"
      )
      art_right_names <- c(
        "ART.d", "ART.d.ci.lower", "ART.d.ci.upper"
      )
      left_cols <- intersect(art_left_names, names(display_df))
      right_cols <- intersect(art_right_names, names(display_df))
    } else {
      left_cols <- grep(
        paste0("^", left_prefix, "\\."),
        names(display_df), value = TRUE
      )
      right_cols <- grep(
        paste0("^", right_prefix, "\\."),
        names(display_df), value = TRUE
      )
    }

    # Left table: Interaction + left columns, strip prefix
    left_df <- display_df[
      , c("Interaction", left_cols), drop = FALSE
    ]
    names(left_df) <- gsub(
      paste0("^", left_prefix, "\\."), "", names(left_df)
    )

    # Right table: right columns only, strip prefix
    right_df <- display_df[, right_cols, drop = FALSE]
    if (has_art) {
      names(right_df) <- gsub("^ART\\.d\\.", "", names(right_df))
      names(right_df) <- gsub("^ART\\.d$", "d", names(right_df))
    } else {
      names(right_df) <- gsub(
        paste0("^", right_prefix, "\\."), "", names(right_df)
      )
    }

    return(shiny$tags$div(
      class = "mt-3 px-2",
      shiny$tags$h6(
        class = "text-primary mb-2",
        bsicons$bs_icon("table", class = "me-1"),
        "Pairwise Comparisons"
      ),
      shiny$tags$div(
        class = "d-flex gap-3",
        # Left panel
        shiny$tags$div(
          class = "flex-fill",
          shiny$tags$h6(
            class = "text-secondary mb-1 small fw-bold",
            left_label
          ),
          shiny$tags$div(
            class = "table-responsive",
            build_posthoc_table(left_df)
          )
        ),
        # Right panel
        shiny$tags$div(
          class = "flex-fill",
          shiny$tags$h6(
            class = "text-secondary mb-1 small fw-bold",
            right_label
          ),
          shiny$tags$div(
            class = "table-responsive",
            build_posthoc_table(right_df)
          )
        )
      )
    ))
  }

  NULL
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
                   plotting_plot_objects = NULL,
                   plotting_normalize_enabled = NULL,
                   plotting_transform_info = NULL) {
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
        filter_p_values =
          input$filter_p_values %||% FALSE,
        filter_valid_comparisons =
          input$filter_valid_comparisons %||% FALSE,
        np_posthoc_method =
          input$np_posthoc_method %||% "dunn"
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

      # When normalization is active, use _normalized columns
      norm_active <- if (!is.null(plotting_normalize_enabled)) {
        isTRUE(plotting_normalize_enabled())
      } else {
        FALSE
      }
      if (norm_active && !is.null(measures)) {
        measures <- vapply(measures, function(col) {
          norm_col <- paste0(col, "_normalized")
          if (norm_col %in% names(data)) norm_col else col
        }, character(1), USE.NAMES = FALSE)
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

      # Immediate notification so user knows computation started
      shiny$showNotification(
        paste0(
          "Computing statistics for ",
          length(measures), " measure(s) with ",
          length(x_cols), "-way design. ",
          "This may take a moment..."
        ),
        type = "message",
        duration = 5
      )

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

      # Wrap computation in withProgress to force UI flush
      shiny$withProgress(
        message = "Computing Statistics",
        value = 0, {

      # --- Run omnibus tests per measurement ---
      n_ways <- length(x_cols)
      omnibus_results <- lapply(measures, function(m) {
        df_m <- filter_excluded_rows(data, m)
        if (params$test_approach == "robust") {
          if (n_ways == 1) {
            robust_tests$perform_t1way(
              df = df_m,
              x_axis = x_cols,
              measure_col = m,
              tr_value = tr_val,
              use_bootstrap = params$use_bootstrap,
              boot_samples = params$boot_samples,
              boot_sample_size = params$boot_sample_size
            )
          } else if (n_ways == 2) {
            robust_tests$perform_t2way(
              df = df_m,
              x_axis = x_cols,
              measure_col = m,
              tr_value = tr_val,
              use_bootstrap = params$use_bootstrap,
              boot_samples = params$boot_samples,
              boot_sample_size = params$boot_sample_size
            )
          } else if (n_ways == 3) {
            robust_tests$perform_t3way(
              df = df_m,
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
        } else if (params$test_approach == "parametric") {
          if (n_ways == 1) {
            parametric_tests$perform_anova1way(
              df = df_m,
              x_axis = x_cols,
              measure_col = m,
              tr_value = tr_val
            )
          } else if (n_ways == 2) {
            parametric_tests$perform_anova2way(
              df = df_m,
              x_axis = x_cols,
              measure_col = m,
              tr_value = tr_val
            )
          } else if (n_ways == 3) {
            parametric_tests$perform_anova3way(
              df = df_m,
              x_axis = x_cols,
              measure_col = m,
              tr_value = tr_val
            )
          } else {
            error_handling$simple_error(
              message = paste0(
                n_ways,
                "-way parametric test is not ",
                "supported."
              ),
              operation_name = "statistics_compute"
            )
          }
        } else if (params$test_approach == "nonparametric") {
          if (n_ways == 1) {
            nonparametric_tests$perform_kruskal1way(
              df = df_m,
              x_axis = x_cols,
              measure_col = m,
              tr_value = tr_val
            )
          } else if (n_ways == 2) {
            nonparametric_tests$perform_art2way(
              df = df_m,
              x_axis = x_cols,
              measure_col = m,
              tr_value = tr_val
            )
          } else if (n_ways == 3) {
            nonparametric_tests$perform_art3way(
              df = df_m,
              x_axis = x_cols,
              measure_col = m,
              tr_value = tr_val
            )
          } else {
            error_handling$simple_error(
              message = paste0(
                n_ways,
                "-way non-parametric test is not ",
                "supported."
              ),
              operation_name = "statistics_compute"
            )
          }
        } else {
          error_handling$simple_error(
            message = paste0(
              "Unknown test approach: '",
              params$test_approach, "'."
            ),
            operation_name = "statistics_compute"
          )
        }
      })
      names(omnibus_results) <- measures

      shiny$incProgress(0.4, detail = "Running omnibus tests...")

      # --- Count NAs per measure with per-group breakdown ---
      na_details <- lapply(measures, function(m) {
        df_m <- filter_excluded_rows(data, m)
        na_mask <- is.na(df_m[[m]])
        total_na <- sum(na_mask)
        if (total_na == 0) {
          return(list(total = 0L, groups = NULL))
        }
        # Build per-group NA counts
        grp <- df_m[, x_cols, drop = FALSE]
        grp$.na <- na_mask
        agg <- stats$aggregate(
          .na ~ .,
          data = grp, FUN = sum
        )
        # Keep only groups that actually have NAs
        agg <- agg[agg$.na > 0, , drop = FALSE]
        list(total = total_na, groups = agg)
      })
      names(na_details) <- measures

      shiny$incProgress(0.1, detail = "Running post-hoc tests...")

      # --- Run post-hoc tests per measurement ---
      posthoc_results <- if (
        params$test_approach == "robust"
      ) {
        ph <- lapply(measures, function(m) {
          df_m <- filter_excluded_rows(data, m)
          robust_posthoc$perform_combined_posthoc(
            df = df_m,
            x_axis = x_cols,
            measure_col = m,
            tr_value = tr_val,
            use_bootstrap = params$use_bootstrap,
            boot_samples = params$boot_samples,
            boot_sample_size = params$boot_sample_size,
            p_adjust_method =
              params$p_val_cor_method,
            filter_valid = isTRUE(
              params$filter_valid_comparisons
            )
          )
        })
        names(ph) <- measures
        ph
      } else if (params$test_approach == "parametric") {
        ph <- lapply(measures, function(m) {
          df_m <- filter_excluded_rows(data, m)
          parametric_posthoc$perform_combined_parametric_posthoc(
            df = df_m,
            x_axis = x_cols,
            measure_col = m,
            p_adjust_method =
              params$p_val_cor_method,
            filter_valid = isTRUE(
              params$filter_valid_comparisons
            )
          )
        })
        names(ph) <- measures
        ph
      } else if (params$test_approach == "nonparametric") {
        ph <- lapply(measures, function(m) {
          df_m <- filter_excluded_rows(data, m)
          nonparametric_posthoc$perform_combined_nonparametric_posthoc(
            df = df_m,
            x_axis = x_cols,
            measure_col = m,
            p_adjust_method =
              params$p_val_cor_method,
            filter_valid = isTRUE(
              params$filter_valid_comparisons
            ),
            posthoc_method =
              params$np_posthoc_method
          )
        })
        names(ph) <- measures
        ph
      } else {
        NULL
      }

      shiny$incProgress(1, detail = "Finalizing results...")

      computation_results(list(
        measures = measures,
        x_axis = x_cols,
        params = params,
        trim_value = tr_val,
        omnibus = omnibus_results,
        posthoc = posthoc_results,
        na_details = na_details,
        timestamp = Sys.time()
      ))
      }) # end withProgress

      computation_status("done")

      rhino$log$info("Statistics: computation complete")
    })

    # --- Register ggiraph outputs when results arrive ---
    shiny$observeEvent(snapshotted_plots(), {
      plots <- snapshotted_plots()
      shiny$req(plots)

      # Register outputs for raw plot keys
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
                ggiraph$opts_sizing(
                  rescale = FALSE
                ),
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

      # Also register under _normalized keys so the same
      # ggiraph output is reachable by normalized measure name
      results <- computation_results()
      if (!is.null(results) && !is.null(results$measures)) {
        norm_measures <- results$measures[
          grepl("_normalized$", results$measures)
        ]
        for (nm in norm_measures) {
          local({
            local_nm <- nm
            raw_key <- resolve_plot_key(local_nm)
            safe_id <- make.names(local_nm)
            output_id <- paste0("stat_plot_", safe_id)

            output[[output_id]] <- ggiraph$renderGirafe({
              p <- snapshotted_plots()[[raw_key]]
              shiny$req(p)

              ws <- window_size()
              w_svg <- max(4, ws$width / 100)
              h_svg <- max(3.5, (ws$height * 0.35) / 96)

              ggiraph$girafe(
                ggobj = p,
                width_svg = w_svg,
                height_svg = h_svg,
                options = list(
                  ggiraph$opts_sizing(
                    rescale = FALSE
                  ),
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
        }
      }
    }, ignoreNULL = TRUE)

    # --- Register download handlers when results arrive ---
    shiny$observeEvent(computation_results(), {
      results <- computation_results()
      shiny$req(results, results$measures)
      plots <- snapshotted_plots()

      lapply(results$measures, function(measure) {
        local({
          local_m <- measure
          safe_id <- make.names(local_m)
          dl_id <- paste0("dl_report_", safe_id)

          output[[dl_id]] <- shiny$downloadHandler(
            filename = function() {
              paste0(
                "statistics_", local_m, "_",
                format(Sys.time(), "%Y%m%d_%H%M%S"),
                ".html"
              )
            },
            content = function(file) {
              res <- computation_results()
              pl <- snapshotted_plots()
              plot_key <- resolve_plot_key(local_m)
              html <- report$generate_html_report(
                measure = local_m,
                plot_object = pl[[plot_key]],
                omnibus_result = res$omnibus[[local_m]],
                posthoc_result = res$posthoc[[local_m]],
                params = res$params,
                x_axis = res$x_axis,
                timestamp = res$timestamp
              )
              writeLines(html, file)
              rhino$log$info(
                "Download: HTML report '{local_m}'"
              )
            }
          )
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

      # Computing state - show spinner
      if (status == "computing") {
        return(
          bslib$card(
            bslib$card_header("Computing..."),
            bslib$card_body(
              class = paste(
                "d-flex align-items-center",
                "justify-content-center"
              ),
              style = "min-height: 300px;",
              shiny$tags$div(
                class = "text-center",
                shiny$tags$div(
                  class = "spinner-border text-primary mb-3",
                  role = "status"
                ),
                shiny$tags$p(
                  class = "text-muted",
                  "Running statistical tests..."
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
            plot_key <- resolve_plot_key(m)
            has_plot <- !is.null(plots) &&
              plot_key %in% names(plots)

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
                shiny$tags$div(
                  class = "d-flex align-items-center gap-2",
                  shiny$tags$a(
                    id = ns(
                      paste0("dl_report_", safe_id)
                    ),
                    class = "shiny-download-link",
                    href = "",
                    target = "_blank",
                    download = NA,
                    title = "Download HTML Report",
                    bsicons$bs_icon(
                      "file-earmark-arrow-down",
                      size = "1.2em"
                    )
                  ),
                  shiny$tags$span(
                    class = "badge bg-secondary",
                    paste0(
                      length(results$x_axis),
                      "-way"
                    )
                  )
                )
              ),
              bslib$card_body(
                class = "p-2 plot-card-body",
                plot_ui,
                # NA removal hint (if any rows were dropped)
                if (
                  !is.null(results$na_details) &&
                  m %in% names(results$na_details) &&
                  results$na_details[[m]]$total > 0
                ) {
                  na_info <- results$na_details[[m]]
                  grp_rows <- if (
                    !is.null(na_info$groups) &&
                    nrow(na_info$groups) > 0
                  ) {
                    lapply(
                      seq_len(nrow(na_info$groups)),
                      function(r) {
                        row <- na_info$groups[r, ]
                        # Build label from factor columns
                        fac_cols <- setdiff(
                          names(row), ".na"
                        )
                        label <- paste(
                          vapply(fac_cols, function(fc) {
                            paste0(fc, " = ", row[[fc]])
                          }, character(1)),
                          collapse = ", "
                        )
                        shiny$tags$li(
                          shiny$tags$span(
                            label
                          ),
                          shiny$tags$span(
                            class = paste(
                              "badge bg-warning",
                              "text-dark ms-2"
                            ),
                            paste0(
                              row$.na, " missing"
                            )
                          )
                        )
                      }
                    )
                  }
                  shiny$tags$div(
                    class = paste(
                      "alert alert-warning",
                      "py-2 px-3 mb-2"
                    ),
                    shiny$tags$details(
                      shiny$tags$summary(
                        style = "cursor: pointer;",
                        shiny$tags$small(
                          bsicons$bs_icon(
                            "exclamation-triangle",
                            class = "me-1"
                          ),
                          paste0(
                            na_info$total,
                            " observation(s) with",
                            " missing values were",
                            " excluded from the",
                            " statistical analysis."
                          )
                        )
                      ),
                      shiny$tags$ul(
                        class = paste(
                          "list-unstyled mb-0",
                          "mt-2 ms-3 small"
                        ),
                        grp_rows
                      )
                    )
                  )
                },
                # Omnibus test results
                render_omnibus_result(
                  results$omnibus[[m]],
                  results$x_axis,
                  results$params$test_approach
                ),
                # Post-hoc pairwise comparisons
                if (
                  !is.null(results$posthoc) &&
                  m %in% names(results$posthoc)
                ) {
                  render_posthoc_result(
                    results$posthoc[[m]],
                    results$x_axis,
                    results$params
                  )
                }
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
