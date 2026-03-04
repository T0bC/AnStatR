box::use(
  bsicons,
  bslib,
  ggplot2,
  ggiraph,
  openxlsx,
  rhino,
  shiny,
)

box::use(
  app/logic/data_utils,
  app/logic/error_handling,
  app/logic/plotting/assumption_checks,
  app/logic/plotting/data_processing,
  app/logic/plotting/normalize,
  app/logic/plotting/scatter,
  app/view/components/sidebar_tabs,
  app/view/error_display,
  app/view/plotting/data_selection,
  app/view/plotting/filter,
  app/view/plotting/processing,
  app/view/plotting/style,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$tagList(
  shiny$tags$script(shiny$HTML(paste0(
    "(function(){",
    "  var unlockTimer = null;",
    "  function lockSidebar(){",
    "    if(unlockTimer){ clearTimeout(unlockTimer); unlockTimer=null; }",
    "    var sb = $('.texan-sidebar');",
    "    sb.find('input,select,button').not('.selectize-input input')",
    "      .addClass('texan-busy-lock')",
    "      .css('pointer-events','none');",
    "    sb.find('.selectize-input').each(function(){",
    "      var $si = $(this);",
    "      var isOpen = $si.closest('.selectize-control')",
    "                       .hasClass('dropdown-active');",
    "      if(!isOpen){",
    "        $si.addClass('texan-busy-lock')",
    "          .css({'pointer-events':'none','opacity':'0.6'});",
    "      }",
    "    });",
    "  }",
    "  function unlockSidebar(){",
    "    $('.texan-sidebar').find('.texan-busy-lock')",
    "      .removeClass('texan-busy-lock')",
    "      .css({'pointer-events':'','opacity':''});",
    "  }",
    "  $(document).on('shiny:busy', function(){ lockSidebar(); });",
    "  $(document).on('shiny:idle', function(){",
    "    if(unlockTimer) clearTimeout(unlockTimer);",
    "    unlockTimer = setTimeout(unlockSidebar, 150);",
    "  });",
    "})();"
  ))),
  sidebar_tabs$tab_layout(
    ns = ns,
    sidebar_id = "sidebar_tabs",
    tabs = list(
      data_selection$tab_ui(ns),
      filter$tab_ui(ns),
      processing$tab_ui(ns),
      style$tab_ui(ns)
    ),
    main_content = shiny$tags$div(
      class = "scrollable-content",
      shiny$uiOutput(ns("main_content"))
    ),
    action_button = shiny$downloadButton(
      outputId = ns("downloadData"),
      label = "Download Filtered Data",
      class = "btn-primary btn-sm w-100"
    ),
    enable_responsive_plots = TRUE,
    results_id = "main_content"
  )
  )
}

#' @export
server <- function(id, input_data, data_version) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    last_error <- shiny$reactiveVal(NULL)
    cached_plot_params <- shiny$reactiveVal(NULL)
    cached_filtered_data <- shiny$reactiveVal(NULL)
    plot_cache <- shiny$reactiveVal(list())

    # Cache window size from JS (px), with sensible defaults
    window_size <- shiny$reactiveVal(
      list(width = 800, height = 400)
    )
    shiny$observe({
      ws <- input$windowSize
      if (!is.null(ws) && !is.null(ws$width)) {
        window_size(ws)
        rhino$log$debug(
          "Plotting: window size {ws$width}x{ws$height} px"
        )
      }
    })

    # Reset state when new data is loaded
    shiny$observeEvent(data_version(), {
      rhino$log$info("Plotting: state reset for new data")
      last_error(NULL)
      plot_cache(list())
      cached_plot_params(NULL)
      cached_filtered_data(NULL)
    }, ignoreInit = TRUE)

    # Delegate to sub-module servers
    data_selection$tab_server(
      input, output, session, input_data, data_version
    )
    filter_result <- filter$tab_server(
      input, output, session, input_data, data_version
    )
    processing$tab_server(
      input, output, session, data_version
    )
    style_result <- style$tab_server(
      input, output, session, filter_result$filtered_data, data_version
    )

    # --- Collect style inputs into a reactive list ---
    plot_params_raw <- shiny$reactive({
      # Gate: need at least xAxis selected
      x_axis <- input$xAxis
      shiny$req(x_axis, length(x_axis) > 0)

      # Gate: need color_map ready (fires after xAxis is set)
      cmap <- style_result$color_map()
      shiny$req(!is.null(cmap))

      m <- input$measureVar
      measure <- if (is.null(m) || length(m) == 0) character(0) else m

      grid_opts <- input$gridOptions
      stat_opts <- input$statOptions

      list(
        x_cols        = x_axis,
        measure_cols  = measure,
        tooltip_cols  = input$tooltip,
        color_cols    = input$pointColor,
        color_map     = cmap,
        point_style   = list(
          size       = input$pointSize   %||% 4,
          spread     = input$pointSpread %||% 0.15,
          alpha      = input$transparency %||% 0.6,
          shape_cols = input$pointShape
        ),
        processing    = list(
          trim_percent      = input$trim_slider %||% 0,
          outlier_enabled   = input$enableOutlierDetection %||% FALSE,
          outlier_method    = input$detectOutlier %||% "IQR",
          outlier_factor    = if ((input$detectOutlier %||% "IQR") %in%
            c("kde", "isolation_forest", "lof")) {
            input$probabilityFactor %||% 0.05
          } else {
            input$standardFactor %||% 1.5
          },
          bootstrap_samples = input$bootstrapSamples %||% 1000,
          normalize_enabled   = input$enableNormalize %||% FALSE,
          normalize_threshold = (input$normalizeThreshold %||% 50) / 100,
          show_transformed    = input$showTransformed %||% FALSE
        ),
        grid_legend   = list(
          legend_position   = input$legendPosition %||% "none",
          h_grid            = "hGrid" %in% grid_opts,
          v_grid            = "vGrid" %in% grid_opts,
          top_right_borders = "topRightBorders" %in% grid_opts,
          show_median       = "showMedian" %in% stat_opts,
          show_sd           = "showSD" %in% stat_opts,
          aspect_ratio      = "aspectRatio" %in% stat_opts
        ),
        stat_line_style = list(
          median_thickness = input$medianThickness %||% 0.5,
          median_width     = input$medianWidth     %||% 0.15,
          sd_thickness     = input$sdThickness     %||% 0.5,
          sd_width         = input$sdWidth         %||% 0.15
        ),
        axis_style    = list(
          tick_length    = input$axisTickLength      %||% 0.15,
          line_thickness = input$axisLineThickness   %||% 0.5
        )
      )
    })

    # Fingerprint-based debounce: collect all plot inputs into a
    # single string, debounce it, and only update cached_plot_params
    # when the fingerprint actually changes.  This lets the user
    # rapidly toggle filters / measure columns / style options and
    # only triggers one plot computation after everything settles.

    # Non-measure fingerprint: captures everything that affects a
    # plot EXCEPT which measurement columns are selected.  Two plots
    # with the same non-measure fingerprint and the same y_col
    # produce identical output, so the cached result can be reused.
    null_to_str <- function(x) {
      if (is.null(x)) "NULL" else paste(x, collapse = ",")
    }

    make_style_fingerprint <- function(params, data_nrow, data_ncol) {
      paste(
        null_to_str(params$x_cols),
        null_to_str(params$tooltip_cols),
        null_to_str(params$color_cols),
        null_to_str(params$color_map),
        params$point_style$size,
        params$point_style$spread,
        params$point_style$alpha,
        null_to_str(params$point_style$shape_cols),
        params$processing$trim_percent,
        params$processing$outlier_enabled,
        params$processing$outlier_method,
        params$processing$outlier_factor,
        params$processing$bootstrap_samples,
        params$processing$normalize_enabled,
        params$processing$normalize_threshold,
        params$processing$show_transformed,
        params$grid_legend$legend_position,
        params$grid_legend$h_grid,
        params$grid_legend$v_grid,
        params$grid_legend$top_right_borders,
        params$grid_legend$show_median,
        params$grid_legend$show_sd,
        params$grid_legend$aspect_ratio,
        params$stat_line_style$median_thickness,
        params$stat_line_style$median_width,
        params$stat_line_style$sd_thickness,
        params$stat_line_style$sd_width,
        params$axis_style$tick_length,
        params$axis_style$line_thickness,
        data_nrow, data_ncol,
        sep = "|"
      )
    }

    # Full fingerprint adds measure_cols for the debounce observer.
    make_plot_fingerprint <- function(params, data) {
      paste(
        null_to_str(params$measure_cols),
        make_style_fingerprint(params, nrow(data), ncol(data)),
        sep = "|"
      )
    }

    # Debounced reactive: collects params + data + fingerprint.
    # shiny$debounce() works on reactives, NOT observers.
    debounced_snapshot <- shiny$reactive({
      params <- plot_params_raw()
      data <- filter_result$filtered_data()
      shiny$req(params, data)
      list(
        params = params,
        data   = data,
        fp     = make_plot_fingerprint(params, data)
      )
    }) |> shiny$debounce(600)

    # Observer propagates debounced snapshot to cached values
    # only when the fingerprint actually changes.
    shiny$observe({
      snap <- debounced_snapshot()
      shiny$req(snap)
      current <- cached_plot_params()
      old_fp <- if (!is.null(current)) {
        make_plot_fingerprint(current, cached_filtered_data())
      } else {
        ""
      }
      if (snap$fp != old_fp) {
        cached_plot_params(snap$params)
        cached_filtered_data(snap$data)
      }
    })

    # Debounced accessors used by all downstream reactives
    plot_params <- shiny$reactive({ cached_plot_params() })
    debounced_filtered_data <- shiny$reactive({ cached_filtered_data() })

    # --- Build plots (one per measurement column) ---
    # All sidebar inputs are locked client-side via shiny:busy/idle
    # JS events (see UI) to prevent mid-flight input changes.
    plots <- shiny$reactive({
      params <- plot_params()
      show_transformed <- isTRUE(params$processing$show_transformed) &&
        isTRUE(params$processing$normalize_enabled) &&
        params$processing$trim_percent <= 0

      # Use processed data (with _normalized cols) when showing
      # transformed values; otherwise use filtered raw data
      data <- if (show_transformed) {
        processed_data()
      } else {
        debounced_filtered_data()
      }
      shiny$req(data, nrow(data) > 0)
      shiny$req(params$measure_cols, length(params$measure_cols) > 0)

      current_fp <- make_style_fingerprint(
        params, nrow(data), ncol(data)
      )
      cache <- plot_cache()
      new_cols <- character(0)
      reused_cols <- character(0)

      result <- lapply(params$measure_cols, function(y_col) {
        cached <- cache[[y_col]]
        if (!is.null(cached) && identical(cached$fp, current_fp)) {
          reused_cols <<- c(reused_cols, y_col)
          return(list(y_col = y_col, result = cached$result))
        }

        new_cols <<- c(new_cols, y_col)
        # Swap to _normalized column when showing transformed data
        plot_col <- if (show_transformed) {
          norm_col <- paste0(y_col, "_normalized")
          if (norm_col %in% names(data)) norm_col else y_col
        } else {
          y_col
        }

        error_handling$safe_execute(
          suppressWarnings(scatter$create_scatter_plot(
            data            = data,
            x_cols          = params$x_cols,
            y_col           = plot_col,
            color_map       = params$color_map,
            color_cols      = params$color_cols,
            tooltip_cols    = params$tooltip_cols,
            point_style     = params$point_style,
            processing      = params$processing,
            grid_legend     = params$grid_legend,
            stat_line_style = params$stat_line_style,
            axis_style      = params$axis_style
          )),
          operation_name = paste("Plot", y_col)
        ) -> exec_result

        list(y_col = y_col, result = exec_result)
      })

      # Update the cache: keep only current measure columns
      new_cache <- list()
      for (item in result) {
        new_cache[[item$y_col]] <- list(
          fp = current_fp,
          result = item$result
        )
      }
      plot_cache(new_cache)

      if (length(reused_cols) > 0) {
        rhino$log$info(
          "Plotting: reused {length(reused_cols)} cached plot(s): ",
          "{paste(reused_cols, collapse = ', ')}"
        )
      }
      if (length(new_cols) > 0) {
        rhino$log$info(
          "Plotting: rendered {length(new_cols)} new plot(s): ",
          "{paste(new_cols, collapse = ', ')}",
          " for x={paste(params$x_cols, collapse = ' | ')}",
          if (show_transformed) " [transformed]" else ""
        )
      }

      result
    })

    # --- Assumption diagnostics (per measurement column) ---
    diagnostics <- shiny$reactive({
      pd <- processed_data()
      shiny$req(pd)
      params <- plot_params()
      shiny$req(params$measure_cols, length(params$measure_cols) > 0)
      shiny$req(params$x_cols, length(params$x_cols) > 0)

      # Build interaction term (same as in data_processing)
      interaction_term <- if (all(params$x_cols %in% names(pd))) {
        data_utils$create_interaction(pd, params$x_cols)
      } else {
        factor(rep("all", nrow(pd)))
      }

      threshold <- params$processing$normalize_threshold
      norm_enabled <- isTRUE(params$processing$normalize_enabled) &&
        params$processing$trim_percent <= 0
      transform_info <- attr(pd, "transform_info")

      lapply(params$measure_cols, function(col) {
        # Per-group normality on raw data
        norm_raw <- assumption_checks$check_normality(
          pd, col, interaction_term
        )
        # Residual-based normality on raw data
        resid_raw <- assumption_checks$check_normality_residuals(
          pd, col, interaction_term
        )
        levene_raw <- assumption_checks$check_homogeneity(
          pd, col, interaction_term
        )
        rec_raw <- assumption_checks$recommend_transformation(
          norm_raw, threshold
        )
        banner_raw <- assumption_checks$build_recommendation_banner(
          rec_raw, levene_raw
        )

        # If normalization was applied to this column, also check
        # the normalized values
        norm_col <- paste0(col, "_normalized")
        has_normalized <- norm_col %in% names(pd)

        norm_post <- NULL
        resid_post <- NULL
        levene_post <- NULL
        rec_post <- NULL
        banner_post <- NULL
        transform_label <- NULL

        if (has_normalized && norm_enabled) {
          norm_post <- assumption_checks$check_normality(
            pd, norm_col, interaction_term,
            outlier_col = paste0(col, "_outlier"),
            trimmed_col = paste0(col, "_trimmed")
          )
          resid_post <- assumption_checks$check_normality_residuals(
            pd, norm_col, interaction_term,
            outlier_col = paste0(col, "_outlier"),
            trimmed_col = paste0(col, "_trimmed")
          )
          levene_post <- assumption_checks$check_homogeneity(
            pd, norm_col, interaction_term,
            outlier_col = paste0(col, "_outlier"),
            trimmed_col = paste0(col, "_trimmed")
          )
          rec_post <- assumption_checks$recommend_transformation(
            norm_post, threshold
          )
          banner_post <- assumption_checks$build_recommendation_banner(
            rec_post, levene_post
          )
          transform_label <- normalize$get_transform_label(
            transform_info, col
          )
        }

        list(
          col             = col,
          normality_raw   = norm_raw,
          residuals_raw   = resid_raw,
          levene_raw      = levene_raw,
          recommendation  = rec_raw,
          banner          = banner_raw,
          normality_post  = norm_post,
          residuals_post  = resid_post,
          levene_post     = levene_post,
          recommendation_post = rec_post,
          banner_post     = banner_post,
          transform_label = transform_label,
          has_normalized  = has_normalized
        )
      })
    })

    # --- Main content: placeholder, error, or plot cards ---
    output$main_content <- shiny$renderUI({
      err <- last_error()
      if (error_handling$is_app_error(err)) {
        return(error_display$error_alert_structured(
          err, type = "danger"
        ))
      }

      # Before params are ready, show placeholder
      params <- plot_params()
      if (is.null(params)) {
        x_axis <- character(0)
        measure <- character(0)
      } else {
        x_axis <- params$x_cols
        measure <- params$measure_cols
      }
      if (length(x_axis) == 0 || length(measure) == 0) {
        return(shiny$tags$div(
          class = paste(
            "d-flex align-items-center",
            "justify-content-center"
          ),
          style = "min-height: 400px;",
          shiny$tags$div(
            class = "text-center text-muted",
            shiny$tags$h4("Plotting"),
            shiny$tags$p(
              "Select descriptive and measurement",
              " columns to get started."
            )
          )
        ))
      }

      # Render one ggiraph output per measurement column
      plot_cards <- lapply(measure, function(y_col) {
        safe_id <- make.names(y_col)
        output_id <- paste0("plot_", safe_id)
        dl_svg_id <- paste0("dl_svg_", safe_id)
        dl_png_id <- paste0("dl_png_", safe_id)

        diag_id <- paste0("diag_", safe_id)

        bslib$card(
          class = "mb-3 plot-card",
          bslib$card_header(
            class = paste(
              "py-2 d-flex justify-content-between",
              "align-items-center"
            ),
            shiny$tags$span(
              class = "fw-semibold", y_col
            ),
            shiny$tags$div(
              class = "d-flex gap-2",
              shiny$tags$a(
                id = ns(dl_svg_id),
                class = "shiny-download-link",
                href = "", target = "_blank",
                download = NA,
                title = "Download SVG",
                bsicons$bs_icon(
                  "filetype-svg", size = "1.2em"
                )
              ),
              shiny$tags$a(
                id = ns(dl_png_id),
                class = "shiny-download-link",
                href = "", target = "_blank",
                download = NA,
                title = "Download PNG",
                bsicons$bs_icon(
                  "filetype-png", size = "1.2em"
                )
              )
            )
          ),
          bslib$card_body(
            class = "p-2 plot-card-body",
            shiny$tags$div(
              class = "responsive-plot",
              ggiraph$girafeOutput(
                ns(output_id),
                height = "auto",
                width = "100%"
              )
            )
          ),
          bslib$card_footer(
            class = "p-2",
            shiny$uiOutput(ns(diag_id))
          )
        )
      })

      do.call(shiny$tagList, plot_cards)
    })

    # --- Render ggiraph outputs + download handlers dynamically ---
    shiny$observe({
      plot_list <- plots()
      shiny$req(plot_list)

      lapply(plot_list, function(item) {
        local({
          local_item <- item
          safe_id <- make.names(local_item$y_col)
          output_id <- paste0("plot_", safe_id)
          dl_svg_id <- paste0("dl_svg_", safe_id)
          dl_png_id <- paste0("dl_png_", safe_id)

          # Helper: get the ggplot object for this measurement
          get_plot <- function() {
            res <- local_item$result
            if (!res$success) return(NULL)
            res$result
          }

          # Render interactive plot (responsive SVG sizing)
          output[[output_id]] <- ggiraph$renderGirafe({
            res <- local_item$result
            if (!res$success) {
              last_error(res$error)
              return(NULL)
            }
            last_error(NULL)

            # ~35% of viewport height in inches (96 dpi)
            ws <- window_size()
            w_svg <- max(4, ws$width / 100)
            h_svg <- max(3.5, (ws$height * 0.35) / 96)

            ggiraph$girafe(
              ggobj = res$result,
              width_svg = w_svg,
              height_svg = h_svg,
              options = list(
                ggiraph$opts_sizing(
                  rescale = FALSE
                ),
                ggiraph$opts_hover(
                  css = "fill-opacity:1;stroke-width:2;"
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
                ggiraph$opts_selection(type = "none")
              )
            )
          })

          # SVG download handler
          output[[dl_svg_id]] <- shiny$downloadHandler(
            filename = function() {
              paste0(local_item$y_col, "_", Sys.Date(), ".svg")
            },
            content = function(file) {
              p <- get_plot()
              shiny$req(p)
              w <- (input$exportWidth %||% 16) / 2.54
              h <- (input$exportHeight %||% 10) / 2.54
              ggplot2$ggsave(
                file, plot = p, device = "svg",
                width = w, height = h
              )
              rhino$log$info(
                "Download: SVG '{local_item$y_col}'"
              )
            }
          )

          # PNG download handler
          output[[dl_png_id]] <- shiny$downloadHandler(
            filename = function() {
              paste0(local_item$y_col, "_", Sys.Date(), ".png")
            },
            content = function(file) {
              p <- get_plot()
              shiny$req(p)
              w <- (input$exportWidth %||% 16) / 2.54
              h <- (input$exportHeight %||% 10) / 2.54
              ggplot2$ggsave(
                file, plot = p, device = "png",
                width = w, height = h, dpi = 300
              )
              rhino$log$info(
                "Download: PNG '{local_item$y_col}'"
              )
            }
          )
        })
      })
    })

    # --- Render diagnostics tables under each plot card ---
    shiny$observe({
      diag_list <- diagnostics()
      shiny$req(diag_list)

      lapply(diag_list, function(diag) {
        local({
          local_diag <- diag
          safe_id <- make.names(local_diag$col)
          diag_id <- paste0("diag_", safe_id)

          output[[diag_id]] <- shiny$renderUI({
            build_diagnostics_ui(local_diag)
          })
        })
      })
    })

    # --- Processed data: filtered + outlier/trim flag columns ---
    # This reactive is consumed by downstream modules (Summary,
    # Statistics) so they respect the same outlier/trim settings.
    processed_data <- shiny$reactive({
      params <- plot_params()
      data <- debounced_filtered_data()
      shiny$req(data, nrow(data) > 0)
      shiny$req(params$measure_cols, length(params$measure_cols) > 0)

      data_processing$process_data(
        data         = data,
        measure_cols = params$measure_cols,
        x_cols       = params$x_cols,
        trim_percent = params$processing$trim_percent,
        outlier_options = list(
          enabled           = params$processing$outlier_enabled,
          method            = params$processing$outlier_method,
          factor            = params$processing$outlier_factor,
          bootstrap_samples = params$processing$bootstrap_samples
        ),
        normalize_options = list(
          enabled   = params$processing$normalize_enabled,
          threshold = params$processing$normalize_threshold
        )
      )
    })

    # --- Download handler: filtered data with processing columns ---
    output$downloadData <- shiny$downloadHandler(
      filename = function() {
        x_cols <- input$xAxis
        x_suffix <- if (!is.null(x_cols) && length(x_cols) > 0) {
          paste0("_", paste(x_cols, collapse = "-"))
        } else {
          ""
        }
        paste0("filtered_data_", Sys.Date(), x_suffix, ".xlsx")
      },
      content = function(file) {
        data <- filter_result$filtered_data()
        if (is.null(data) || nrow(data) == 0) {
          wb <- openxlsx$createWorkbook()
          openxlsx$addWorksheet(wb, "No Data")
          openxlsx$writeData(wb, "No Data", "No filtered data available.")
          openxlsx$saveWorkbook(wb, file, overwrite = TRUE)
          return()
        }

        export_data <- processed_data()
        shiny$req(export_data)

        openxlsx$write.xlsx(export_data, file, rowNames = FALSE)
        rhino$log$info("Download: filtered data ({nrow(export_data)} rows)")
      }
    )

    # Return selections for downstream modules (e.g. Summary, Statistics)
    list(
      x_axis = shiny$reactive({ input$xAxis }),
      measure_cols = shiny$reactive({
        p <- plot_params()
        if (is.null(p)) character(0) else p$measure_cols
      }),
      trim_percent = shiny$reactive({ input$trim_slider %||% 0 }),
      processed_data = processed_data,
      normalize_enabled = shiny$reactive({
        isTRUE(input$enableNormalize) &&
          (input$trim_slider %||% 0) <= 0
      }),
      transform_info = shiny$reactive({
        pd <- processed_data()
        if (is.null(pd)) return(NULL)
        attr(pd, "transform_info")
      }),
      plot_objects = shiny$reactive({
        pl <- plots()
        if (is.null(pl)) return(NULL)
        # Named list: measure_col -> ggplot object
        result <- list()
        for (item in pl) {
          if (item$result$success) {
            result[[item$y_col]] <- item$result$result
          }
        }
        if (length(result) == 0) NULL else result
      })
    )
  })
}


# =============================================================================
# Internal helpers — diagnostics UI rendering
# =============================================================================

#' Build the diagnostics UI for one measurement column
#'
#' Creates a pivoted HTML table (groups as columns) with Shapiro-Wilk
#' results, Levene's test footer, and a recommendation banner.
#'
#' @param diag List from the diagnostics reactive
#' @return shiny tagList
build_diagnostics_ui <- function(diag) {
  # =====================================================================
  # 1. Build the BANNER (always visible compact summary)
  # =====================================================================
  has_post <- isTRUE(diag$has_normalized) &&
    !is.null(diag$recommendation_post)

  banner_ui <- if (has_post) {
    build_comparison_banner(diag)
  } else {
    build_raw_banner(diag)
  }

  # Compact test summary lines (residuals + Levene's)
  # Use post-transformation results when available
  resid <- if (has_post) diag$residuals_post else diag$residuals_raw
  levene <- if (has_post) diag$levene_post else diag$levene_raw

  summary_lines <- shiny$tags$div(
    class = "mt-1",
    build_residuals_line(resid, label_prefix = "Residuals"),
    build_levene_line(levene)
  )

  # =====================================================================
  # 2. Build the DETAIL tables (hidden in collapsible)
  # =====================================================================
  detail_elements <- list()

  # Raw per-group table
  detail_elements[[length(detail_elements) + 1]] <-
    build_shapiro_table(
      diag$normality_raw,
      "Shapiro-Wilk Normality Test (per group)"
    )
  detail_elements[[length(detail_elements) + 1]] <-
    build_residuals_line(
      diag$residuals_raw, label_prefix = "Residuals"
    )
  detail_elements[[length(detail_elements) + 1]] <-
    build_levene_line(diag$levene_raw)

  # Post-transformation tables (if applicable)
  if (isTRUE(diag$has_normalized) &&
      !is.null(diag$normality_post)) {
    label <- if (!is.null(diag$transform_label)) {
      paste0("After Transformation (", diag$transform_label, ")")
    } else {
      "After Transformation"
    }
    detail_elements[[length(detail_elements) + 1]] <-
      shiny$tags$hr(class = "my-1")
    detail_elements[[length(detail_elements) + 1]] <-
      build_shapiro_table(diag$normality_post, label)
    detail_elements[[length(detail_elements) + 1]] <-
      build_residuals_line(
        diag$residuals_post, label_prefix = "Residuals"
      )
    detail_elements[[length(detail_elements) + 1]] <-
      build_levene_line(diag$levene_post)
  }

  # =====================================================================
  # 3. Assemble: banner + summary lines + collapsible details
  # =====================================================================
  shiny$tagList(
    banner_ui,
    summary_lines,
    shiny$tags$details(
      class = "mt-2 small",
      shiny$tags$summary(
        class = "text-muted",
        style = "cursor: pointer;",
        "Show detailed test results"
      ),
      shiny$tags$div(
        class = "mt-1 ps-1",
        do.call(shiny$tagList, detail_elements)
      )
    )
  )
}


#' Build comparison banner (when normalization was applied)
#'
#' Uses residual normality as the primary ANOVA criterion.
#' Per-group results are secondary context.
#'
#' @param diag Diagnostics list
#' @return shiny tag
build_comparison_banner <- function(diag) {
  n_bad_before <- diag$recommendation$n_non_normal
  n_bad_after  <- diag$recommendation_post$n_non_normal
  n_groups     <- diag$recommendation_post$n_groups

  resid_before <- diag$residuals_raw
  resid_after  <- diag$residuals_post

  resid_normal_before <- !is.na(resid_before$normal) &&
    resid_before$normal == "yes"
  resid_normal_after <- !is.null(resid_after) &&
    !is.na(resid_after$normal) && resid_after$normal == "yes"

  # Primary criterion: residual normality
  if (resid_normal_after) {
    # Residuals are normal after transformation → ANOVA OK
    if (n_bad_after > 0) {
      text <- paste0(
        "Model residuals are normally distributed after ",
        "transformation \u2014 classical ANOVA is valid. ",
        "Per-group: ", n_bad_after, "/", n_groups,
        " groups non-normal (see details)."
      )
    } else {
      text <- paste0(
        "Transformation achieved normality ",
        "(residuals + all ", n_groups, " groups). ",
        "Classical ANOVA is valid."
      )
    }
    css <- "alert-success"
    icon <- "check-circle-fill"
  } else if (!resid_normal_after && resid_normal_before) {
    # Residuals were normal before but not after → worsened
    text <- paste0(
      "Transformation worsened residual normality. ",
      "Model residuals were normal before transformation ",
      "but are non-normal after. ",
      "Consider keeping raw data for this variable."
    )
    css <- "alert-danger"
    icon <- "x-circle-fill"
  } else if (!resid_normal_after && !resid_normal_before) {
    # Residuals non-normal before and after
    grp_improved <- n_bad_after < n_bad_before
    if (grp_improved) {
      text <- paste0(
        "Transformation improved per-group normality (",
        n_bad_before, " \u2192 ", n_bad_after,
        " non-normal groups) but residuals remain ",
        "non-normal. Consider robust/non-parametric tests."
      )
      css <- "alert-warning"
      icon <- "exclamation-triangle-fill"
    } else {
      text <- paste0(
        "Residuals remain non-normal after transformation. ",
        "Per-group: ", n_bad_after, "/", n_groups,
        " non-normal. ",
        "Consider robust or non-parametric tests."
      )
      css <- "alert-warning"
      icon <- "exclamation-triangle-fill"
    }
  } else {
    # Fallback
    text <- paste0(
      "Per-group: ", n_bad_after, "/", n_groups,
      " non-normal after transformation."
    )
    css <- "alert-info"
    icon <- "info-circle-fill"
  }

  shiny$tags$div(
    class = paste("alert", css, "py-1 px-2 small mb-0"),
    shiny$tags$div(
      bsicons$bs_icon(icon, class = "me-1"),
      text
    )
  )
}


#' Build raw recommendation banner (no normalization)
#'
#' Uses residual normality as the primary ANOVA criterion.
#' Per-group results provide supplementary context.
#'
#' @param diag Diagnostics list
#' @return shiny tag
build_raw_banner <- function(diag) {
  rec <- diag$recommendation
  resid <- diag$residuals_raw
  n_groups <- rec$n_groups
  n_bad <- rec$n_non_normal

  resid_normal <- !is.null(resid) && !is.na(resid$normal) &&
    resid$normal == "yes"

  if (resid_normal && n_bad == 0) {
    # Everything normal
    text <- paste0(
      "Assumptions met: residuals and all ", n_groups,
      " groups are normally distributed. ",
      "Classical ANOVA is valid."
    )
    css <- "alert-success"
    icon <- "check-circle-fill"
  } else if (resid_normal && n_bad > 0) {
    # Residuals OK but some groups flagged
    text <- paste0(
      "Model residuals are normally distributed ",
      "\u2014 classical ANOVA is valid. ",
      "Per-group: ", n_bad, "/", n_groups,
      " groups non-normal (see details)."
    )
    css <- "alert-success"
    icon <- "check-circle-fill"
  } else if (!resid_normal && n_bad == 0) {
    # Groups OK individually but residuals flagged
    text <- paste0(
      "Per-group normality OK, but model residuals ",
      "are non-normal. ANOVA may still be robust for ",
      "balanced designs. Consider checking QQ-plot."
    )
    css <- "alert-warning"
    icon <- "exclamation-triangle-fill"
  } else {
    # Both non-normal
    text <- paste0(
      n_bad, "/", n_groups,
      " groups non-normal. ",
      "Model residuals are also non-normal. ",
      "Enable 'Normalize data' in Data Processing to attempt ",
      "transformation, or use robust/non-parametric tests."
    )
    css <- "alert-danger"
    icon <- "x-circle-fill"
  }

  shiny$tags$div(
    class = paste0("alert ", css, " py-1 px-2 small mb-0"),
    shiny$tags$div(
      bsicons$bs_icon(icon, class = "me-1"),
      text
    )
  )
}


#' Build a pivoted Shapiro-Wilk HTML table
#'
#' Groups as columns, statistics (n, W, p, status) as rows.
#'
#' @param norm_df Data frame from check_normality()
#' @param title Character, table heading
#' @return shiny tag (HTML table)
build_shapiro_table <- function(norm_df, title) {
  if (is.null(norm_df) || nrow(norm_df) == 0) {
    return(shiny$tags$p(
      class = "text-muted small fst-italic",
      "No normality data available."
    ))
  }

  groups <- norm_df$group

  # Header row: empty cell + group names
  header_cells <- c(
    list(shiny$tags$th("")),
    lapply(groups, function(g) {
      shiny$tags$th(class = "text-center px-2", g)
    })
  )

  # n row
  n_cells <- c(
    list(shiny$tags$td(
      class = "fw-semibold text-muted", "n"
    )),
    lapply(norm_df$n, function(v) {
      shiny$tags$td(class = "text-center px-2", v)
    })
  )

  # W row
  w_cells <- c(
    list(shiny$tags$td(
      class = "fw-semibold text-muted", "W"
    )),
    lapply(norm_df$W, function(v) {
      shiny$tags$td(
        class = "text-center px-2",
        if (is.na(v)) "—" else format(round(v, 3), nsmall = 3)
      )
    })
  )

  # p row
  p_cells <- c(
    list(shiny$tags$td(
      class = "fw-semibold text-muted", "p"
    )),
    lapply(norm_df$p_value, function(v) {
      shiny$tags$td(
        class = "text-center px-2",
        assumption_checks$format_p(v)
      )
    })
  )

  # Status row (checkmark or X)
  status_cells <- c(
    list(shiny$tags$td(
      class = "fw-semibold text-muted", ""
    )),
    lapply(norm_df$normal, function(v) {
      if (is.na(v) || v == "identical values") {
        icon <- bsicons$bs_icon(
          "dash-circle", class = "text-muted"
        )
      } else if (v == "yes") {
        icon <- bsicons$bs_icon(
          "check-circle-fill", class = "text-success"
        )
      } else {
        icon <- bsicons$bs_icon(
          "x-circle-fill", class = "text-danger"
        )
      }
      shiny$tags$td(class = "text-center px-2", icon)
    })
  )

  shiny$tags$div(
    shiny$tags$div(
      class = "small fw-semibold text-muted mb-1", title
    ),
    shiny$tags$table(
      class = "table table-sm table-borderless mb-1 small",
      style = "font-size: 0.8rem;",
      shiny$tags$thead(
        do.call(shiny$tags$tr, header_cells)
      ),
      shiny$tags$tbody(
        do.call(shiny$tags$tr, n_cells),
        do.call(shiny$tags$tr, w_cells),
        do.call(shiny$tags$tr, p_cells),
        do.call(shiny$tags$tr, status_cells)
      )
    )
  )
}


#' Build Levene's test result line
#'
#' @param levene List from check_homogeneity()
#' @return shiny tag
build_levene_line <- function(levene) {
  if (is.null(levene) || is.na(levene$p_value)) {
    return(shiny$tags$div(
      class = "small text-muted fst-italic",
      "Levene's test: insufficient data."
    ))
  }

  icon <- if (levene$equal_variances == "yes") {
    bsicons$bs_icon(
      "check-circle-fill", class = "text-success me-1"
    )
  } else {
    bsicons$bs_icon(
      "x-circle-fill", class = "text-danger me-1"
    )
  }

  label <- if (levene$equal_variances == "yes") {
    "Equal variances"
  } else {
    "Unequal variances"
  }

  shiny$tags$div(
    class = "small text-muted",
    icon,
    paste0(
      "Levene's: F(", levene$df1, ", ", levene$df2, ") = ",
      format(round(levene$F_statistic, 2), nsmall = 2),
      ", p = ", assumption_checks$format_p(levene$p_value),
      "  \u2014 ", label
    )
  )
}


#' Build residual-based normality test result line
#'
#' @param resid_result List from check_normality_residuals()
#' @param label_prefix Character, prefix for the label (default "Residuals")
#' @return shiny tag
build_residuals_line <- function(resid_result,
                                 label_prefix = "Residuals") {
  if (is.null(resid_result) || is.na(resid_result$p_value)) {
    return(shiny$tags$div(
      class = "small text-muted fst-italic",
      paste0(label_prefix, " (model-based): insufficient data.")
    ))
  }

  icon <- if (resid_result$normal == "yes") {
    bsicons$bs_icon(
      "check-circle-fill", class = "text-success me-1"
    )
  } else {
    bsicons$bs_icon(
      "x-circle-fill", class = "text-danger me-1"
    )
  }

  verdict <- if (resid_result$normal == "yes") {
    "normal"
  } else {
    "non-normal"
  }

  shiny$tags$div(
    class = "small text-muted",
    icon,
    paste0(
      label_prefix, " (model-based): W = ",
      format(round(resid_result$W, 3), nsmall = 3),
      ", p = ", assumption_checks$format_p(resid_result$p_value),
      ", n = ", resid_result$n,
      "  \u2014 ", verdict
    )
  )
}
