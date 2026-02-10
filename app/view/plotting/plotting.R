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
  app/logic/error_handling,
  app/logic/plotting/data_processing,
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

  sidebar_tabs$tab_layout(
    ns = ns,
    sidebar_id = "sidebar_tabs",
    tabs = list(
      data_selection$tab_ui(ns),
      filter$tab_ui(ns),
      processing$tab_ui(ns),
      style$tab_ui(ns)
    ),
    main_content = shiny$uiOutput(ns("main_content")),
    action_button = shiny$downloadButton(
      outputId = ns("downloadData"),
      label = "Download Filtered Data",
      class = "btn-primary btn-sm w-100"
    ),
    enable_responsive_plots = TRUE,
    results_id = "main_content"
  )
}

#' @export
server <- function(id, input_data, data_version) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    last_error <- shiny$reactiveVal(NULL)
    result <- shiny$reactiveVal(NULL)

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
      result(NULL)
      last_error(NULL)
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
      input, output, session, input_data, data_version
    )

    # --- Collect style inputs into a reactive list ---
    plot_params <- shiny$reactive({
      # Gate: need at least xAxis selected
      x_axis <- input$xAxis
      shiny$req(x_axis, length(x_axis) > 0)

      # Gate: need color_map ready (fires after xAxis is set)
      cmap <- style_result$color_map()
      shiny$req(!is.null(cmap))

      grid_opts <- input$gridOptions
      stat_opts <- input$statOptions

      list(
        x_cols        = x_axis,
        measure_cols  = input$measureVar,
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
          bootstrap_samples = input$bootstrapSamples %||% 1000
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

    # --- Build plots (one per measurement column) ---
    plots <- shiny$reactive({
      params <- plot_params()
      data <- filter_result$filtered_data()
      shiny$req(data, nrow(data) > 0)
      shiny$req(params$measure_cols, length(params$measure_cols) > 0)

      rhino$log$info(
        "Plotting: rendering {length(params$measure_cols)} plot(s) ",
        "for x={paste(params$x_cols, collapse = ' | ')}"
      )

      lapply(params$measure_cols, function(y_col) {
        exec_result <- error_handling$safe_execute(
          scatter$create_scatter_plot(
            data            = data,
            x_cols          = params$x_cols,
            y_col           = y_col,
            color_map       = params$color_map,
            color_cols      = params$color_cols,
            tooltip_cols    = params$tooltip_cols,
            point_style     = params$point_style,
            processing      = params$processing,
            grid_legend     = params$grid_legend,
            stat_line_style = params$stat_line_style,
            axis_style      = params$axis_style
          ),
          operation_name = paste("Plot", y_col)
        )
        list(y_col = y_col, result = exec_result)
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

      # Before xAxis is selected, show placeholder
      x_axis <- input$xAxis
      measure <- input$measureVar
      if (is.null(x_axis) || length(x_axis) == 0 ||
          is.null(measure) || length(measure) == 0) {
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

            # Convert container px to SVG inches (100 px/in)
            ws <- window_size()
            w_svg <- max(4, ws$width / 100)
            h_svg <- max(2.5, ws$height / 100)

            ggiraph$girafe(
              ggobj = res$result,
              width_svg = w_svg,
              height_svg = h_svg,
              options = list(
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

        params <- plot_params()
        export_data <- data_processing$process_data(
          data         = data,
          measure_cols = params$measure_cols,
          x_cols       = params$x_cols,
          trim_percent = params$processing$trim_percent,
          outlier_options = list(
            enabled           = params$processing$outlier_enabled,
            method            = params$processing$outlier_method,
            factor            = params$processing$outlier_factor,
            bootstrap_samples = params$processing$bootstrap_samples
          )
        )

        openxlsx$write.xlsx(export_data, file, rowNames = FALSE)
        rhino$log$info("Download: filtered data ({nrow(export_data)} rows)")
      }
    )

    # Return selections for downstream modules (e.g. Summary)
    list(
      x_axis = shiny$reactive({ input$xAxis }),
      measure_cols = shiny$reactive({ input$measureVar })
    )
  })
}
