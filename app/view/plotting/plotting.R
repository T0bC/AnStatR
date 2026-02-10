box::use(
  bsicons,
  bslib,
  ggiraph,
  shiny,
)

box::use(
  app/logic/error_handling,
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
    main_content = shiny$uiOutput(ns("main_content"))
  )
}

#' @export
server <- function(id, input_data, data_version) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    last_error <- shiny$reactiveVal(NULL)
    result <- shiny$reactiveVal(NULL)

    # Reset state when new data is loaded
    shiny$observeEvent(data_version(), {
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
        output_id <- paste0("plot_", make.names(y_col))
        bslib$card(
          bslib$card_header(y_col),
          bslib$card_body(
            ggiraph$girafeOutput(ns(output_id), height = "400px")
          )
        )
      })

      do.call(shiny$tagList, plot_cards)
    })

    # --- Render ggiraph outputs dynamically ---
    shiny$observe({
      plot_list <- plots()
      shiny$req(plot_list)

      lapply(plot_list, function(item) {
        output_id <- paste0("plot_", make.names(item$y_col))

        output[[output_id]] <- ggiraph$renderGirafe({
          res <- item$result
          if (!res$success) {
            last_error(res$error)
            return(NULL)
          }
          last_error(NULL)
          ggiraph$girafe(
            ggobj = res$result,
            width_svg = 8,
            height_svg = 5,
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
      })
    })

    # Return for downstream modules
    invisible(NULL)
  })
}
