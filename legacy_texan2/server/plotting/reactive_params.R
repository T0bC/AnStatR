#' Reactive Parameters Component
#'
#' Creates all plot-related reactives: selections, styling options, and the
#' consolidated plot_params reactive with caching and debouncing.
#'
#' Following the explicit dependency injection pattern:
#' - All dependencies are passed as explicit parameters
#' - No implicit scoping or global state access
#'
#' @name reactive_params
NULL

# Import column utilities
box::use(../../utils/column_utils)


#' Create Column Reactives
#'
#' Creates reactives for descriptive and measurement columns.
#'
#' @param median_data Reactive containing the median-processed data
#' @return List with descriptive_cols and measurement_cols reactives
#' @export
create_column_reactives <- function(median_data) {
    list(
        descriptive_cols = shiny::reactive({
            shiny::req(median_data())
            column_utils$get_descriptive_cols(median_data())
        }),
        measurement_cols = shiny::reactive({
            shiny::req(median_data())
            column_utils$get_measurement_cols(median_data())
        })
    )
}


#' Create Selection Reactives
#'
#' Creates reactives for user selections (measures, x-axis, tooltip, colors).
#'
#' @param input Shiny input object from parent module
#'   - input$measureVar: Selected measurement columns
#'   - input$xAxis: Selected X-axis columns
#'   - input$tooltip: Selected tooltip columns
#'   - input$pointColor: Selected color columns
#' @return List of selection reactives
#' @export
create_selection_reactives <- function(input) {
    list(
        measures = shiny::reactive({ input$measureVar }),
        x_axis = shiny::reactive({ input$xAxis }),
        tooltip_cols = shiny::reactive({ input$tooltip }),
        color_cols = shiny::reactive({
            color_cols <- input$pointColor
            if (is.null(color_cols) || length(color_cols) == 0) {
                return(input$xAxis)
            }
            color_cols
        })
    )
}


#' Create Window Size Reactive
#'
#' Creates a cached reactive for window size from JS.
#'
#' @param input Shiny input object from parent module
#'   - input$windowSize: Window dimensions from plot_resize.js
#' @param debug Logical, enable debug logging
#' @return Reactive returning list(width, height)
#' @export
create_window_size_reactive <- function(input, debug = FALSE) {
    cached_window_size <- shiny::reactiveVal(list(width = 800, height = 600))
    
    shiny::observe({
        ws <- input$windowSize
        if (!is.null(ws)) {
            current <- cached_window_size()
            if (is.null(current) || ws$width != current$width || ws$height != current$height) {
                if (debug) {
                    message(paste0("[", format(Sys.time(), "%H:%M:%S"), "] window_size CHANGED: ", 
                                  ws$width, "x", ws$height))
                }
                cached_window_size(ws)
            }
        }
    })
    
    shiny::reactive({ cached_window_size() })
}


#' Create Export Dimension Reactives
#'
#' @param input Shiny input object from parent module
#'   - input$exportWidth: Export width in cm
#'   - input$exportHeight: Export height in cm
#' @return List with export_width and export_height reactives
#' @export
create_export_dimension_reactives <- function(input) {
    list(
        export_width = shiny::reactive({ input$exportWidth %||% 16 }),
        export_height = shiny::reactive({ input$exportHeight %||% 10 })
    )
}


#' Create Processing Option Reactives
#'
#' Creates reactives for trim percentage and outlier detection options.
#'
#' @param input Shiny input object from parent module
#'   - input$trim_slider: Trim percentage
#'   - input$enableOutlierDetection: Enable outlier detection
#'   - input$detectOutlier: Outlier detection method
#'   - input$standardFactor: Factor for IQR/SD methods
#'   - input$probabilityFactor: Factor for probability methods
#'   - input$bootstrapSamples: Number of bootstrap samples
#' @return List with trim_percent and outlier_options reactives
#' @export
create_processing_option_reactives <- function(input) {
    list(
        trim_percent = shiny::reactive({ input$trim_slider %||% 0 }),
        outlier_options = shiny::reactive({
            list(
                enabled = input$enableOutlierDetection %||% FALSE,
                method = input$detectOutlier %||% "IQR",
                factor = if (input$detectOutlier %in% c("kde", "isolation_forest", "lof")) {
                    input$probabilityFactor %||% 0.05
                } else {
                    input$standardFactor %||% 1.5
                },
                bootstrap_samples = input$bootstrapSamples %||% 1000
            )
        })
    )
}


#' Create Style Reactives
#'
#' Creates reactives for point, grid/legend, stat line, and axis styling.
#'
#' @param input Shiny input object from parent module
#'   - input$pointSize, input$pointSpread, input$transparency, input$pointShape
#'   - input$legendPosition, input$gridOptions, input$statOptions
#'   - input$medianThickness, input$medianWidth, input$sdThickness, input$sdWidth
#'   - input$axisTickLength, input$axisLineThickness
#' @return List of style reactives
#' @export
create_style_reactives <- function(input) {
    list(
        point_style = shiny::reactive({
            shape_cols <- input$pointShape
            if (length(shape_cols) == 0) shape_cols <- NULL
            
            list(
                size = input$pointSize %||% 4,
                spread = input$pointSpread %||% 0.15,
                alpha = input$transparency %||% 0.6,
                shape_cols = shape_cols
            )
        }),
        
        grid_legend_options = shiny::reactive({
            grid_opts <- input$gridOptions %||% character(0)
            stat_opts <- input$statOptions %||% character(0)
            list(
                legend_position = input$legendPosition %||% "none",
                h_grid = "hGrid" %in% grid_opts,
                v_grid = "vGrid" %in% grid_opts,
                top_right_borders = "topRightBorders" %in% grid_opts,
                show_median = "showMedian" %in% stat_opts,
                show_sd = "showSD" %in% stat_opts,
                aspect_ratio = "aspectRatio" %in% stat_opts
            )
        }),
        
        stat_line_style = shiny::reactive({
            list(
                median_thickness = input$medianThickness %||% 0.5,
                median_width = input$medianWidth %||% 0.15,
                sd_thickness = input$sdThickness %||% 0.5,
                sd_width = input$sdWidth %||% 0.15
            )
        }),
        
        axis_style = shiny::reactive({
            list(
                tick_length = input$axisTickLength %||% 0.15,
                line_thickness = input$axisLineThickness %||% 0.5
            )
        })
    )
}


#' Create Consolidated Plot Parameters
#'
#' Creates the consolidated plot_params reactive with caching and debouncing.
#' Bundles all plot-affecting reactives into a single reactive that only
#' updates when values actually change.
#'
#' @param filtered_data Reactive returning filtered data frame
#' @param selection_reactives List from create_selection_reactives
#' @param processing_reactives List from create_processing_option_reactives
#' @param style_reactives List from create_style_reactives
#' @param window_size Reactive returning window dimensions
#' @param custom_color_map Reactive returning named color vector
#' @param debug Logical, enable debug logging
#' @return Reactive returning consolidated plot parameters list
#' @export
create_plot_params <- function(filtered_data,
                                selection_reactives,
                                processing_reactives,
                                style_reactives,
                                window_size,
                                custom_color_map,
                                debug = FALSE) {
    
    # Cache for plot parameters
    cached_plot_params <- shiny::reactiveVal(NULL)
    
    # Helper to create a fingerprint for comparison
#' @export
    make_fingerprint <- function(params) {
        paste(
            nrow(params$data),
            paste(params$x_cols, collapse = ":"),
            paste(params$tooltip_cols, collapse = ":"),
            params$trim_percent,
            params$outlier_options$enabled,
            params$outlier_options$method,
            params$outlier_options$factor,
            paste(params$color_cols, collapse = ":"),
            paste(names(params$color_map), params$color_map, collapse = ":"),
            params$window_size$width,
            params$window_size$height,
            params$point_style$size,
            params$point_style$spread,
            params$point_style$alpha,
            paste(params$point_style$shape_cols %||% "none", collapse = ":"),
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
            sep = "|"
        )
    }
    
#' @export
    debug_log <- function(source, details = NULL) {
        if (debug) {
            timestamp <- format(Sys.time(), "%H:%M:%S.%OS3")
            msg <- paste0("[", timestamp, "] REACTIVE: ", source)
            if (!is.null(details)) {
                msg <- paste0(msg, " | ", details)
            }
            message(msg)
        }
    }
    
    # Observer that updates cache only when values change
    shiny::observe({
        data_val <- filtered_data()
        x_val <- selection_reactives$x_axis()
        tt_val <- selection_reactives$tooltip_cols()
        trim_val <- processing_reactives$trim_percent()
        outlier_val <- processing_reactives$outlier_options()
        color_cols_val <- selection_reactives$color_cols()
        color_map_val <- custom_color_map()
        win_val <- window_size()
        point_style_val <- style_reactives$point_style()
        grid_legend_val <- style_reactives$grid_legend_options()
        stat_line_val <- style_reactives$stat_line_style()
        axis_style_val <- style_reactives$axis_style()
        
        new_params <- list(
            data = data_val,
            x_cols = x_val,
            tooltip_cols = tt_val,
            trim_percent = trim_val,
            outlier_options = outlier_val,
            color_cols = color_cols_val,
            color_map = color_map_val,
            window_size = win_val,
            point_style = point_style_val,
            grid_legend = grid_legend_val,
            stat_line_style = stat_line_val,
            axis_style = axis_style_val
        )
        
        current <- cached_plot_params()
        new_fp <- make_fingerprint(new_params)
        old_fp <- if (!is.null(current)) make_fingerprint(current) else ""
        
        if (new_fp != old_fp) {
            debug_log("plot_params CHANGED", paste0(
                "data_rows=", nrow(data_val), 
                ", x_cols=", paste(x_val, collapse=","),
                ", trim=", trim_val,
                ", outlier_enabled=", outlier_val$enabled,
                ", color_map_len=", length(color_map_val),
                ", window_width=", win_val$width
            ))
            cached_plot_params(new_params)
        }
    }) |> shiny::debounce(350)
    
    # Expose cached params as reactive
    shiny::reactive({ cached_plot_params() })
}
