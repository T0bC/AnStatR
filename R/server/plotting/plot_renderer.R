#' Plot Renderer Component
#'
#' Handles rendering of multiple interactive ggiraph scatter plots.
#' Each measurement variable gets its own plot card.
#'
#' Following the explicit dependency injection pattern:
#' - All dependencies (output, ns, reactives) are passed as explicit parameters
#' - No implicit scoping or global state access


#' Create Cached Plot Objects Reactive
#'
#' Creates a reactive that returns a named list of ggplot objects for each measurement.
#' Uses bindCache to cache plots based on all plot-affecting parameters.
#' This allows sharing plot objects between plotting and statistics tabs.
#'
#' @param plot_params Reactive returning consolidated list of plot parameters
#' @param measure_cols Reactive returning selected measurement column names
#' @param create_scatter_plot Function to create scatter plots (injected dependency)
#' @return Reactive returning named list of ggplot objects (keyed by measure name)
#' @export
create_cached_plot_objects <- function(plot_params, measure_cols, create_scatter_plot) {
    shiny::reactive({
        params <- plot_params()
        measures <- measure_cols()
        shiny::req(params$data, params$x_cols, measures)
        
        outlier_opts <- params$outlier_options %||% 
            list(enabled = FALSE, method = "IQR", factor = 1.5, bootstrap_samples = 1000)
        
        plots <- lapply(measures, function(measure) {
            create_scatter_plot(
                data = params$data,
                x_col = params$x_cols,
                y_col = measure,
                tooltip_cols = params$tooltip_cols,
                point_style = params$point_style,
                trim_percent = params$trim_percent %||% 0,
                outlier_detection = outlier_opts$enabled,
                outlier_method = outlier_opts$method,
                outlier_factor = outlier_opts$factor,
                bootstrap_samples = outlier_opts$bootstrap_samples,
                color_cols = params$color_cols,
                color_map = params$color_map,
                grid_legend = params$grid_legend,
                stat_line_style = params$stat_line_style,
                axis_style = params$axis_style
            )
        })
        names(plots) <- measures
        plots
    }) |> shiny::bindCache(
        measure_cols(),
        plot_params()$x_cols,
        plot_params()$trim_percent,
        plot_params()$outlier_options$enabled %||% FALSE,
        plot_params()$outlier_options$method %||% "IQR",
        plot_params()$outlier_options$factor %||% 1.5,
        plot_params()$color_cols,
        plot_params()$color_map,
        plot_params()$point_style,
        plot_params()$grid_legend,
        plot_params()$stat_line_style,
        plot_params()$axis_style,
        nrow(plot_params()$data)
    )
}


#' @param output Shiny output object from parent module
#' @param ns Namespace function from parent module (session$ns)
#' @param plot_params Reactive returning consolidated list of plot parameters:
#'   data, x_cols, tooltip_cols, trim_percent, outlier_options, color_cols, color_map, window_size
#' @param measure_cols Reactive returning selected measurement column names
#' @param create_scatter_plot Function to create scatter plots (injected dependency)
#' @param cached_plot_objects Reactive returning cached ggplot objects (from create_cached_plot_objects)
#' @param export_width Reactive returning export width in cm
#' @param export_height Reactive returning export height in cm
#' @return NULL (side effects only - registers plot outputs and download handlers)
#' @export
setup_plot_outputs <- function(output, 
                                ns, 
                                plot_params,
                                measure_cols,
                                create_scatter_plot,
                                cached_plot_objects,
                                export_width = NULL,
                                export_height = NULL) {
    
    # Track which outputs have been registered to avoid re-registration
    registered_outputs <- shiny::reactiveVal(character(0))
    
    # Debug flag from environment variable (set TEXAN_DEBUG_PLOT_RENDERER=true to enable)
    DEBUG_PLOT_RENDERER <- tolower(Sys.getenv("TEXAN_DEBUG_PLOT_RENDERER", "false")) == "true"
    
    # Register dynamic plot outputs for each measurement column
    # Only re-register when measure_cols actually changes (not on other reactive updates)
    shiny::observeEvent(measure_cols(), ignoreNULL = TRUE, {
        measures <- measure_cols()
        shiny::req(measures)
        
        if (DEBUG_PLOT_RENDERER) {
            message(paste0("[", format(Sys.time(), "%H:%M:%S"), 
                          "] observeEvent(measure_cols) TRIGGERED: ", 
                          paste(measures, collapse=", ")))
        }
        
        # Check which measures need new output registration
        already_registered <- registered_outputs()
        
        # Check if measures are identical (both content and length)
        if (setequal(measures, already_registered)) {
            if (DEBUG_PLOT_RENDERER) message("  -> No new measures, skipping re-registration")
            return()  # No changes needed
        }
        
        if (DEBUG_PLOT_RENDERER) {
            message(paste0("  -> Registering outputs for: ", paste(measures, collapse=", ")))
        }
        
        # Update registered list
        registered_outputs(measures)
        
        # Create a plot output for each measurement
        lapply(measures, function(measure) {
            # Use local() to ensure proper closure capture for each iteration
            local({
                # Capture measure in local scope
                local_measure <- measure
                # Create safe ID from measure name
                plot_id <- paste0("plot_", gsub("[^a-zA-Z0-9]", "_", local_measure))
                download_id <- paste0("download_", plot_id)
                
                # Helper function to get the cached ggplot (shared between render and download)
#' @export
                get_plot <- function() {
                    plots <- cached_plot_objects()
                    shiny::req(plots, local_measure %in% names(plots))
                    plots[[local_measure]]
                }
                
                # Register the girafe output for interactive plots
                # Uses cached_plot_objects for efficient rendering
                output[[plot_id]] <- ggiraph::renderGirafe({
                    # Debug logging (conditional)
                    if (DEBUG_PLOT_RENDERER) {
                        message(paste0("[", format(Sys.time(), "%H:%M:%S.%OS3"), 
                                      "] renderGirafe EXECUTING for: ", local_measure))
                    }
                    
                    # Get the cached ggplot object
                    p <- get_plot()
                    
                    # Get SVG dimensions from window size (measured by JS)
                    params <- plot_params()
                    win_size <- params$window_size
                    
                    # Get container width (JS measures main content area)
                    container_width <- if (!is.null(win_size) && !is.null(win_size$width) && win_size$width > 0) {
                        win_size$width
                    } else {
                        800  # Default fallback
                    }
                    
                    # Get container height (JS measures actual card body height)
                    container_height <- if (!is.null(win_size) && !is.null(win_size$height) && win_size$height > 0) {
                        win_size$height
                    } else {
                        400  # Default fallback
                    }
                    
                    # Convert pixels to SVG inches (100 pixels per inch)
                    width_svg <- container_width / 100
                    height_svg <- container_height / 100
                    
                    # Convert to girafe interactive plot
                    ggiraph::girafe(
                        ggobj = p,
                        width_svg = width_svg,
                        height_svg = height_svg,
                        options = list(
                            ggiraph::opts_zoom(max = 5),
                            ggiraph::opts_selection(type = "single"),
                            ggiraph::opts_hover(css = "fill:red;stroke:black;cursor:pointer;"),
                            ggiraph::opts_hover_inv(css = "opacity:0.5;"),
                            ggiraph::opts_tooltip(
                                css = "background-color:#333;color:white;padding:8px 12px;border-radius:4px;font-size:12px;",
                                use_fill = FALSE
                            ),
                            ggiraph::opts_toolbar(
                                saveaspng = TRUE,
                                position = "top",
                                hidden = NULL
                            )
                        )
                    )
                })
                
                # Register download handler for SVG export
                output[[download_id]] <- shiny::downloadHandler(
                    filename = function() {
                        paste0(local_measure, "_", Sys.Date(), ".svg")
                    },
                    content = function(file) {
                        # Get export dimensions (convert cm to inches)
                        width_cm <- if (!is.null(export_width) && is.function(export_width)) {
                            export_width()
                        } else {
                            16
                        }
                        height_cm <- if (!is.null(export_height) && is.function(export_height)) {
                            export_height()
                        } else {
                            10
                        }
                        
                        # Convert cm to inches (1 inch = 2.54 cm)
                        width_in <- width_cm / 2.54
                        height_in <- height_cm / 2.54
                        
                        # Get the cached plot
                        p <- get_plot()
                        
                        # Save as SVG
                        grDevices::svg(file, width = width_in, height = height_in)
                        print(p)
                        grDevices::dev.off()
                    }
                )
            })
        })
    })
}


#' Generate Plot List UI
#'
#' Creates the UI container with plot cards stacked vertically (one per row).
#' Uses ggiraph::girafeOutput for interactive plots.
#' Each card includes a download button for SVG export.
#'
#' @param ns Namespace function from parent module
#' @param measures Character vector of measurement column names
#' @return A div containing vertically stacked plot cards
#' @export
generate_plot_grid_ui <- function(ns, measures) {
    
    # Generate plot cards for each measurement - one per row
    plot_cards <- lapply(measures, function(measure) {
        plot_id <- paste0("plot_", gsub("[^a-zA-Z0-9]", "_", measure))
        download_id <- paste0("download_", plot_id)
        
        bslib::card(
            class = "mb-3 plot-card",
            bslib::card_header(
                class = "py-2 d-flex justify-content-between align-items-center",
                shiny::tags$span(class = "fw-semibold", measure),
                shiny::tags$a(
                    id = ns(download_id),
                    class = "shiny-download-link text-primary",
                    href = "",
                    target = "_blank",
                    download = NA,
                    title = "Download plot (PNG)",
                    style = "font-size: 1.2rem;",
                    bsicons::bs_icon("box-arrow-down")
                )
            ),
            bslib::card_body(
                class = "p-2 plot-card-body",
                # Use girafeOutput for interactive plots with responsive wrapper
                # Height is "auto" - actual size determined by SVG dimensions from server
                shiny::tags$div(
                    class = "responsive-plot",
                    shinycssloaders::withSpinner(
                        ggiraph::girafeOutput(
                            ns(plot_id), 
                            height = "auto",
                            width = "100%"
                        ),
                        type = 6,        # Spinner style (1-8)
                        color = "#0d6efd", # Bootstrap primary color
                        hide.ui = TRUE
                    )
                )
            )
        )
    })
    
    # Stack cards vertically
    shiny::tags$div(
        class = "plot-container",
        plot_cards
    )
}


#' Create Placeholder Message UI
#'
#' Returns a centered placeholder message for various states.
#'
#' @param message Character string or tagList for the message content
#' @param min_height CSS min-height value (default: "300px")
#' @return A div with centered message
#' @export
create_placeholder_ui <- function(message, min_height = "300px") {
    shiny::tags$div(
        class = "d-flex align-items-center justify-content-center",
        style = paste0("min-height: ", min_height, ";"),
        shiny::tags$p(class = "text-muted", message)
    )
}
