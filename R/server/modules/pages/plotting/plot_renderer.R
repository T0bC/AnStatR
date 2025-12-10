#' Plot Renderer Component
#'
#' Handles rendering of multiple interactive ggiraph scatter plots.
#' Each measurement variable gets its own plot card.
#'
#' Following the explicit dependency injection pattern:
#' - All dependencies (output, ns, reactives) are passed as explicit parameters
#' - No implicit scoping or global state access
#'
#' @param output Shiny output object from parent module
#' @param ns Namespace function from parent module (session$ns)
#' @param plot_params Reactive returning consolidated list of plot parameters:
#'   data, x_cols, tooltip_cols, trim_percent, outlier_options, color_cols, color_map, window_size
#' @param measure_cols Reactive returning selected measurement column names
#' @param create_scatter_plot Function to create scatter plots (injected dependency)
#' @param export_width Reactive returning export width in cm
#' @param export_height Reactive returning export height in cm
#' @return NULL (side effects only - registers plot outputs and download handlers)
setup_plot_outputs <- function(output, 
                                ns, 
                                plot_params,
                                measure_cols,
                                create_scatter_plot,
                                export_width = NULL,
                                export_height = NULL) {
    
    # Track which outputs have been registered to avoid re-registration
    registered_outputs <- shiny::reactiveVal(character(0))
    
    # ===== DEBUG: Toggle to enable/disable debug logging =====
    DEBUG_PLOT_RENDERER <- FALSE
    
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
        new_measures <- setdiff(measures, already_registered)
        
        # Only register outputs for new measures
        if (length(new_measures) == 0 && length(measures) == length(already_registered)) {
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
                
                # Helper function to create the ggplot (shared between render and download)
                create_plot <- function() {
                    params <- plot_params()
                    shiny::req(params$data, params$x_cols)
                    
                    outlier_opts <- params$outlier_options %||% 
                        list(enabled = FALSE, method = "IQR", factor = 1.5, bootstrap_samples = 1000)
                    
                    create_scatter_plot(
                        data = params$data,
                        x_col = params$x_cols,
                        y_col = local_measure,
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
                }
                
                # Register the girafe output for interactive plots
                # Uses consolidated plot_params for single-debounce reactivity
                output[[plot_id]] <- ggiraph::renderGirafe({
                    # Debug logging (conditional)
                    if (DEBUG_PLOT_RENDERER) {
                        message(paste0("[", format(Sys.time(), "%H:%M:%S.%OS3"), 
                                      "] renderGirafe EXECUTING for: ", local_measure))
                    }
                    
                    # Get all parameters from consolidated reactive (single dependency)
                    params <- plot_params()
                    shiny::req(params$data, params$x_cols)
                    
                    outlier_opts <- params$outlier_options %||% 
                        list(enabled = FALSE, method = "IQR", factor = 1.5, bootstrap_samples = 1000)
                    
                    # Calculate SVG width from container size
                    # The formula produces a reasonable SVG size in inches
                    win_size <- params$window_size
                    width_svg <- if (!is.null(win_size) && !is.null(win_size$width)) {
                        round(win_size$width / 100, 0) / 2.0
                    } else {
                        5.0  # Default width
                    }
                    
                    # Create the ggplot with interactive elements
                    p <- create_scatter_plot(
                        data = params$data,
                        x_col = params$x_cols,
                        y_col = local_measure,
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
                    
                    # Convert to girafe interactive plot
                    ggiraph::girafe(
                        ggobj = p,
                        width_svg = width_svg,
                        height_svg = ceiling(650/100) / 2.0,
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
                        
                        # Create the plot
                        p <- create_plot()
                        
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
#' @param plot_height Height of each plot (CSS value, default "400px")
#' @return A div containing vertically stacked plot cards
generate_plot_grid_ui <- function(ns, measures, plot_height = "400px") {
    
    # Generate plot cards for each measurement - one per row
    plot_cards <- lapply(measures, function(measure) {
        plot_id <- paste0("plot_", gsub("[^a-zA-Z0-9]", "_", measure))
        download_id <- paste0("download_", plot_id)
        
        bslib::card(
            class = "mb-3",
            bslib::card_header(
                class = "py-2 d-flex justify-content-between align-items-center",
                shiny::tags$span(class = "fw-semibold", measure),
                shiny::downloadButton(
                    outputId = ns(download_id),
                    label = NULL,
                    icon = shiny::icon("download"),
                    class = "btn-sm btn-outline-secondary",
                    title = "Download as SVG"
                )
            ),
            bslib::card_body(
                class = "p-2",
                # Use girafeOutput for interactive plots with responsive wrapper
                # Wrapped in withSpinner for loading indicator during plot updates
                shiny::tags$div(
                    class = "responsive-plot",
                    shinycssloaders::withSpinner(
                        ggiraph::girafeOutput(
                            ns(plot_id), 
                            height = plot_height,
                            width = "100%"
                        ),
                        type = 6,        # Spinner style (1-8)
                        color = "#0d6efd" # Bootstrap primary color
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
create_placeholder_ui <- function(message, min_height = "300px") {
    shiny::tags$div(
        class = "d-flex align-items-center justify-content-center",
        style = paste0("min-height: ", min_height, ";"),
        shiny::tags$p(class = "text-muted", message)
    )
}
