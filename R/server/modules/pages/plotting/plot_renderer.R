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
#' @param filtered_data Reactive returning the filtered data frame
#' @param x_cols Reactive returning selected X-axis column name(s)
#' @param measure_cols Reactive returning selected measurement column names
#' @param tooltip_cols Reactive returning selected tooltip column names
#' @param create_scatter_plot Function to create scatter plots (injected dependency)
#' @param window_size Reactive returning list with width/height from JS (for dynamic SVG sizing)
#' @return NULL (side effects only - registers plot outputs)
setup_plot_outputs <- function(output, 
                                ns, 
                                filtered_data, 
                                x_cols, 
                                measure_cols,
                                tooltip_cols,
                                create_scatter_plot,
                                window_size = NULL) {
    
    # Register dynamic plot outputs for each measurement column
    # Width calculation MUST be outside renderGirafe to trigger re-registration on resize
    shiny::observe({
        measures <- measure_cols()
        shiny::req(measures)
        
        # Calculate width OUTSIDE renderGirafe (like your old working code)
        # This ensures the observe re-runs when window_size changes
        win_size <- if (!is.null(window_size) && is.function(window_size)) window_size() else NULL
        
        # DEBUG: Log when this observe fires
        message("plot_renderer observe fired, win_size: ", 
                if (!is.null(win_size)) paste0(win_size$width, "x", win_size$height) else "NULL")
        
        if (!is.null(win_size) && !is.null(win_size$width)) {
            width_svg <- round(win_size$width / 100, 0) / 1.5
        } else {
            width_svg <- 10 / 1.5
        }
        
        # Create a plot output for each measurement
        lapply(measures, function(measure) {
            # Use local() to ensure proper closure capture for each iteration
            local({
                # Capture measure and width in local scope
                local_measure <- measure
                local_width <- width_svg
                # Create safe ID from measure name
                plot_id <- paste0("plot_", gsub("[^a-zA-Z0-9]", "_", local_measure))
                
                # Register the girafe output for interactive plots
                output[[plot_id]] <- ggiraph::renderGirafe({
                    df <- filtered_data()
                    x <- x_cols()
                    tt_cols <- tooltip_cols()
                    
                    shiny::req(df, x)
                    
                    # Create the ggplot with interactive elements
                    p <- create_scatter_plot(
                        data = df,
                        x_col = x,
                        y_col = local_measure,
                        tooltip_cols = tt_cols
                    )
                    
                    # Convert to girafe interactive plot
                    ggiraph::girafe(
                        ggobj = p,
                        width_svg = local_width,
                        height_svg = ceiling(650/100) / 1.5,
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
            })
        })
    })
}


#' Generate Plot List UI
#'
#' Creates the UI container with plot cards stacked vertically (one per row).
#' Uses ggiraph::girafeOutput for interactive plots.
#'
#' @param ns Namespace function from parent module
#' @param measures Character vector of measurement column names
#' @param plot_height Height of each plot (CSS value, default "400px")
#' @return A div containing vertically stacked plot cards
generate_plot_grid_ui <- function(ns, measures, plot_height = "400px") {
    
    # Generate plot cards for each measurement - one per row
    plot_cards <- lapply(measures, function(measure) {
        plot_id <- paste0("plot_", gsub("[^a-zA-Z0-9]", "_", measure))
        
        bslib::card(
            class = "mb-3",
            bslib::card_header(
                class = "py-2",
                shiny::tags$span(class = "fw-semibold", measure)
            ),
            bslib::card_body(
                class = "p-2",
                # Use girafeOutput for interactive plots with responsive wrapper
                shiny::tags$div(
                    class = "responsive-plot",
                    ggiraph::girafeOutput(
                        ns(plot_id), 
                        height = plot_height,
                        width = "100%"
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
