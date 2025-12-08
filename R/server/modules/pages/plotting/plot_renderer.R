#' Plot Renderer Component
#'
#' Handles rendering of multiple ggplot2 scatter plots in a responsive grid layout.
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
#' @param create_scatter_plot Function to create scatter plots (injected dependency)
#' @param plot_height Height of each plot in pixels (default: 350)
#' @return NULL (side effects only - registers plot outputs)
setup_plot_outputs <- function(output, 
                                ns, 
                                filtered_data, 
                                x_cols, 
                                measure_cols,
                                create_scatter_plot,
                                plot_height = 350) {
    
    # Register dynamic plot outputs for each measurement column
    shiny::observe({
        measures <- measure_cols()
        shiny::req(measures)
        
        # Create a plot output for each measurement
        lapply(measures, function(measure) {
            # Create safe ID from measure name
            plot_id <- paste0("plot_", gsub("[^a-zA-Z0-9]", "_", measure))
            
            # Register the plot output
            output[[plot_id]] <- shiny::renderPlot({
                df <- filtered_data()
                x <- x_cols()
                
                shiny::req(df, x)
                
                create_scatter_plot(
                    data = df,
                    x_col = x,
                    y_col = measure
                )
            }, height = plot_height)
        })
    })
}


#' Generate Plot List UI
#'
#' Creates the UI container with plot cards stacked vertically (one per row).
#' Uses 100% width for responsive resizing.
#'
#' @param ns Namespace function from parent module
#' @param measures Character vector of measurement column names
#' @param plot_height Height of each plot in pixels (or "auto" for responsive)
#' @return A div containing vertically stacked plot cards
generate_plot_grid_ui <- function(ns, measures, plot_height = "400px") {
    
    # Generate plot cards for each measurement - one per row
    plot_cards <- lapply(measures, function(measure) {
        plot_id <- paste0("plot_", gsub("[^a-zA-Z0-9]", "_", measure))
        
        bslib::card(
            class = "mb-3",  # margin bottom for spacing between cards
            bslib::card_header(
                class = "py-2",
                shiny::tags$span(class = "fw-semibold", measure)
            ),
            bslib::card_body(
                class = "p-2",
                # Use 100% width for responsive behavior
                shiny::plotOutput(
                    ns(plot_id), 
                    height = plot_height,
                    width = "100%"
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
