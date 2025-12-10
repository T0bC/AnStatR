#' Plots UI Component
#'
#' Handles rendering of the main plots container with step-by-step guidance.
#'
#' Following the explicit dependency injection pattern:
#' - All dependencies are passed as explicit parameters
#' - No implicit scoping or global state access
#'
#' @name plots_ui
NULL


#' Setup Plots UI Output
#'
#' Renders the plots UI container with placeholder messages or plot grid.
#'
#' @param output Shiny output object from parent module
#' @param ns Namespace function from parent module (session$ns)
#' @param input Shiny input object from parent module
#'   - input$metaData: Selected descriptive columns
#'   - input$measureVar: Selected measurement columns
#'   - input$xAxis: Selected X-axis columns
#' @param median_data Reactive containing the median-processed data
#' @param debug Logical, enable debug logging
#' @return NULL (side effects only - registers output)
setup_plots_ui_output <- function(output, ns, input, median_data, debug = FALSE) {
    
    debug_log <- function(source, details = NULL) {
        if (debug) {
            timestamp <- format(Sys.time(), "%H:%M:%S.%OS3")
            msg <- paste0("[", timestamp, "] ", source)
            if (!is.null(details)) {
                msg <- paste0(msg, " | ", details)
            }
            message(msg)
        }
    }
    
    output$plots <- shiny::renderUI({
        debug_log("output$plots renderUI EXECUTING", paste0(
            "metaData=", length(input$metaData),
            ", measures=", length(input$measureVar),
            ", xAxis=", length(input$xAxis)
        ))
        
        # Check if we have the minimum required selections
        has_data <- !is.null(median_data()) && nrow(median_data()) > 0
        meta_data <- input$metaData
        measures <- input$measureVar
        x_axis <- input$xAxis
        
        if (!has_data) {
            return(create_placeholder_ui(
                "No data available. Please complete the Median Analysis first."
            ))
        }
        
        # Step 1: Need descriptive columns
        if (is.null(meta_data) || length(meta_data) == 0) {
            return(create_placeholder_ui(
                shiny::tagList(
                    shiny::tags$div(
                        class = "text-center",
                        bsicons::bs_icon("1-circle", size = "2em", class = "text-primary mb-2"),
                        shiny::tags$p(
                            "Select ", shiny::tags$strong("Descriptive columns"),
                            " in the sidebar to get started."
                        ),
                        shiny::tags$p(
                            class = "small text-muted",
                            "These columns describe your data (e.g., sample ID, treatment, group)."
                        )
                    )
                )
            ))
        }
        
        # Step 2: Need measurement columns
        if (is.null(measures) || length(measures) == 0) {
            return(create_placeholder_ui(
                shiny::tagList(
                    shiny::tags$div(
                        class = "text-center",
                        bsicons::bs_icon("2-circle", size = "2em", class = "text-primary mb-2"),
                        shiny::tags$p(
                            "Select ", shiny::tags$strong("Measurement columns (Y-Axis)"),
                            " to define what to plot."
                        ),
                        shiny::tags$p(
                            class = "small text-muted",
                            "One plot will be generated for each measurement column."
                        )
                    )
                )
            ))
        }
        
        # Step 3: Need X-axis
        if (is.null(x_axis) || length(x_axis) == 0) {
            return(create_placeholder_ui(
                shiny::tagList(
                    shiny::tags$div(
                        class = "text-center",
                        bsicons::bs_icon("3-circle", size = "2em", class = "text-primary mb-2"),
                        shiny::tags$p(
                            "Select ", shiny::tags$strong("X-Axis"),
                            " column(s) to complete the plot configuration."
                        ),
                        shiny::tags$p(
                            class = "small text-muted",
                            "You can select up to 3 columns for grouping on the X-axis."
                        )
                    )
                )
            ))
        }
        
        # Generate plot grid
        generate_plot_grid_ui(
            ns = ns,
            measures = measures,
            plot_height = "400px"
        )
    })
}
