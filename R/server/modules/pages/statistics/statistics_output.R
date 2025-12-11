#' Statistics Output Logic
#'
#' Handles the main output area for statistics results.
#'
#' @param input Shiny input object from the parent module
#' @param output Shiny output object from the parent module
#' @param session Shiny session object from the parent module
#' @param processed_data Reactive containing the processed data from plotting
#' @param selected_measures Reactive containing selected measurement columns
#' @param x_axis Reactive containing selected X-axis columns
#' @param stats_params Reactive containing all statistics parameters
#' @param debug Logical, whether to enable debug logging
setup_statistics_output <- function(input, output, session, processed_data, 
                                     selected_measures, x_axis, stats_params, debug = FALSE) {
    ns <- session$ns
    
    # Store computation results
    computation_results <- shiny::reactiveVal(NULL)
    computation_status <- shiny::reactiveVal("idle")  # idle, computing, done, error
    
    # Handle compute button click
    shiny::observeEvent(input$compute_button, {
        # Validate inputs
        data <- processed_data()
        measures <- selected_measures()
        x_cols <- x_axis()
        params <- stats_params()
        
        if (is.null(data) || nrow(data) == 0) {
            computation_status("error")
            computation_results(list(error = "No data available. Please load data and configure the Plotting tab first."))
            return()
        }
        
        if (length(measures) == 0) {
            computation_status("error")
            computation_results(list(error = "No measurement columns selected. Please select measurements in the Plotting tab."))
            return()
        }
        
        if (length(x_cols) == 0) {
            computation_status("error")
            computation_results(list(error = "No X-axis columns selected. Please select X-axis in the Plotting tab."))
            return()
        }
        
        # Set status to computing
        computation_status("computing")
        
        if (debug) {
            message("[Statistics] Starting computation...")
            message("[Statistics] Measures: ", paste(measures, collapse = ", "))
            message("[Statistics] X-axis: ", paste(x_cols, collapse = ", "))
            message("[Statistics] Bootstrap: ", params$use_bootstrap)
        }
        
        # TODO: Implement actual statistical computation
        # For now, store placeholder results
        computation_results(list(
            data = data,
            measures = measures,
            x_axis = x_cols,
            params = params,
            timestamp = Sys.time()
        ))
        computation_status("done")
        
        if (debug) {
            message("[Statistics] Computation complete.")
        }
    })
    
    # Render main output area
    output$statistics_output <- shiny::renderUI({
        status <- computation_status()
        results <- computation_results()
        
        # Initial state - no computation yet
        if (status == "idle" || is.null(results)) {
            return(
                bslib::card(
                    bslib::card_header("Statistical Test Results"),
                    bslib::card_body(
                        class = "d-flex align-items-center justify-content-center",
                        style = "min-height: 300px;",
                        shiny::tags$div(
                            class = "text-center text-muted",
                            shiny::tags$p(
                                bsicons::bs_icon("calculator", size = "3em", class = "mb-3")
                            ),
                            shiny::tags$p(
                                "Configure options in the sidebar and click ",
                                shiny::tags$strong("Compute Statistics"),
                                " to run the analysis."
                            ),
                            shiny::tags$p(
                                class = "small",
                                "Data selection, filtering, and trimming are inherited from the Plotting tab."
                            )
                        )
                    )
                )
            )
        }
        
        # Computing state
        if (status == "computing") {
            return(
                bslib::card(
                    bslib::card_header("Statistical Test Results"),
                    bslib::card_body(
                        class = "d-flex align-items-center justify-content-center",
                        style = "min-height: 300px;",
                        shiny::tags$div(
                            class = "text-center",
                            shinycssloaders::withSpinner(
                                shiny::tags$div(),
                                type = 6,
                                color = "#0d6efd"
                            ),
                            shiny::tags$p(class = "mt-3", "Computing statistics...")
                        )
                    )
                )
            )
        }
        
        # Error state
        if (status == "error" && !is.null(results$error)) {
            return(
                bslib::card(
                    bslib::card_header(
                        class = "bg-danger text-white",
                        "Error"
                    ),
                    bslib::card_body(
                        shiny::tags$div(
                            class = "alert alert-danger",
                            shiny::icon("exclamation-triangle"),
                            " ",
                            results$error
                        )
                    )
                )
            )
        }
        
        # Results state - placeholder for now
        if (status == "done") {
            return(
                bslib::card(
                    bslib::card_header(
                        shiny::tags$div(
                            class = "d-flex justify-content-between align-items-center",
                            "Statistical Test Results",
                            shiny::tags$small(
                                class = "text-muted",
                                paste("Computed:", format(results$timestamp, "%H:%M:%S"))
                            )
                        )
                    ),
                    bslib::card_body(
                        shiny::tags$div(
                            class = "alert alert-info",
                            shiny::tags$strong("Placeholder: "),
                            "Statistical computation logic will be implemented here."
                        ),
                        shiny::tags$h6("Configuration Summary:"),
                        shiny::tags$ul(
                            shiny::tags$li(paste("Measurements:", paste(results$measures, collapse = ", "))),
                            shiny::tags$li(paste("X-axis:", paste(results$x_axis, collapse = ", "))),
                            shiny::tags$li(paste("Bootstrap:", results$params$use_bootstrap)),
                            shiny::tags$li(paste("P-value adjustment:", results$params$p_val_cor_method)),
                            shiny::tags$li(paste("Filter significant:", results$params$filter_p_values))
                        )
                    )
                )
            )
        }
    })
}
