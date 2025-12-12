#' Statistics Output Logic
#'
#' Handles the main output area for statistics results.


#' Check if an object is a structured error from safe_stat_test
#'
#' Wrapper around is_app_error() for backward compatibility.
#'
#' @param obj Object to check
#' @return Logical, TRUE if obj is a structured error
is_stat_error <- function(obj) {
    is_app_error(obj)
}


#' Render structured error with expandable stack trace
#'
#' Wrapper around render_app_error() for backward compatibility.
#'
#' @param error_obj Structured error object from create_stat_error()
#' @return Shiny tags object with error display
render_stat_error <- function(error_obj) {
    render_app_error(error_obj)
}


#' Render a data frame as an HTML table
#'
#' @param df Data frame to render
#' @return Shiny tags object with HTML table
render_stats_table <- function(df) {
    # Build header row
    header_cells <- lapply(names(df), function(col) {
        shiny::tags$th(col)
    })
    header_row <- shiny::tags$tr(header_cells)
    
    # Build body rows
    body_rows <- lapply(seq_len(nrow(df)), function(i) {
        cells <- lapply(seq_len(ncol(df)), function(j) {
            shiny::tags$td(as.character(df[i, j]))
        })
        shiny::tags$tr(cells)
    })
    
    shiny::tags$table(
        class = "table table-sm table-striped table-bordered",
        shiny::tags$thead(header_row),
        shiny::tags$tbody(body_rows)
    )
}


#' @param input Shiny input object from the parent module
#' @param output Shiny output object from the parent module
#' @param session Shiny session object from the parent module
#' @param processed_data Reactive containing the processed data from plotting
#' @param selected_measures Reactive containing selected measurement columns
#' @param x_axis Reactive containing selected X-axis columns
#' @param trim_percent Reactive containing the trim percentage from plotting
#' @param stats_params Reactive containing all statistics parameters
#' @param debug Logical, whether to enable debug logging
setup_statistics_output <- function(input, output, session, processed_data, 
                                     selected_measures, x_axis, trim_percent,
                                     stats_params, debug = FALSE) {
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
        
        # Check level consistency for multi-way designs (two-way, three-way)
        # This warns users that Welch-Yuen is not robust against level discrepancies
        if (length(x_cols) > 1) {
            # For each measurement, get filtered data and check consistency
            level_discrepancies <- character(0)
            
            for (measure in measures) {
                filtered_df <- get_filtered_measurement_data(data, measure)
                discrepancy <- check_level_consistency(
                    df = filtered_df,
                    primary_group = x_cols[1],
                    secondary_groups = x_cols[2:length(x_cols)]
                )
                if (!is.null(discrepancy)) {
                    level_discrepancies <- c(
                        level_discrepancies,
                        paste0("<b>", measure, ":</b>"),
                        discrepancy
                    )
                }
            }
            
            if (length(level_discrepancies) > 0) {
                shiny::showModal(
                    shiny::modalDialog(
                        title = "Level Discrepancy Detected",
                        shiny::HTML(
                            paste0(
                                paste(level_discrepancies, collapse = "<br>"),
                                "<br><br>The <b>Welch-Yuen</b> test is not robust against <b>level discrepancies</b>.<br>",
                                "Please ensure that the <b>levels</b> of the groups are <b>consistent</b> across all selected columns.<br>",
                                "Cliff's <b>Delta</b> and <b>Linear Contrast</b> tests are still computed..."
                            )
                        ),
                        easyClose = TRUE,
                        footer = shiny::modalButton("Continue")
                    )
                )
            }
        }
        
        # Set status to computing
        computation_status("computing")
        
        if (debug) {
            message("[Statistics] Starting computation...")
            message("[Statistics] Measures: ", paste(measures, collapse = ", "))
            message("[Statistics] X-axis: ", paste(x_cols, collapse = ", "))
            message("[Statistics] Bootstrap: ", params$use_bootstrap)
        }
        
        # Compute statistics for each measurement with progress indicator
        tr_value <- (trim_percent() %||% 0) / 100
        n_measures <- length(measures)
        
        results_list <- shiny::withProgress(
            message = "Computing Statistics",
            detail = "This might take a while...",
            value = 0,
            {
                lapply(seq_along(measures), function(i) {
                    measure <- measures[i]
                    
                    shiny::incProgress(
                        1 / n_measures,
                        detail = paste("Computing statistics for", measure)
                    )
                    
                    # Get filtered data for this measurement (excludes outliers/trimmed)
                    filtered_df <- get_filtered_measurement_data(data, measure)
                    
                    # Check level consistency for this measurement (multi-way only)
                    level_discrepancy <- NULL
                    if (length(x_cols) > 1) {
                        level_discrepancy <- check_level_consistency(
                            df = filtered_df,
                            primary_group = x_cols[1],
                            secondary_groups = x_cols[2:length(x_cols)]
                        )
                    }
                    
                    # Compute all statistics for this measurement
                    compute_measurement_statistics(
                        df = filtered_df,
                        x_axis = x_cols,
                        measure_col = measure,
                        tr_value = tr_value,
                        params = params,
                        level_discrepancy = level_discrepancy
                    )
                })
            }
        )
        
        # Store results
        computation_results(list(
            results = results_list,
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
        
        # Results state - render per-measurement results
        if (status == "done") {
            # Build UI for each measurement result
            measurement_cards <- lapply(results$results, function(res) {
                # Header with design type
                header_content <- shiny::tags$div(
                    class = "d-flex justify-content-between align-items-center",
                    shiny::tags$span(
                        bsicons::bs_icon("graph-up", class = "me-2"),
                        res$measure
                    ),
                    shiny::tags$span(
                        class = "badge bg-secondary",
                        res$design_type
                    )
                )
                
                # Error display if any
                error_ui <- NULL
                if (length(res$errors) > 0) {
                    error_ui <- shiny::tags$div(
                        class = "alert alert-warning",
                        shiny::icon("exclamation-triangle"),
                        " ",
                        paste(unlist(res$errors), collapse = "; ")
                    )
                }
                
                # T-way result (ANOVA)
                tway_ui <- NULL
                if (!is.null(res$result_t_way)) {
                    if (is_stat_error(res$result_t_way)) {
                        # Structured error with stack traces
                        tway_ui <- shiny::tags$div(
                            class = "alert alert-danger",
                            render_stat_error(res$result_t_way)
                        )
                    } else if (is.data.frame(res$result_t_way) && "Error" %in% names(res$result_t_way)) {
                        # Legacy error format (data frame with Error column)
                        tway_ui <- shiny::tags$div(
                            class = "alert alert-danger",
                            shiny::HTML(paste(res$result_t_way$Error, collapse = "<br>"))
                        )
                    } else if (is.data.frame(res$result_t_way)) {
                        # Valid results - render as HTML table
                        tway_ui <- shiny::tags$div(
                            shiny::tags$h6(
                                class = "mb-2",
                                res$header
                            ),
                            shiny::tags$div(
                                class = "table-responsive",
                                render_stats_table(res$result_t_way)
                            )
                        )
                    }
                }
                
                # Combined results placeholder
                combined_ui <- shiny::tags$div(
                    class = "mt-3",
                    shiny::tags$h6("Pairwise Comparisons (Linear Contrasts + Cliff's Delta)"),
                    shiny::tags$div(
                        class = "alert alert-secondary",
                        shiny::tags$small("Results tables will be rendered here once tests are implemented.")
                    )
                )
                
                bslib::card(
                    class = "mb-3",
                    bslib::card_header(header_content),
                    bslib::card_body(
                        error_ui,
                        tway_ui,
                        combined_ui
                    )
                )
            })
            
            return(
                shiny::tagList(
                    shiny::tags$div(
                        class = "d-flex justify-content-between align-items-center mb-3",
                        shiny::tags$h5(
                            class = "mb-0",
                            "Statistical Test Results"
                        ),
                        shiny::tags$small(
                            class = "text-muted",
                            paste("Computed:", format(results$timestamp, "%H:%M:%S"))
                        )
                    ),
                    shiny::tags$div(
                        class = "alert alert-info py-2",
                        shiny::tags$small(
                            shiny::tags$strong("Configuration: "),
                            paste0(
                                length(results$measures), " measurement(s), ",
                                length(results$x_axis), "-way design, ",
                                "Bootstrap: ", ifelse(results$params$use_bootstrap, "Yes", "No"), ", ",
                                "P-adjustment: ", results$params$p_val_cor_method
                            )
                        )
                    ),
                    measurement_cards
                )
            )
        }
    })
}
