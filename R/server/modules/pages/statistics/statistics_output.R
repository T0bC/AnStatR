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
#' @param cached_plot_objects Reactive containing cached ggplot objects from plotting tab
#' @param plot_params Reactive containing plot parameters including window_size from plotting tab
#' @param debug Logical, whether to enable debug logging
#' @param data_version Reactive integer that increments when new data is loaded (optional)
#' @return Reactive containing computation results (for use by download handlers)
setup_statistics_output <- function(input, output, session, processed_data, 
                                     selected_measures, x_axis, trim_percent,
                                     stats_params, cached_plot_objects, plot_params, 
                                     debug = FALSE, data_version = NULL) {
    ns <- session$ns
    
    # Store computation results
    computation_results <- shiny::reactiveVal(NULL)
    computation_status <- shiny::reactiveVal("idle")  # idle, computing, done, error
    
    # Track registered plot outputs to avoid re-registration
    registered_stat_plots <- shiny::reactiveVal(character(0))
    
    # Reset computation state when data changes (prevents stale results with large datasets)
    if (!is.null(data_version)) {
        shiny::observeEvent(data_version(), {
            computation_results(NULL)
            computation_status("idle")
            registered_stat_plots(character(0))
        }, ignoreInit = TRUE)
    }
    
    # Register plot outputs for each measurement (reuses cached plots from plotting tab)
    shiny::observeEvent(selected_measures(), ignoreNULL = TRUE, {
        measures <- selected_measures()
        shiny::req(measures)
        
        already_registered <- registered_stat_plots()
        
        # Check if measures are identical (both content and length)
        if (setequal(measures, already_registered)) {
            return()
        }
        
        registered_stat_plots(measures)
        
        lapply(measures, function(measure) {
            local({
                local_measure <- measure
                plot_id <- paste0("stat_plot_", gsub("[^a-zA-Z0-9]", "_", local_measure))
                
                output[[plot_id]] <- ggiraph::renderGirafe({
                    plots <- cached_plot_objects()
                    shiny::req(plots, local_measure %in% names(plots))
                    p <- plots[[local_measure]]
                    
                    # Get SVG dimensions from window size (same as plotting tab)
                    params <- plot_params()
                    win_size <- params$window_size
                    
                    # Get container width (JS measures main content area)
                    container_width <- if (!is.null(win_size) && !is.null(win_size$width) && win_size$width > 0) {
                        win_size$width
                    } else {
                        800  # Default fallback
                    }
                    
                    # For statistics tab, use a fixed height ratio (smaller than plotting tab)
                    # since we show multiple cards with results below the plot
                    container_height <- if (!is.null(win_size) && !is.null(win_size$height) && win_size$height > 0) {
                        min(win_size$height * 0.6, 300)  # 60% of plotting height, max 300px
                    } else {
                        250  # Default fallback
                    }
                    
                    # Convert pixels to SVG inches (100 pixels per inch)
                    width_svg <- container_width / 100
                    height_svg <- container_height / 100
                    
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
            })
        })
    })
    
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
                # Create download button ID for this measurement
                safe_measure <- gsub("[^a-zA-Z0-9]", "_", res$measure)
                download_id <- paste0("download_report_", safe_measure)
                
                # Header with design type and download button
                header_content <- shiny::tags$div(
                    class = "d-flex justify-content-between align-items-center",
                    shiny::tags$span(
                        bsicons::bs_icon("graph-up", class = "me-2"),
                        res$measure
                    ),
                    shiny::tags$div(
                        class = "d-flex align-items-center gap-2",
                        shiny::tags$span(
                            class = "badge bg-secondary",
                            res$design_type
                        ),
                        shiny::tags$a(
                            id = ns(download_id),
                            class = "shiny-download-link text-primary",
                            href = "",
                            target = "_blank",
                            download = NA,
                            title = "Download report (HTML)",
                            style = "font-size: 1.2rem;",
                            bsicons::bs_icon("box-arrow-down")
                        )
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
                
                # Linear Contrasts (lincon) results
                lincon_ui <- NULL
                if (!is.null(res$result_lincon) && stats_params()$show_additional_output) {
                    if (is_stat_error(res$result_lincon)) {
                        lincon_ui <- shiny::tags$div(
                            class = "mt-3",
                            shiny::tags$h6("Linear Contrasts"),
                            shiny::tags$div(
                                class = "alert alert-danger",
                                render_stat_error(res$result_lincon)
                            )
                        )
                    } else if (is.data.frame(res$result_lincon) && "Error" %in% names(res$result_lincon)) {
                        lincon_ui <- shiny::tags$div(
                            class = "mt-3",
                            shiny::tags$h6("Linear Contrasts"),
                            shiny::tags$div(
                                class = "alert alert-danger",
                                shiny::HTML(paste(res$result_lincon$Error, collapse = "<br>"))
                            )
                        )
                    } else if (is.data.frame(res$result_lincon) && nrow(res$result_lincon) > 0) {
                        lincon_ui <- shiny::tags$div(
                            class = "mt-3",
                            shiny::tags$h6("Linear Contrasts"),
                            shiny::tags$div(
                                class = "table-responsive",
                                render_stats_table(res$result_lincon)
                            )
                        )
                    }
                }
                
                # Cliff's Delta results
                cliff_ui <- NULL
                if (!is.null(res$result_cliff) && stats_params()$show_additional_output) {
                    if (is_stat_error(res$result_cliff)) {
                        cliff_ui <- shiny::tags$div(
                            class = "mt-3",
                            shiny::tags$h6("Cliff's Delta + Effect Size"),
                            shiny::tags$div(
                                class = "alert alert-danger",
                                render_stat_error(res$result_cliff)
                            )
                        )
                    } else if (is.data.frame(res$result_cliff) && "Error" %in% names(res$result_cliff)) {
                        cliff_ui <- shiny::tags$div(
                            class = "mt-3",
                            shiny::tags$h6("Cliff's Delta + Effect Size"),
                            shiny::tags$div(
                                class = "alert alert-danger",
                                shiny::HTML(paste(res$result_cliff$Error, collapse = "<br>"))
                            )
                        )
                    } else if (is.data.frame(res$result_cliff) && nrow(res$result_cliff) > 0) {
                        cliff_ui <- shiny::tags$div(
                            class = "mt-3",
                            shiny::tags$h6("Cliff's Delta + Effect Size"),
                            shiny::tags$div(
                                class = "table-responsive",
                                render_stats_table(res$result_cliff)
                            )
                        )
                    }
                }
                
                # Combined results table
                combined_ui <- NULL
                if (!is.null(res$result_combined)) {
                    if (is_stat_error(res$result_combined)) {
                        combined_ui <- shiny::tags$div(
                            class = "mt-3",
                            shiny::tags$h6(
                                class = "text-primary",
                                bsicons::bs_icon("table", class = "me-1"),
                                "Combined Pairwise Comparisons"
                            ),
                            shiny::tags$div(
                                class = "alert alert-danger",
                                render_stat_error(res$result_combined)
                            )
                        )
                    } else if (is.data.frame(res$result_combined) && "Error" %in% names(res$result_combined)) {
                        combined_ui <- shiny::tags$div(
                            class = "mt-3",
                            shiny::tags$h6(
                                class = "text-primary",
                                bsicons::bs_icon("table", class = "me-1"),
                                "Combined Pairwise Comparisons"
                            ),
                            shiny::tags$div(
                                class = "alert alert-warning",
                                shiny::HTML(paste(res$result_combined$Error, collapse = "<br>"))
                            )
                        )
                    } else if (is.data.frame(res$result_combined) && nrow(res$result_combined) > 0) {
                        combined_ui <- shiny::tags$div(
                            class = "mt-3",
                            shiny::tags$h6(
                                class = "text-primary",
                                bsicons::bs_icon("table", class = "me-1"),
                                "Combined Pairwise Comparisons"
                            ),
                            shiny::tags$div(
                                class = "table-responsive",
                                render_stats_table(res$result_combined)
                            )
                        )
                    }
                }
                
                # Plot UI - reuses cached plot from plotting tab (interactive ggiraph)
                plot_id <- paste0("stat_plot_", gsub("[^a-zA-Z0-9]", "_", res$measure))
                plot_ui <- shiny::tags$div(
                    class = "mb-3 border-bottom pb-3",
                    ggiraph::girafeOutput(ns(plot_id), height = "250px", width = "100%")
                )
                
                bslib::card(
                    class = "mb-3",
                    bslib::card_header(header_content),
                    bslib::card_body(
                        plot_ui,
                        error_ui,
                        tway_ui,
                        lincon_ui,
                        cliff_ui,
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
    
    # Return computation_results for use by download handlers
    computation_results
}
