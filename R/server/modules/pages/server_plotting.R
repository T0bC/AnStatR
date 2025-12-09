#' Server logic for the Plotting page
#'
#' @param id Module namespace ID
#' @param median_data Reactive containing the median-processed data from server_median
#' @param data_version Reactive integer that increments when new data is loaded
#' @return NULL (side effects only)
server_plotting <- function(id, median_data, data_version) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Reset all inputs when new data is loaded
        if (!is.null(data_version)) {
            shiny::observeEvent(data_version(), {
                shiny::updateSelectizeInput(session, "metaData", selected = character(0))
                shiny::updateSelectizeInput(session, "measureVar", selected = character(0))
                shiny::updateSelectizeInput(session, "hideCols", selected = character(0))
                shiny::updateSelectizeInput(session, "xAxis", selected = character(0))
                shiny::updateSelectizeInput(session, "tooltip", selected = character(0))
                shiny::updateSliderInput(session, "trim_slider", value = 0)
                shiny::updateCheckboxInput(session, "enableOutlierDetection", value = FALSE)
                shiny::updateRadioButtons(session, "detectOutlier", selected = "IQR")
                shiny::updateSliderInput(session, "standardFactor", value = 1.5)
                shiny::updateSliderInput(session, "probabilityFactor", value = 0.05)
                shiny::updateNumericInput(session, "bootstrapSamples", value = 1000)
            }, ignoreInit = TRUE)
        }
        
        # Reactive: get descriptive columns (strict pattern: uppercase + underscores only)
        descriptive_cols <- shiny::reactive({
            shiny::req(median_data())
            get_descriptive_cols(median_data())
        })
        
        # Reactive: get measurement columns
        measurement_cols <- shiny::reactive({
            shiny::req(median_data())
            get_measurement_cols(median_data())
        })
        
        # Update metaData choices when data changes
        shiny::observe({
            cols <- descriptive_cols()
            shiny::updateSelectizeInput(
                session, "metaData",
                choices = cols,
                selected = input$metaData
            )
        })
        
        # Update measureVar choices when data changes
        shiny::observe({
            cols <- measurement_cols()
            shiny::updateSelectizeInput(
                session, "measureVar",
                choices = cols,
                selected = input$measureVar
            )
        })
        
        # Update hideCols, xAxis, tooltip choices based on selected metaData
        shiny::observe({
            selected_meta <- input$metaData
            if (is.null(selected_meta)) selected_meta <- character(0)
            
            # Update hideCols
            shiny::updateSelectizeInput(
                session, "hideCols",
                choices = selected_meta,
                selected = input$hideCols[input$hideCols %in% selected_meta]
            )
            
            # Update xAxis
            shiny::updateSelectizeInput(
                session, "xAxis",
                choices = selected_meta,
                selected = input$xAxis[input$xAxis %in% selected_meta]
            )
            
            # Update tooltip
            shiny::updateSelectizeInput(
                session, "tooltip",
                choices = selected_meta,
                selected = input$tooltip[input$tooltip %in% selected_meta]
            )
            
            # Update pointShape choices (all metaData columns - user can shape by any descriptive column)
            shiny::updateSelectizeInput(
                session, "pointShape",
                choices = selected_meta,
                selected = input$pointShape[input$pointShape %in% selected_meta]
            )
        })
        
        # Update pointColor choices based on X-axis selection
        shiny::observe({
            x_axis <- input$xAxis
            if (is.null(x_axis) || length(x_axis) == 0) {
                shiny::updateSelectizeInput(session, "pointColor", choices = character(0))
            } else {
                shiny::updateSelectizeInput(
                    session, "pointColor",
                    choices = x_axis,
                    selected = if (is.null(input$pointColor)) x_axis[1] else input$pointColor
                )
            }
        })
        
        # Reactive: columns to show for filtering (metaData minus hideCols)
        # No debounce - feeds into filtered_data which feeds into plot_params (global debounce)
        filter_cols <- shiny::reactive({
            selected <- input$metaData
            hidden <- input$hideCols
            if (is.null(selected)) return(character(0))
            selected[!selected %in% hidden]
        })
        
        # Render checkboxes for filtering
        output$checkboxes <- shiny::renderUI({
            data <- median_data()
            shiny::req(data)
            
            cols <- filter_cols()
            
            if (length(cols) == 0) {
                return(shiny::tags$p(
                    class = "text-muted fst-italic small",
                    "Select descriptive columns or unhide some to see filtering options."
                ))
            }
            
            # Split columns into two groups for side-by-side layout
            if (length(cols) > 1) {
                half <- ceiling(length(cols) / 2)
                cols1 <- cols[seq_len(half)]
                cols2 <- cols[-seq_len(half)]
                
                shiny::fluidRow(
                    shiny::column(6, lapply(cols1, function(col) {
                        choices <- unique(data[[col]])
                        shiny::checkboxGroupInput(ns(col), label = col, choices = choices, selected = choices)
                    })),
                    shiny::column(6, lapply(cols2, function(col) {
                        choices <- unique(data[[col]])
                        shiny::checkboxGroupInput(ns(col), label = col, choices = choices, selected = choices)
                    }))
                )
            } else {
                col <- cols[1]
                choices <- unique(data[[col]])
                shiny::checkboxGroupInput(ns(col), label = col, choices = choices, selected = choices)
            }
        })
        
        # Reactive: filtered data based on checkbox selections
        # No debounce here - plot_params handles the global debounce for plot rendering
        # colorPickers UI will update immediately (which is fine for UI responsiveness)
        filtered_data <- shiny::reactive({
            data <- median_data()
            shiny::req(data)
            
            cols <- filter_cols()
            if (length(cols) == 0) return(data)
            
            # Apply filters from each checkbox group
            for (col in cols) {
                selected_values <- input[[col]]
                if (!is.null(selected_values) && length(selected_values) > 0) {
                    data <- data[data[[col]] %in% selected_values, , drop = FALSE]
                }
            }
            
            data
        })
        
        # Reactive: selected measurement columns
        selected_measures <- shiny::reactive({
            input$measureVar
        })
        
        # Reactive: selected X-axis columns
        selected_x_axis <- shiny::reactive({
            input$xAxis
        })
        
        # Reactive: selected tooltip columns
        selected_tooltip_cols <- shiny::reactive({
            input$tooltip
        })
        
        # Reactive: selected color columns (for interaction-based coloring)
        selected_color_cols <- shiny::reactive({
            color_cols <- input$pointColor
            # Default to x-axis if no color columns selected
            if (is.null(color_cols) || length(color_cols) == 0) {
                return(input$xAxis)
            }
            color_cols
        })
        
        # Reactive: window size from JS (for dynamic SVG sizing)
        # Access namespaced input set by plot_resize.js via initializeWindowSize()
        # Use reactiveVal to cache and only update when values actually change
        cached_window_size <- shiny::reactiveVal(list(width = 800, height = 600))
        
        shiny::observe({
            ws <- input$windowSize
            if (!is.null(ws)) {
                current <- cached_window_size()
                # Only update if values actually changed
                if (is.null(current) || ws$width != current$width || ws$height != current$height) {
                    if (exists("DEBUG_REACTIVES") && DEBUG_REACTIVES) {
                        message(paste0("[", format(Sys.time(), "%H:%M:%S"), "] window_size CHANGED: ", 
                                      ws$width, "x", ws$height))
                    }
                    cached_window_size(ws)
                }
            }
        })
        
        # Expose as reactive for downstream use
        window_size <- shiny::reactive({
            cached_window_size()
        })
        
        # Reactive: export dimensions from Plot Style tab
        export_width <- shiny::reactive({
            input$exportWidth %||% 16
        })
        export_height <- shiny::reactive({
            input$exportHeight %||% 10
        })
        
        # Reactive: trim percentage from Processing tab
        # No debounce here - consolidated in plot_params
        trim_percent <- shiny::reactive({
            input$trim_slider %||% 0
        })
        
        # Reactive: outlier detection options from Processing tab
        # No debounce here - consolidated in plot_params
        outlier_options <- shiny::reactive({
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
        
        # ===== DEBUG: Toggle this to enable/disable debug logging =====
        DEBUG_REACTIVES <- FALSE
        
        debug_log <- function(source, details = NULL) {
            if (DEBUG_REACTIVES) {
                timestamp <- format(Sys.time(), "%H:%M:%S.%OS3")
                msg <- paste0("[", timestamp, "] REACTIVE: ", source)
                if (!is.null(details)) {
                    msg <- paste0(msg, " | ", details)
                }
                message(msg)
            }
        }
        
        # Reactive: point styling options from Plot Style accordion
        point_style <- shiny::reactive({
            # shape_cols can be NULL, single column, or multiple columns
            shape_cols <- input$pointShape
            if (length(shape_cols) == 0) shape_cols <- NULL
            
            list(
                size = input$pointSize %||% 4,
                spread = input$pointSpread %||% 0.15,
                alpha = input$transparency %||% 0.6,
                shape_cols = shape_cols  # Character vector of column(s) for shape mapping
            )
        })
        
        # Reactive: grid and legend options from Legend & Grid accordion
        grid_legend_options <- shiny::reactive({
            # Use character(0) as default when NULL (all unchecked) - not the full set
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
        })
        
        # Reactive: median and SD line styling
        stat_line_style <- shiny::reactive({
            list(
                median_thickness = input$medianThickness %||% 0.5,
                median_width = input$medianWidth %||% 0.15,
                sd_thickness = input$sdThickness %||% 0.5,
                sd_width = input$sdWidth %||% 0.15
            )
        })
        
        # Reactive: axis styling options
        axis_style <- shiny::reactive({
            list(
                tick_length = input$axisTickLength %||% 0.15,
                line_thickness = input$axisLineThickness %||% 0.5
            )
        })
        
        # Consolidated plot parameters - bundles all plot-affecting reactives
        # Uses caching to prevent re-renders when values haven't actually changed
        
        # Cache for plot parameters - only updates when values actually differ
        cached_plot_params <- shiny::reactiveVal(NULL)
        
        # Helper to create a fingerprint for comparison (excludes data content, uses row count)
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
                # Point style fingerprint
                params$point_style$size,
                params$point_style$spread,
                params$point_style$alpha,
                paste(params$point_style$shape_cols %||% "none", collapse = ":"),
                # Grid/legend fingerprint
                params$grid_legend$legend_position,
                params$grid_legend$h_grid,
                params$grid_legend$v_grid,
                params$grid_legend$top_right_borders,
                params$grid_legend$show_median,
                params$grid_legend$show_sd,
                params$grid_legend$aspect_ratio,
                # Stat line style fingerprint
                params$stat_line_style$median_thickness,
                params$stat_line_style$median_width,
                params$stat_line_style$sd_thickness,
                params$stat_line_style$sd_width,
                # Axis style fingerprint
                params$axis_style$tick_length,
                params$axis_style$line_thickness,
                sep = "|"
            )
        }
        
        # Observer that updates cache only when values change
        shiny::observe({
            # Capture all values
            data_val <- filtered_data()
            x_val <- selected_x_axis()
            tt_val <- selected_tooltip_cols()
            trim_val <- trim_percent()
            outlier_val <- outlier_options()
            color_cols_val <- selected_color_cols()
            color_map_val <- custom_color_map()
            win_val <- window_size()
            point_style_val <- point_style()
            grid_legend_val <- grid_legend_options()
            stat_line_val <- stat_line_style()
            axis_style_val <- axis_style()
            
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
            
            # Compare fingerprints to detect actual changes
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
        }) |> shiny::debounce(350)  # Debounce the observer, not the reactive
        
        # Expose cached params as reactive
        plot_params <- shiny::reactive({
            cached_plot_params()
        })
        
        # Setup plot outputs using injected component
        # Following explicit dependency injection pattern
        setup_plot_outputs(
            output = output,
            ns = ns,
            plot_params = plot_params,
            measure_cols = selected_measures,
            create_scatter_plot = create_scatter_plot,
            export_width = export_width,
            export_height = export_height
        )
        
        # Render the plots UI container
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
            
            # Generate plot grid using injected component
            generate_plot_grid_ui(
                ns = ns,
                measures = measures,
                plot_height = "400px"
            )
        })
        
        # Reactive: get unique color groups based on interaction of selected color columns
        color_groups <- shiny::reactive({
            data <- filtered_data()
            color_cols <- selected_color_cols()
            
            if (is.null(data) || nrow(data) == 0 || is.null(color_cols) || length(color_cols) == 0) {
                return(character(0))
            }
            
            # Use create_interaction to get unique group levels
            interaction_factor <- create_interaction(data, color_cols)
            sort(as.character(unique(interaction_factor)))
        })
        
        # Reactive: collect custom colors from dynamic color picker inputs
        # No debounce here - consolidated in plot_params (global debounce)
        custom_color_map <- shiny::reactive({
            groups <- color_groups()
            if (length(groups) == 0) return(NULL)
            
            # Build named vector of colors from inputs
            colors <- sapply(groups, function(group) {
                input_id <- paste0("color_", gsub("[^[:alnum:]]", "_", group))
                color <- input[[input_id]]
                if (is.null(color)) {
                    # Return default if input not yet created
                    NA_character_
                } else {
                    color
                }
            })
            names(colors) <- groups
            
            # Fill in NA values with default palette
            na_idx <- is.na(colors)
            if (any(na_idx)) {
                n_na <- sum(na_idx)
                default_colors <- if (length(groups) <= 8) {
                    scales::hue_pal()(length(groups))
                } else {
                    grDevices::colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
                                                  "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"))(length(groups))
                }
                colors[na_idx] <- default_colors[na_idx]
            }
            
            colors
        })  # Debouncing handled by consolidated plot_params reactive
        
        # Render dynamic color pickers for unique groups in the data
        output$colorPickers <- shiny::renderUI({
            data <- filtered_data()
            color_cols <- selected_color_cols()
            
            # Need data and color column selection
            if (is.null(data) || nrow(data) == 0 || is.null(color_cols) || length(color_cols) == 0) {
                return(shiny::tags$p(
                    class = "text-muted small fst-italic",
                    "Select X-Axis columns to customize group colors."
                ))
            }
            
            # Get unique groups using the reactive
            groups <- color_groups()
            
            if (length(groups) == 0) {
                return(shiny::tags$p(class = "text-muted small", "No groups found."))
            }
            
            # Generate a default color palette (or use existing custom colors)
            existing_colors <- custom_color_map()
            default_colors <- if (length(groups) <= 8) {
                scales::hue_pal()(length(groups))
            } else {
                grDevices::colorRampPalette(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
                                              "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"))(length(groups))
            }
            
            # Create color pickers in a responsive grid
            num_cols <- min(3, length(groups))
            col_width <- 12 / num_cols
            
            color_inputs <- lapply(seq_along(groups), function(i) {
                group <- groups[i]
                input_id <- paste0("color_", gsub("[^[:alnum:]]", "_", group))
                
                # Use existing color if available, otherwise default
                current_color <- if (!is.null(existing_colors) && group %in% names(existing_colors)) {
                    existing_colors[[group]]
                } else {
                    default_colors[i]
                }
                
                shiny::column(
                    width = col_width,
                    colourpicker::colourInput(
                        inputId = ns(input_id),
                        label = group,
                        value = current_color,
                        showColour = "both",      # Show color swatch AND hex text input
                        allowTransparent = FALSE,
                        closeOnClick = TRUE
                    )
                )
            })
            
            shiny::fluidRow(color_inputs)
        })
        
        # Download handler for filtered data (Excel format)
        output$downloadData <- shiny::downloadHandler(
            filename = function() {
                # Create descriptive filename with date and selected X-axis columns
                x_cols <- input$xAxis
                x_suffix <- if (!is.null(x_cols) && length(x_cols) > 0) {
                    paste0("_", paste(x_cols, collapse = "-"))
                } else {
                    ""
                }
                paste0("filtered_data_", Sys.Date(), x_suffix, ".xlsx")
            },
            content = function(file) {
                data <- filtered_data()
                if (is.null(data) || nrow(data) == 0) {
                    # Create empty workbook with message if no data
                    wb <- openxlsx::createWorkbook()
                    openxlsx::addWorksheet(wb, "No Data")
                    openxlsx::writeData(wb, "No Data", "No filtered data available.")
                    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
                    return()
                }
                # Write data to Excel
                openxlsx::write.xlsx(data, file, rowNames = FALSE)
            }
        )
    })
}
