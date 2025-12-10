server_median <- function(id, loaded_data, data_version = NULL) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Shared reactive values
        filtered_data <- shiny::reactiveVal(NULL)
        filter_message <- shiny::reactiveVal("No data loaded.")
        median_results <- shiny::reactiveVal(NULL)
        removed_cols <- shiny::reactiveVal(NULL)  # Columns removed due to within-group variation

        # Reset all state when new data is loaded
        # This prevents stale selections from causing errors with new datasets
        if (!is.null(data_version)) {
            shiny::observeEvent(data_version(), {
                # Reset all reactive values to initial state
                filtered_data(NULL)
                filter_message("New data loaded. Configure grouping and filtering options.")
                median_results(NULL)
                removed_cols(NULL)
                
                # Reset UI inputs using updateSelectizeInput
                shiny::updateSelectizeInput(session, "grouping_columns", selected = character(0))
                shiny::updateSelectizeInput(session, "quality_column", selected = "None")
                shiny::updateSelectizeInput(session, "bad_quality_values", selected = character(0))
            }, ignoreInit = TRUE)
        }

        # Initialize modular components with explicit parameters
        # Following the explicit dependency injection pattern...

        # Help button handler
        handle_help_button(
            input = input,
            session = session
        )

        # Grouping UI renderer (no longer updates a separate reactiveVal)
        render_grouping_ui(
            output = output,
            output_id = "grouping_ui",
            loaded_data = loaded_data,
            input = input,
            session = session
        )

        # Quality filter UI renderer (returns quality_col_info reactive)
        quality_col_info <- render_quality_filter_ui(
            output = output,
            output_id = "quality_filter_ui",
            loaded_data = loaded_data,
            input = input,
            session = session
        )

        # ----- Unified Debounced Parameters -----
        # Consolidates grouping and quality inputs with single debounce
        # Prevents double-renders when multiple inputs change
        median_params <- create_median_params(
            loaded_data = loaded_data,
            input = input,
            quality_col_info = quality_col_info,
            debounce_ms = 400
        )
        
        # Extract individual values from unified params for downstream use
        selected_grouping_cols <- shiny::reactive({
            params <- median_params()
            if (is.null(params)) return(NULL)
            params$grouping_cols
        })
        
        quality_settings <- shiny::reactive({
            params <- median_params()
            if (is.null(params)) return(list(enabled = FALSE, column = NULL, type = "none"))
            params$quality_settings
        })

        # Apply quality filtering when unified params change
        shiny::observe({
            shiny::req(loaded_data(), median_params())
            
            data <- loaded_data()
            params <- median_params()
            
            # Apply quality filter
            result <- apply_quality_filter(data, params$quality_settings, params$grouping_cols)
            
            filtered_data(result$data)
            filter_message(result$message)
        })

        # Render filtering message
        output$filteringMessage <- shiny::renderUI({
            msg <- filter_message()
            data <- filtered_data()
            grouping <- selected_grouping_cols()
            removed <- removed_cols()
            
            if (is.null(data)) {
                return(shiny::tags$div(
                    class = "alert alert-secondary",
                    "Load data to begin filtering."
                ))
            }
            
            # Split message into lines for display
            msg_lines <- strsplit(msg, "\n")[[1]]
            
            # Build grouping info
            grouping_info <- if (is.null(grouping) || length(grouping) == 0) {
                shiny::tags$p(
                    class = "mb-1",
                    shiny::tags$em("No grouping selected - showing filtered data without median calculation.")
                )
            } else {
                shiny::tags$p(
                    class = "mb-1",
                    shiny::tags$strong("Grouping by: "), 
                    paste(grouping, collapse = ", ")
                )
            }
            
            # Build removed columns info
            removed_info <- if (!is.null(removed) && length(removed) > 0) {
                shiny::tags$p(
                    class = "mb-1 text-warning",
                    shiny::tags$strong("Columns removed (vary within groups): "),
                    paste(removed, collapse = ", ")
                )
            } else {
                NULL
            }
            
            shiny::tags$div(
                class = "alert alert-info",
                shiny::tags$strong("Processing Summary"),
                shiny::tags$hr(class = "my-2"),
                grouping_info,
                removed_info,
                shiny::tags$hr(class = "my-2"),
                shiny::tags$strong("Quality Filter: "),
                lapply(msg_lines, function(line) {
                    shiny::tags$span(line, shiny::tags$br())
                })
            )
        })

        # Reactive for quality column name (extracted from quality_settings)
        quality_col_name <- shiny::reactive({
            settings <- quality_settings()
            if (settings$enabled && !is.null(settings$column)) {
                settings$column
            } else {
                NULL
            }
        })

        # Median table renderer
        render_median_table(
            output = output,
            output_id = "medianTable",
            filtered_data = filtered_data,
            grouping_cols = selected_grouping_cols,
            quality_col = quality_col_name,
            median_results = median_results,
            removed_cols = removed_cols
        )

        # Return reactive with median results
        shiny::reactive({
            median_results()
        })
    })
}
