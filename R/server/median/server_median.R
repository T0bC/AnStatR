# Import help modal module
box::use(./help_modal)
# Import grouping UI module
box::use(../../ui/median/grouping_ui)
# Import quality filter UI module
box::use(../../ui/median/quality_filter_ui)
# Import median params module
box::use(./median_params)
# Import quality filter logic module
box::use(./quality_filter_logic)
# Import median table module
box::use(./median_table)
# Import column utils module
box::use(../../utils/column_utils)

#' @export
server_median <- function(id, loaded_data, data_version = NULL) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Shared reactive values
        filtered_data <- shiny::reactiveVal(NULL)
        filter_message <- shiny::reactiveVal("No data loaded.")
        median_results <- shiny::reactiveVal(NULL)
        removed_cols <- shiny::reactiveVal(NULL)  # Columns removed due to within-group variation

        # Store previous column names to enable smart selection retention
        previous_descriptive_cols <- shiny::reactiveVal(character(0))
        
        # Reset state and update UI choices when new data is loaded
        # Smart selection retention: keep selections for columns that exist in both datasets
        if (!is.null(data_version)) {
            shiny::observeEvent(data_version(), {
                # Reset all reactive values to initial state
                filtered_data(NULL)
                filter_message("New data loaded. Configure grouping and filtering options.")
                median_results(NULL)
                removed_cols(NULL)
                
                # Get new descriptive columns from the new dataset
                new_data <- loaded_data()
                if (!is.null(new_data)) {
                    new_cols <- column_utils$get_descriptive_cols(new_data)
                    old_cols <- previous_descriptive_cols()
                    
                    # Smart selection retention for grouping columns
                    current_grouping <- shiny::isolate(input$grouping_columns)
                    retained_grouping <- if (!is.null(current_grouping)) {
                        intersect(current_grouping, new_cols)
                    } else {
                        character(0)
                    }
                    
                    # Smart selection retention for quality column
                    current_quality <- shiny::isolate(input$quality_column)
                    retained_quality <- if (!is.null(current_quality) && 
                                           current_quality != "None" && 
                                           current_quality %in% new_cols) {
                        current_quality
                    } else {
                        "None"
                    }
                    
                    # Update selectize inputs with new choices and retained selections
                    shiny::updateSelectizeInput(
                        session, "grouping_columns",
                        choices = new_cols,
                        selected = retained_grouping
                    )
                    shiny::updateSelectizeInput(
                        session, "quality_column",
                        choices = c("None (no quality filtering)" = "None", new_cols),
                        selected = retained_quality
                    )
                    # Always reset bad_quality_values since they depend on quality column content
                    shiny::updateSelectizeInput(session, "bad_quality_values", selected = character(0))
                    
                    # Update stored previous columns for next data load
                    previous_descriptive_cols(new_cols)
                } else {
                    # No data - reset everything
                    shiny::updateSelectizeInput(session, "grouping_columns", choices = character(0), selected = character(0))
                    shiny::updateSelectizeInput(session, "quality_column", choices = c("None (no quality filtering)" = "None"), selected = "None")
                    shiny::updateSelectizeInput(session, "bad_quality_values", selected = character(0))
                    previous_descriptive_cols(character(0))
                }
            }, ignoreInit = TRUE)
        }
        
        # Initialize previous_descriptive_cols when data is first loaded
        shiny::observeEvent(loaded_data(), {
            data <- loaded_data()
            if (!is.null(data) && length(previous_descriptive_cols()) == 0) {
                previous_descriptive_cols(column_utils$get_descriptive_cols(data))
            }
        }, once = TRUE)

        # Initialize modular components with explicit parameters
        # Following the explicit dependency injection pattern...

        # Help button handler
        help_modal$handle_help_button(
            input = input,
            session = session
        )

        # Grouping UI renderer (no longer updates a separate reactiveVal)
        grouping_ui$render_grouping_ui(
            output = output,
            output_id = "grouping_ui",
            loaded_data = loaded_data,
            input = input,
            session = session
        )

        # Quality filter UI renderer (returns quality_col_info reactive)
        quality_col_info <- quality_filter_ui$render_quality_filter_ui(
            output = output,
            output_id = "quality_filter_ui",
            loaded_data = loaded_data,
            input = input,
            session = session
        )

        # ----- Unified Debounced Parameters -----
        # Consolidates grouping and quality inputs with single debounce
        # Prevents double-renders when multiple inputs change
        median_params <- median_params$create_median_params(
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
        # Uses observeEvent on median_params() to prevent race conditions
        shiny::observeEvent(median_params(), {
            params <- median_params()
            shiny::req(params)
            
            # Isolate loaded_data to prevent double-triggering
            data <- shiny::isolate(loaded_data())
            shiny::req(data)
            
            # Apply quality filter
            result <- quality_filter_logic$apply_quality_filter(data, params$quality_settings, params$grouping_cols)
            
            filtered_data(result$data)
            filter_message(result$message)
        }, ignoreNULL = TRUE, ignoreInit = FALSE)

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
        median_table$render_median_table(
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
