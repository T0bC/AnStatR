server_median <- function(id, loaded_data, data_version = NULL) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Shared reactive values
        selected_grouping_cols <- shiny::reactiveVal(NULL)
        quality_settings <- shiny::reactiveVal(list(enabled = FALSE))
        filtered_data <- shiny::reactiveVal(NULL)
        filter_message <- shiny::reactiveVal("No data loaded.")
        median_results <- shiny::reactiveVal(NULL)

        # Source modular component functions
        source("R/server/modules/pages/median/help_modal.R", local = TRUE)
        source("R/server/modules/pages/median/grouping_ui.R", local = TRUE)
        source("R/server/modules/pages/median/quality_filter_ui.R", local = TRUE)
        source("R/server/modules/pages/median/quality_filter_logic.R", local = TRUE)
        source("R/server/modules/pages/median/median_table.R", local = TRUE)

        # Reset all state when new data is loaded
        # This prevents stale selections from causing errors with new datasets
        if (!is.null(data_version)) {
            shiny::observeEvent(data_version(), {
                # Reset all reactive values to initial state
                selected_grouping_cols(NULL)
                quality_settings(list(enabled = FALSE))
                filtered_data(NULL)
                filter_message("New data loaded. Configure grouping and filtering options.")
                median_results(NULL)
                
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

        # Grouping UI renderer
        render_grouping_ui(
            output = output,
            output_id = "grouping_ui",
            loaded_data = loaded_data,
            input = input,
            session = session,
            selected_grouping_cols = selected_grouping_cols
        )

        # Quality filter UI renderer
        render_quality_filter_ui(
            output = output,
            output_id = "quality_filter_ui",
            loaded_data = loaded_data,
            input = input,
            session = session,
            quality_settings = quality_settings
        )

        # Apply quality filtering when inputs change
        shiny::observe({
            shiny::req(loaded_data())
            
            data <- loaded_data()
            settings <- quality_settings()
            grouping <- selected_grouping_cols()
            
            # Apply quality filter
            result <- apply_quality_filter(data, settings, grouping)
            
            filtered_data(result$data)
            filter_message(result$message)
        })

        # Render filtering message
        output$filteringMessage <- shiny::renderUI({
            msg <- filter_message()
            data <- filtered_data()
            
            if (is.null(data)) {
                return(shiny::tags$div(
                    class = "alert alert-secondary",
                    "Load data to begin filtering."
                ))
            }
            
            # Split message into lines for display
            msg_lines <- strsplit(msg, "\n")[[1]]
            
            shiny::tags$div(
                class = "alert alert-info",
                shiny::tags$strong("Filter Results"),
                shiny::tags$br(),
                lapply(msg_lines, function(line) {
                    shiny::tags$span(line, shiny::tags$br())
                })
            )
        })

        # Median table renderer
        render_median_table(
            output = output,
            output_id = "medianTable",
            filtered_data = filtered_data,
            median_results = median_results
        )

        # Return reactive with median results
        shiny::reactive({
            median_results()
        })
    })
}
