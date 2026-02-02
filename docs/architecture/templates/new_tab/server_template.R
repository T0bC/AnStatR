#' Server module for {TabName} page
#'
#' Handles all {TabName}-related server logic.
#'
#' @param id Module namespace ID
#' @param input_data Reactive containing data from upstream module
#' @param data_version Reactive integer that increments when new data is loaded
#' @return NULL (side effects only) or list of reactives for downstream modules
#'
#' USAGE: Copy this file to R/server/modules/pages/server_{tabname}.R
#'        Replace all {tabname} with your tab name (lowercase)
#'        Replace all {TabName} with your tab name (TitleCase)
server_{tabname} <- function(id, input_data, data_version) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Reactive values for module state
        state <- shiny::reactiveValues(
            result = NULL,
            last_computation = NULL
        )
        
        # Reset state when data version changes (new data loaded)
        shiny::observeEvent(data_version(), {
            state$result <- NULL
            state$last_computation <- NULL
        }, ignoreInit = TRUE)
        
        # Example: Update input choices based on data
        shiny::observe({
            data <- input_data()
            if (is.null(data)) return()
            
            # Update selectize choices
            shiny::updateSelectizeInput(
                session,
                "input1",
                choices = names(data),
                selected = NULL
            )
        })
        
        # Action button handler
        shiny::observeEvent(input$action_button, {
            # Validate inputs
            shiny::req(input_data())
            
            # Show notification
            shiny::showNotification(
                "Processing...",
                type = "message",
                duration = 2
            )
            
            # Perform computation
            # state$result <- compute_something(input_data(), input$input1)
        })
        
        # Render main output
        output${tabname}_results <- shiny::renderUI({
            if (is.null(state$result)) {
                # Placeholder when no results
                shiny::div(
                    class = "d-flex align-items-center justify-content-center h-100",
                    style = "min-height: 400px;",
                    shiny::div(
                        class = "text-center text-muted",
                        shiny::h4("{TabName} Results"),
                        shiny::p("Configure options and click the action button to generate results.")
                    )
                )
            } else {
                # Render actual results
                shiny::div(
                    # Your result UI here
                )
            }
        })
        
        # Return NULL or list of reactives for downstream modules
        invisible(NULL)
    })
}
