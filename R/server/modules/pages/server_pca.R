#' Server module for PCA page
#'
#' Handles all PCA-related server logic.
#'
#' @param id Module namespace ID
#' @param median_data Reactive containing median-processed data from median module
#' @param data_version Reactive integer that increments when new data is loaded
#' @return NULL (side effects only)
server_pca <- function(id, median_data, data_version) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Reactive values for PCA state
        pca_state <- shiny::reactiveValues(
            pca_result = NULL,
            last_computation = NULL
        )
        
        # Get available columns from median data
        available_cols <- shiny::reactive({
            data <- median_data()
            if (is.null(data)) return(list(descriptive = NULL, measurement = NULL))
            
            # Use column utility functions if available
            all_cols <- names(data)
            numeric_cols <- names(data)[sapply(data, is.numeric)]
            non_numeric_cols <- setdiff(all_cols, numeric_cols)
            
            list(
                descriptive = all_cols,
                measurement = numeric_cols
            )
        })
        
        # Update metadata column choices
        shiny::observe({
            cols <- available_cols()
            shiny::updateSelectizeInput(
                session,
                "metaData",
                choices = cols$descriptive,
                selected = NULL
            )
        })
        
        # Update measurement column choices (exclude selected metadata)
        shiny::observe({
            cols <- available_cols()
            selected_meta <- input$metaData
            available_measures <- setdiff(cols$measurement, selected_meta)
            
            shiny::updateSelectizeInput(
                session,
                "measureVar",
                choices = available_measures,
                selected = NULL
            )
        })
        
        # Update GroupBiplot choices based on selected metadata
        shiny::observe({
            selected_meta <- input$metaData
            shiny::updateSelectizeInput(
                session,
                "GroupBiplot",
                choices = selected_meta,
                selected = NULL
            )
        })
        
        # Reset state when data version changes
        shiny::observeEvent(data_version(), {
            pca_state$pca_result <- NULL
            pca_state$last_computation <- NULL
        }, ignoreInit = TRUE)
        
        # Placeholder for PCA computation (logic to be implemented later)
        shiny::observeEvent(input$compute_pca_button, {
            shiny::showNotification(
                "PCA computation will be implemented in a future update.",
                type = "message",
                duration = 3
            )
        })
        
        # Placeholder for help button
        shiny::observeEvent(input$helpButton, {
            shiny::showModal(shiny::modalDialog(
                title = "PCA Help",
                shiny::p("Principal Component Analysis (PCA) is a dimensionality reduction technique."),
                shiny::p("1. Select descriptive columns that identify your samples."),
                shiny::p("2. Select measurement columns containing numerical data for analysis."),
                shiny::p("3. Enable 'Scale Data' if variables have different units/magnitudes."),
                shiny::p("4. Click 'Compute PCA' to run the analysis."),
                easyClose = TRUE,
                footer = shiny::modalButton("Close")
            ))
        })
        
        # Render placeholder for PCA results
        output$pca_results <- shiny::renderUI({
            shiny::div(
                class = "d-flex align-items-center justify-content-center h-100",
                style = "min-height: 400px;",
                shiny::div(
                    class = "text-center text-muted",
                    shiny::h4("PCA Results"),
                    shiny::p("Select data columns and click 'Compute PCA' to generate results."),
                    shiny::p(
                        class = "small",
                        "Results will include biplots, scree plots, and contribution charts."
                    )
                )
            )
        })
        
        # Return NULL - no outputs needed for downstream modules yet
        invisible(NULL)
    })
}
