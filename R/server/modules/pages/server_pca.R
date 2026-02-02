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
        
        # Source components
        source("R/server/modules/pages/pca/pca_utils.R", local = TRUE)
        source("R/utils/error_handling.R", local = TRUE)
        source("R/ui/modules/components/error_display.R", local = TRUE)
        source("R/server/modules/pages/pca/kmo_results.R", local = TRUE)
        source("R/server/modules/pages/pca/kmo_computation.R", local = TRUE)
        source("R/server/modules/pages/pca/pca_computation.R", local = TRUE)
        source("R/server/modules/pages/pca/correlation_plot.R", local = TRUE)
        
        # Reactive values for PCA state
        pca_state <- shiny::reactiveValues(
            pca_result = NULL,
            kmo_result = NULL,
            prepared_data = NULL,
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
            pca_state$kmo_result <- NULL
            pca_state$prepared_data <- NULL
            pca_state$last_computation <- NULL
        }, ignoreInit = TRUE)
        
        # PCA computation handler
        handle_pca_computation(
            input = input,
            median_data = median_data,
            pca_state = pca_state
        )
        
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
        
        # Render PCA results
        output$pca_results <- shiny::renderUI({
            kmo <- pca_state$kmo_result
            
            # No computation yet
            if (is.null(kmo)) {
                return(shiny::div(
                    class = "d-flex align-items-center justify-content-center h-100",
                    style = "min-height: 400px;",
                    shiny::div(
                        class = "text-center text-muted",
                        shiny::h4("PCA Results"),
                        shiny::p("Select data columns and click 'Compute PCA' to generate results."),
                        shiny::p(
                            class = "small",
                            "Results will include KMO measures, biplots, scree plots, and contribution charts."
                        )
                    )
                ))
            }
            
            # Error case
            if (is_app_error(kmo)) {
                return(error_alert_structured(kmo, type = "danger"))
            }
            
            # Check correlation plot result for errors
            corr_result <- correlation_plot_result()
            corr_content <- if (!is.null(corr_result) && !corr_result$success) {
                # Show error instead of plot
                shiny::div(
                    class = "p-3",
                    error_alert_structured(corr_result$error, type = "danger")
                )
            } else {
                # Show the plot
                ggiraph::girafeOutput(ns("correlation_plot"), height = "500px")
            }
            
            # Success - render KMO results and correlation plot/error
            shiny::tagList(
                render_kmo_results(kmo),
                
                # Correlation Matrix accordion (new, closed by default)
                bslib::accordion(
                    id = "correlation_accordion",
                    open = FALSE,
                    bslib::accordion_panel(
                        title = shiny::tags$span(
                            bsicons::bs_icon("grid-3x3", class = "me-2"),
                            "Correlation Matrix"
                        ),
                        value = "correlation_matrix",
                        corr_content
                    )
                )
            )
        })
        
        # Reactive to store correlation data computation result or error
        # This separates computation (which may fail) from rendering (which should not fail)
        # Note: prepared_data is already cleaned (NA rows removed) and contains only measurement columns
        correlation_plot_result <- shiny::reactive({
            prepared <- pca_state$prepared_data
            
            if (is.null(prepared) || ncol(prepared) < 2) {
                return(NULL)
            }
            
            # Use column names from prepared data (already subset to measurement cols)
            measure_cols <- names(prepared)
            
            error_context <- list(
                n_variables = length(measure_cols),
                variables = paste(measure_cols, collapse = ", "),
                n_observations = nrow(prepared)
            )
            
            # Wrap only the computation in safe_execute - rendering happens separately
            result <- safe_execute(
                expr = compute_correlation_data(prepared, measure_cols),
                operation_name = "Correlation Plot",
                context = error_context,
                error_parser = correlation_error_parser
            )
            
            return(result)
        })
        
        # Render correlation plot - only renders if computation succeeded
        output$correlation_plot <- ggiraph::renderGirafe({
            result <- correlation_plot_result()
            
            if (is.null(result)) {
                return(NULL)
            }
            
            if (!result$success) {
                # Return NULL - the error is displayed in the UI via error_alert_structured
                return(NULL)
            }
            
            # Render the pre-computed correlation data
            # This should not fail since all validation happened in compute_correlation_data
            render_correlation_girafe(result$result)
        })
        
        # Return NULL - no outputs needed for downstream modules yet
        invisible(NULL)
    })
}
