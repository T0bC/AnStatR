#' Server module for PCA page
#'
#' Handles all PCA-related server logic.
#'
#' @param id Module namespace ID
#' @param median_data Reactive containing median-processed data from median module
#' @param data_version Reactive integer that increments when new data is loaded
#' @return NULL (side effects only)
#' @export
server_pca <- function(id, median_data, data_version) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Import components
        box::use(../../utils/column_utils)
        box::use(../pca/pca_utils)
        box::use(../../utils/error_handling)
        box::use(../../ui/components/error_display)
        box::use(../pca/kmo_results)
        box::use(../pca/kmo_computation)
        box::use(../pca/pca_computation)
        box::use(../pca/correlation_plot)
        box::use(../pca/pca_results)
        box::use(../pca/optimal_components)
        box::use(../pca/optimal_components_results)
        
        # Reactive values for PCA state
        pca_state <- shiny::reactiveValues(
            pca_result = NULL,
            kmo_result = NULL,
            prepared_data = NULL,
            correlation_result = NULL,
            optimal_result = NULL,
            last_computation = NULL
        )
        
        # Get available columns from median data using column naming conventions
        # Matches the Plotting tab behavior: descriptive = UPPERCASE, measurement = mixed case
        available_cols <- shiny::reactive({
            data <- median_data()
            if (is.null(data)) return(list(descriptive = NULL, measurement = NULL))
            
            list(
                descriptive = column_utils$get_descriptive_cols(data),
                measurement = column_utils$get_measurement_cols(data)
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
            pca_state$correlation_result <- NULL
            pca_state$optimal_result <- NULL
            pca_state$last_computation <- NULL
        }, ignoreInit = TRUE)
        
        # PCA computation handler
        pca_computation$handle_pca_computation(
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
            if (error_handling$is_app_error(kmo)) {
                return(error_display$error_alert_structured(kmo, type = "danger"))
            }
            
            # Check correlation plot result for errors
            corr_result <- correlation_plot_result()
            corr_content <- if (!is.null(corr_result) && !corr_result$success) {
                # Show error instead of plot
                shiny::div(
                    class = "p-3",
                    error_display$error_alert_structured(corr_result$error, type = "danger")
                )
            } else {
                # Show the plot
                ggiraph::girafeOutput(ns("correlation_plot"), height = "500px")
            }
            
            # Check PCA result
            pca <- pca_state$pca_result
            pca_content <- NULL
            
            if (!is.null(pca)) {
                if (error_handling$is_app_error(pca)) {
                    pca_content <- error_display$error_alert_structured(pca, type = "danger")
                } else {
                    pca_content <- pca_results$render_pca_results(pca, ns)
                }
            }
            
            # Success - render results with visual grouping
            shiny::tagList(
                # Section 1: Data Info (rows removed message)
                shiny::tags$div(
                    class = "mb-2",
                    # Extract and show rows removed info from KMO result
                    if (!is.null(kmo$rows_removed) && kmo$rows_removed > 0) {
                        shiny::tags$div(
                            class = "alert alert-info d-flex align-items-center",
                            role = "alert",
                            bsicons::bs_icon("info-circle-fill", class = "me-1"),
                            sprintf(
                                "%d of %d rows were excluded due to missing values in selected columns.",
                                kmo$rows_removed,
                                kmo$original_rows
                            )
                        )
                    }
                ),
                
                # Section 2: Correlation Matrix
                shiny::tags$div(
                    class = "mb-2",
                    bslib::accordion(
                        id = "correlation_accordion",
                        open = FALSE,
                        bslib::accordion_panel(
                            title = shiny::tags$span(
                                bsicons::bs_icon("grid-3x3", class = "me-1"),
                                "Correlation Matrix"
                            ),
                            value = "correlation_matrix",
                            corr_content
                        )
                    )
                ),
                
                # Section 3: KMO Results
                shiny::tags$div(
                    class = "mb-2",
                    kmo_results$render_kmo_results(kmo)
                ),
                
                # Section 4: Optimal Components
                shiny::tags$div(
                    class = "mb-2",
                    bslib::accordion(
                        id = "optimal_components_accordion",
                        open = FALSE,
                        bslib::accordion_panel(
                            title = shiny::tags$span(
                                bsicons::bs_icon("sliders", class = "me-1"),
                                "Optimal Number of Components"
                            ),
                            value = "optimal_components",
                            optimal_components_results$render_optimal_components_content(pca_state$optimal_result, ns)
                        )
                    )
                ),
                
                # Section 5: PCA Results (grouped)
                shiny::tags$div(
                    class = "mb-2",
                    shiny::tags$h5(
                        class = "text-muted mb-2",
                        bsicons::bs_icon("bar-chart-line", class = "me-1"),
                        "PCA Results"
                    ),
                    pca_content
                )
            )
        })
        
        # Reactive to access pre-computed correlation data from pca_state
        # Computation now happens in handle_pca_computation with unified progress
        correlation_plot_result <- shiny::reactive({
            pca_state$correlation_result
        })
        
        # Render optimal components scree plot
        output$optimal_scree_plot <- ggiraph::renderGirafe({
            optimal <- pca_state$optimal_result
            
            if (is.null(optimal) || error_handling$is_app_error(optimal)) {
                return(NULL)
            }
            
            optimal_components_results$render_optimal_scree_girafe(optimal)
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
            correlation_plot$render_correlation_girafe(result$result)
        })
        
        # Download handler for Excel export
        output$download_pca_excel <- shiny::downloadHandler(
            filename = function() {
                paste0("pca_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
            },
            content = function(file) {
                pca <- pca_state$pca_result
                shiny::req(pca)
                shiny::req(!error_handling$is_app_error(pca))
                pca_results$createPCAExcelOutput(pca, file)
            }
        )
        
        # Download handler for RDS export (PCA object for later use)
        output$download_pca_rda <- shiny::downloadHandler(
            filename = function() {
                paste0("pca_object_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
            },
            content = function(file) {
                pca <- pca_state$pca_result
                shiny::req(pca)
                shiny::req(!error_handling$is_app_error(pca))
                saveRDS(pca, file)
            }
        )
        
        # Return NULL - no outputs needed for downstream modules yet
        invisible(NULL)
    })
}
