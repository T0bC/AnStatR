#' PCA Computation Handler
#'
#' Handles KMO computation with validation and progress feedback.
#'
#' @param input Shiny input object from parent module
#'   - input$measureVar: Selected measurement columns
#'   - input$scale_data: Whether to scale data
#' @param median_data Reactive containing the source data
#' @param pca_state ReactiveValues to store computation results
#' @return NULL (side effects only)
handle_pca_computation <- function(input, median_data, pca_state) {
    shiny::observeEvent(input$compute_pca_button, {
        data <- median_data()
        measure_cols <- input$measureVar
        
        # Validation
        if (is.null(data)) {
            pca_state$kmo_result <- simple_error(
                "No data available. Please load data first.",
                operation_name = "PCA Validation"
            )
            return()
        }
        
        if (is.null(measure_cols) || length(measure_cols) < 2) {
            pca_state$kmo_result <- simple_error(
                "Please select at least 2 measurement columns for PCA.",
                operation_name = "PCA Validation"
            )
            return()
        }
        
        # Check for missing values
        pca_subset <- data[, measure_cols, drop = FALSE]
        if (any(is.na(pca_subset))) {
            pca_state$kmo_result <- simple_error(
                "Selected columns contain missing values. Please handle missing data first.",
                operation_name = "PCA Validation",
                context = list(columns_with_na = names(which(colSums(is.na(pca_subset)) > 0)))
            )
            return()
        }
        
        # Compute with progress
        shiny::withProgress(message = "Computing KMO measures...", {
            prepared_data <- prepare_pca_data(
                data = data,
                measure_cols = measure_cols,
                scale = isTRUE(input$scale_data)
            )
            
            shiny::incProgress(0.3)
            
            kmo_result <- calculate_kmo(prepared_data)
            
            shiny::incProgress(0.7)
            
            pca_state$kmo_result <- kmo_result
            pca_state$prepared_data <- prepared_data
            pca_state$last_computation <- Sys.time()
        })
    })
}
