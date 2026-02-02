#' PCA Computation Handler
#'
#' Handles KMO and PCA computation with validation and progress feedback.
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
        
        # Compute with unified progress for all PCA steps
        shiny::withProgress(message = "Preparing data...", value = 0, {
            # Step 1: Prepare data (10%)
            prep_result <- prepare_pca_data(
                data = data,
                measure_cols = measure_cols,
                scale = isTRUE(input$scale_data)
            )
            
            prepared_data <- prep_result$data
            
            # Check if we have enough data after NA removal
            if (nrow(prepared_data) < 2) {
                pca_state$kmo_result <- simple_error(
                    "Not enough complete observations after removing rows with missing values.",
                    operation_name = "PCA Validation",
                    context = list(
                        original_rows = prep_result$original_rows,
                        rows_removed = prep_result$rows_removed,
                        remaining_rows = nrow(prepared_data)
                    )
                )
                return()
            }
            
            shiny::setProgress(0.1, message = "Computing correlation matrix...")
            
            # Step 2: Compute correlation matrix (30%)
            corr_cols <- names(prepared_data)
            error_context <- list(
                n_variables = length(corr_cols),
                variables = paste(corr_cols, collapse = ", "),
                n_observations = nrow(prepared_data)
            )
            
            correlation_result <- safe_execute(
                expr = compute_correlation_data(prepared_data, corr_cols),
                operation_name = "Correlation Plot",
                context = error_context,
                error_parser = correlation_error_parser
            )
            
            pca_state$correlation_result <- correlation_result
            pca_state$prepared_data <- prepared_data
            
            shiny::setProgress(0.3, message = "Computing KMO measures...")
            
            # Step 3: Compute KMO (50%)
            kmo_result <- calculate_kmo(prepared_data)
            
            # Add info about removed rows to KMO result if any were removed
            if (!is_app_error(kmo_result) && prep_result$rows_removed > 0) {
                kmo_result$rows_removed <- prep_result$rows_removed
                kmo_result$original_rows <- prep_result$original_rows
            }
            
            pca_state$kmo_result <- kmo_result
            
            shiny::setProgress(0.5, message = "Computing PCA...")
            
            # If KMO failed, skip PCA computation
            if (is_app_error(kmo_result)) {
                pca_state$pca_result <- NULL
                pca_state$last_computation <- Sys.time()
                return()
            }
            
            # Step 4: Compute PCA (100%)
            pca_result <- calculate_pca(prepared_data)
            
            shiny::setProgress(0.9, message = "Finalizing...")
            
            pca_state$pca_result <- pca_result
            pca_state$last_computation <- Sys.time()
            
            shiny::setProgress(1.0)
        })
    })
}


#' Calculate PCA using FactoMineR
#'
#' @param data Data frame with numeric columns (already prepared and scaled)
#' @param ncp Number of principal components to compute (default: 5)
#' @return PCA result object from FactoMineR, or structured error
calculate_pca <- function(data, ncp = 5) {
    error_context <- list(
        n_variables = ncol(data),
        n_observations = nrow(data),
        variables = paste(names(data), collapse = ", ")
    )
    
    # Limit ncp to number of variables - 1
    max_ncp <- min(ncp, ncol(data) - 1, nrow(data) - 1)
    
    result <- safe_execute(
        expr = FactoMineR::PCA(
            data,
            scale.unit = FALSE,  # Data already scaled in prepare_pca_data
            ncp = max_ncp,
            graph = FALSE
        ),
        operation_name = "PCA",
        context = error_context,
        error_parser = pca_error_parser
    )
    
    if (!result$success) return(result$error)
    
    result$result
}


#' Error parser for PCA-specific errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
pca_error_parser <- function(error_msg, operation_name = "PCA") {
    if (grepl("singular|invertible", error_msg, ignore.case = TRUE)) {
        "PCA: Data matrix is singular. Remove highly correlated or constant variables."
    } else if (grepl("NA|missing|NaN", error_msg, ignore.case = TRUE)) {
        "PCA: Data contains missing values. Please handle missing data first."
    } else if (grepl("numeric", error_msg, ignore.case = TRUE)) {
        "PCA: All selected columns must be numeric."
    } else if (grepl("ncp|dimension", error_msg, ignore.case = TRUE)) {
        "PCA: Invalid number of components. Check your data dimensions."
    } else {
        paste0("PCA calculation failed: ", error_msg)
    }
}
