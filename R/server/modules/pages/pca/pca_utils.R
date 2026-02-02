#' PCA Utility Functions
#'
#' Provides data preparation and KMO calculation for PCA analysis.

#' Prepare data for PCA analysis
#'
#' Subsets to measurement columns, removes rows with any NA values,
#' and optionally scales the data. The cleaned data is used for all
#' subsequent PCA calculations (KMO, correlation plot, etc.).
#'
#' @param data Data frame with measurement columns
#' @param measure_cols Character vector of column names to include
#' @param scale Logical, whether to scale the data
#' @return List with:
#'   - data: Data frame with selected columns, NA rows removed, optionally scaled
#'   - rows_removed: Number of rows removed due to NA values
#'   - original_rows: Original number of rows
prepare_pca_data <- function(data, measure_cols, scale = TRUE) {
    pca_data <- data[, measure_cols, drop = FALSE]
    original_rows <- nrow(pca_data)
    
    # Remove rows with any NA in measurement columns
    complete_rows <- complete.cases(pca_data)
    pca_data <- pca_data[complete_rows, , drop = FALSE]
    rows_removed <- original_rows - nrow(pca_data)
    
    if (scale && nrow(pca_data) > 0) {
        pca_data <- as.data.frame(scale(pca_data))
    }
    
    list(
        data = pca_data,
        rows_removed = rows_removed,
        original_rows = original_rows
    )
}

#' Calculate KMO measure
#'
#' @param data Data frame with numeric columns (already prepared)
#' @return List with overall KMO and individual variable KMOs, or structured error
calculate_kmo <- function(data) {
    error_context <- list(
        n_variables = ncol(data),
        n_observations = nrow(data),
        variables = paste(names(data), collapse = ", ")
    )
    
    result <- safe_execute(
        expr = psych::KMO(data),
        operation_name = "KMO",
        context = error_context,
        error_parser = kmo_error_parser
    )
    
    if (!result$success) return(result$error)
    
    kmo <- result$result
    list(
        overall = kmo$MSA,
        individual = kmo$MSAi
    )
}

#' Error parser for KMO-specific errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
kmo_error_parser <- function(error_msg, operation_name = "KMO") {
    if (grepl("singular|invertible", error_msg, ignore.case = TRUE)) {
        "KMO: Correlation matrix is singular. Remove highly correlated or constant variables."
    } else if (grepl("NA|missing", error_msg, ignore.case = TRUE)) {
        "KMO: Data contains missing values. Please handle missing data first."
    } else if (grepl("numeric", error_msg, ignore.case = TRUE)) {
        "KMO: All selected columns must be numeric."
    } else {
        paste0("KMO calculation failed: ", error_msg)
    }
}
