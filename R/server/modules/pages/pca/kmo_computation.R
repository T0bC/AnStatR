#' KMO Computation Functions
#'
#' Provides KMO measure calculation with proper error handling.

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
