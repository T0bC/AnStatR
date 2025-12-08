# Data utility functions for data transformations
# These functions are used for creating interaction terms and marking trimmed data

#' Create interaction term from multiple columns
#'
#' Combines multiple factor columns into a single interaction term.
#' Useful for grouping data by multiple categorical variables.
#'
#' @param df Data frame containing the columns
#' @param cols Character vector of column names to combine
#' @return Factor vector representing the interaction of all specified columns
#' @examples
#' # Single column returns as factor
#' create_interaction(df, "SPECIES")
#' # Multiple columns create interaction
#' create_interaction(df, c("SPECIES", "DIET"))
create_interaction <- function(df, cols) {
    if (length(cols) == 0) {
        stop("At least one column must be provided.")
    }
    
    # Convert specified columns to factors
    factor_cols <- lapply(cols, function(col) as.factor(df[[col]]))
    
    # Single column: return as factor
    if (length(cols) == 1) {
        return(factor_cols[[1]])
    }
    
    # Multiple columns: create interaction term
    interaction(factor_cols, drop = TRUE)
}


#' Mark data points as trimmed or retained based on trim percentage
#'
#' For each group (defined by interaction_col), marks points that fall in the
#' extreme trim_percent from each end of the distribution as trimmed.
#' This allows visualizing which points would be excluded from a trimmed mean.
#'
#' @param data Data frame containing the data
#' @param value_col Character string of the column containing values to trim
#' @param group_col Character string or factor defining groups (from create_interaction)
#' @param trim_percent Numeric, percentage (0-100) to trim from EACH end
#' @return Data frame with additional columns:
#'   - .group: the grouping factor
#'   - .is_trimmed: logical, TRUE if point is trimmed (excluded)
#'   - .trim_rank: rank within group (for debugging)
mark_trimmed_data <- function(data, value_col, group_col, trim_percent = 0) {
    # Validate inputs
    if (!value_col %in% names(data)) {
        stop(paste("Column", value_col, "not found in data"))
    }
    
    # Convert trim_percent (0-100) to proportion (0-0.5)
    # trim_percent = 10 means remove 10% from each end
    trim_prop <- min(trim_percent / 100, 0.5)
    
    # Handle group_col - can be a column name or already a factor
    if (is.character(group_col) && length(group_col) == 1 && group_col %in% names(data)) {
        data$.group <- as.factor(data[[group_col]])
    } else if (is.factor(group_col) || is.character(group_col)) {
        data$.group <- as.factor(group_col)
    } else {
        # Single group for all data
        data$.group <- factor(rep("all", nrow(data)))
    }
    
    # Initialize columns
    data$.is_trimmed <- FALSE
    data$.trim_rank <- NA_integer_
    
    # If no trimming, return early
    if (trim_prop <= 0) {
        return(data)
    }
    
    # Process each group
    groups <- unique(data$.group)
    
    for (grp in groups) {
        idx <- which(data$.group == grp)
        n <- length(idx)
        
        if (n == 0) next
        
        # Number of values to trim from each end
        k <- floor(n * trim_prop)
        
        # Get values and their order
        values <- data[[value_col]][idx]
        order_idx <- order(values)
        
        # Assign ranks within group
        data$.trim_rank[idx] <- order_idx
        
        # Mark trimmed points (k lowest and k highest)
        if (k > 0) {
            # Indices within the group that are trimmed
            trimmed_positions <- c(
                order_idx[seq_len(k)],           # k lowest
                order_idx[(n - k + 1):n]         # k highest
            )
            # Map back to data frame indices
            data$.is_trimmed[idx[trimmed_positions]] <- TRUE
        }
    }
    
    return(data)
}
