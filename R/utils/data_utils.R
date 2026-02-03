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
#' @export
create_interaction <- function(df, cols) {
    if (length(cols) == 0) {
        stop("At least one column must be provided.")
    }
    
    # Convert specified columns to factors, replacing NA with "NA" string to preserve rows
    # This ensures data points with missing descriptive values are still plotted
    factor_cols <- lapply(cols, function(col) {
        values <- df[[col]]
        # Replace NA with "NA" string before converting to factor
        values[is.na(values)] <- "NA"
        as.factor(values)
    })
    
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
#' @export
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


#' Detect outliers using various statistical methods
#'
#' Marks outliers in a measurement column based on the selected detection method.
#' Detection is performed within groups defined by the group column.
#'
#' @param data Data frame containing the data
#' @param value_col Character string of the column to check for outliers
#' @param group_col Factor or character vector defining groups (from create_interaction)
#' @param method Character, one of: "IQR", "zscore", "modified_zscore", 
#'   "adjusted_boxplot", "kde", "isolation_forest", "lof", "bootstrap"
#' @param factor Numeric, threshold factor (interpretation depends on method)
#' @param bootstrap_samples Integer, number of bootstrap samples (for bootstrap method)
#' @return Data frame with additional column .is_outlier (logical)
#' @export
detect_outliers <- function(data, value_col, group_col, 
                            method = "IQR", factor = 1.5, 
                            bootstrap_samples = 1000) {
    
    # Validate method
    valid_methods <- c("IQR", "zscore", "modified_zscore", 
                       "adjusted_boxplot", "kde", "isolation_forest",
                       "lof", "bootstrap")
    if (!method %in% valid_methods) {
        stop(paste("Invalid method. Choose one of:", paste(valid_methods, collapse = ", ")))
    }
    
    # Validate value column
    if (!value_col %in% names(data)) {
        stop(paste("Column", value_col, "not found in data"))
    }
    
    # Handle group_col - can be a column name or already a factor
    if (is.character(group_col) && length(group_col) == 1 && group_col %in% names(data)) {
        data$.outlier_group <- as.factor(data[[group_col]])
    } else if (is.factor(group_col) || is.character(group_col)) {
        data$.outlier_group <- as.factor(group_col)
    } else {
        data$.outlier_group <- factor(rep("all", nrow(data)))
    }
    
    # Define outlier detection functions (return logical vector same length as input)
#' @export
    detect_iqr <- function(x, fac) {
        result <- rep(FALSE, length(x))
        valid <- is.finite(x)
        if (sum(valid) < 4) return(result)
        
        q1 <- stats::quantile(x[valid], 0.25)
        q3 <- stats::quantile(x[valid], 0.75)
        iqr <- q3 - q1
        lower <- q1 - fac * iqr
        upper <- q3 + fac * iqr
        result[valid] <- x[valid] < lower | x[valid] > upper
        result
    }
    
#' @export
    detect_zscore <- function(x, fac) {
        result <- rep(FALSE, length(x))
        valid <- is.finite(x)
        if (sum(valid) < 3) return(result)
        
        z <- (x - mean(x[valid])) / stats::sd(x[valid])
        result[valid] <- abs(z[valid]) > fac
        result
    }
    
#' @export
    detect_modified_zscore <- function(x, fac) {
        result <- rep(FALSE, length(x))
        valid <- is.finite(x)
        if (sum(valid) < 3) return(result)
        
        med <- stats::median(x[valid])
        mad_val <- stats::mad(x[valid], constant = 1.4826)
        if (mad_val == 0) return(result)
        
        mod_z <- 0.6745 * (x - med) / mad_val
        result[valid] <- abs(mod_z[valid]) > fac
        result
    }
    
#' @export
    detect_adjusted_boxplot <- function(x, fac) {
        result <- rep(FALSE, length(x))
        valid <- is.finite(x)
        if (sum(valid) < 4) return(result)
        
        if (!requireNamespace("robustbase", quietly = TRUE)) {
            warning("Package 'robustbase' needed for adjusted_boxplot. Falling back to IQR.")
            return(detect_iqr(x, fac))
        }
        
        xv <- x[valid]
        mc <- robustbase::mc(xv)
        q1 <- stats::quantile(xv, 0.25)
        q3 <- stats::quantile(xv, 0.75)
        iqr <- q3 - q1
        
        if (mc >= 0) {
            lower <- q1 - fac * exp(-3.5 * mc) * iqr
            upper <- q3 + fac * exp(4 * mc) * iqr
        } else {
            lower <- q1 - fac * exp(-4 * mc) * iqr
            upper <- q3 + fac * exp(3.5 * abs(mc)) * iqr
        }
        result[valid] <- xv < lower | xv > upper
        result
    }
    
#' @export
    detect_kde <- function(x, fac) {
        result <- rep(FALSE, length(x))
        valid <- is.finite(x)
        if (sum(valid) < 4) return(result)
        
        xv <- x[valid]
        dens <- stats::density(xv)
        point_dens <- stats::approx(dens$x, dens$y, xout = xv)$y
        threshold <- stats::quantile(point_dens, fac, na.rm = TRUE)
        result[valid] <- point_dens < threshold
        result
    }
    
#' @export
    detect_isolation_forest <- function(x, fac) {
        result <- rep(FALSE, length(x))
        valid <- is.finite(x)
        if (sum(valid) < 10) return(result)
        
        if (!requireNamespace("isotree", quietly = TRUE)) {
            warning("Package 'isotree' needed for isolation_forest. Falling back to IQR.")
            return(detect_iqr(x, 1.5))
        }
        
        xv <- x[valid]
        iso <- isotree::isolation.forest(matrix(xv, ncol = 1), ntrees = 100, nthreads = 1)
        scores <- predict(iso, matrix(xv, ncol = 1))
        threshold <- stats::quantile(scores, 1 - fac)
        result[valid] <- scores > threshold
        result
    }
    
#' @export
    detect_lof <- function(x, fac) {
        result <- rep(FALSE, length(x))
        valid <- is.finite(x)
        if (sum(valid) < 10) return(result)
        
        if (!requireNamespace("dbscan", quietly = TRUE)) {
            warning("Package 'dbscan' needed for LOF. Falling back to IQR.")
            return(detect_iqr(x, 1.5))
        }
        
        xv <- x[valid]
        k <- min(5, length(xv) - 1)
        if (k < 1) return(result)
        
        lof_scores <- dbscan::lof(matrix(xv, ncol = 1), minPts = k)
        threshold <- stats::quantile(lof_scores, 1 - fac, na.rm = TRUE)
        result[valid] <- lof_scores > threshold
        result
    }
    
#' @export
    detect_bootstrap <- function(x, fac, n_samples) {
        result <- rep(FALSE, length(x))
        valid <- is.finite(x)
        if (sum(valid) < 4) return(result)
        
        xv <- x[valid]
        boot_means <- replicate(n_samples, mean(sample(xv, replace = TRUE)))
        boot_sd <- stats::sd(boot_means)
        if (boot_sd == 0) return(result)
        
        x_centered <- abs(xv - stats::median(xv))
        result[valid] <- x_centered > (fac * boot_sd)
        result
    }
    
    # Use dplyr to process by group
    data <- data %>%
        dplyr::group_by(.data$.outlier_group) %>%
        dplyr::mutate(
            .is_outlier = switch(method,
                "IQR" = detect_iqr(.data[[value_col]], factor),
                "zscore" = detect_zscore(.data[[value_col]], factor),
                "modified_zscore" = detect_modified_zscore(.data[[value_col]], factor),
                "adjusted_boxplot" = detect_adjusted_boxplot(.data[[value_col]], factor),
                "kde" = detect_kde(.data[[value_col]], factor),
                "isolation_forest" = detect_isolation_forest(.data[[value_col]], factor),
                "lof" = detect_lof(.data[[value_col]], factor),
                "bootstrap" = detect_bootstrap(.data[[value_col]], factor, bootstrap_samples)
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-".outlier_group")
    
    return(data)
}


#' Get filtered data for a specific measurement
#'
#' Extracts rows for a specific measurement column, excluding outliers and trimmed values.
#' Uses the {col}_outlier and {col}_trimmed columns created by the plotting module.
#'
#' @param data Data frame with {col}_outlier and {col}_trimmed columns
#' @param measure_col Character, name of the measurement column
#' @return Data frame with only valid (non-outlier, non-trimmed) rows for this measurement
#' @export
get_filtered_measurement_data <- function(data, measure_col) {
    outlier_col <- paste0(measure_col, "_outlier")
    trimmed_col <- paste0(measure_col, "_trimmed")
    
    # Get exclusion flags (default to FALSE if columns don't exist)
    is_outlier <- if (outlier_col %in% names(data)) data[[outlier_col]] else rep(FALSE, nrow(data))
    is_trimmed <- if (trimmed_col %in% names(data)) data[[trimmed_col]] else rep(FALSE, nrow(data))
    
    # Filter: keep rows that are not outliers, not trimmed, and have valid measurement values
    keep_idx <- !is_outlier & !is_trimmed & !is.na(data[[measure_col]])
    
    data[keep_idx, , drop = FALSE]
}


#' Check level consistency across groups in multi-way designs
#'
#' For multi-way designs (two-way, three-way), checks if all combinations of the
#' primary grouping variable have the same levels of secondary grouping variables.
#' This is important for Welch-Yuen tests which are not robust against level discrepancies.
#'
#' @param df Data frame containing the data
#' @param primary_group Character, the primary grouping column (first X-axis variable)
#' @param secondary_groups Character vector, secondary grouping columns to check for consistency
#' @return NULL if consistent, otherwise character vector of discrepancy messages
#' @export
check_level_consistency <- function(df, primary_group, secondary_groups) {
    if (length(secondary_groups) == 0) return(NULL)
    
    # Get all unique levels of the primary group
    primary_levels <- unique(df[[primary_group]])
    
    # For each secondary group, get expected levels (all unique values in full dataset)
    expected_levels <- lapply(secondary_groups, function(col) unique(df[[col]]))
    names(expected_levels) <- secondary_groups
    
    discrepancies <- character(0)
    
    # Check each primary group level
    for (level in primary_levels) {
        df_subset <- df[df[[primary_group]] == level, , drop = FALSE]
        
        # Check each secondary group
        for (sec_col in secondary_groups) {
            actual_levels <- unique(df_subset[[sec_col]])
            expected <- expected_levels[[sec_col]]
            
            if (length(actual_levels) != length(expected)) {
                msg <- sprintf(
                    "In %s='%s': found %d level(s) of '%s' instead of %d expected.",
                    primary_group, level, 
                    length(actual_levels), sec_col, 
                    length(expected)
                )
                discrepancies <- c(discrepancies, msg)
            }
        }
    }
    
    if (length(discrepancies) == 0) NULL else discrepancies
}
