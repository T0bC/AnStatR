# Quality Filter Logic
# This file defines the filtering logic for quality-based data filtering
#
# @description Applies quality filtering with awareness of grouping structure.
# When grouping is defined, bad values are only removed if the group contains
# at least one good value. Groups with only bad values are kept intact.

#' Apply quality filter to data
#'
#' @param data Data frame to filter
#' @param quality_settings List with filter settings (from quality_filter_ui)
#' @param grouping_cols Character vector of grouping column names (can be NULL)
#' @return List with:
#'   - data: Filtered data frame
#'   - message: Description of filtering results
#' @export
apply_quality_filter <- function(data, quality_settings, grouping_cols) {
    # No filtering if disabled
    if (!quality_settings$enabled || is.null(quality_settings$column)) {
        return(list(
            data = data,
            message = "No quality filtering applied."
        ))
    }
    
    col <- quality_settings$column
    rows_before <- nrow(data)
    
    # Categorical filtering
    if (quality_settings$type == "categorical") {
        bad_values <- quality_settings$bad_values
        
        # No bad values selected
        if (is.null(bad_values) || length(bad_values) == 0) {
            return(list(
                data = data,
                message = "Quality column selected but no bad values specified."
            ))
        }
        
        result <- filter_categorical(data, col, bad_values, grouping_cols)
        
    } else {
        # Numeric/percentage filtering
        threshold <- quality_settings$threshold
        
        if (is.null(threshold)) {
            return(list(
                data = data,
                message = "Quality column selected but no threshold specified."
            ))
        }
        
        result <- filter_numeric(data, col, threshold, grouping_cols)
    }
    
    result
}

#' Filter by categorical bad values
#' @keywords internal
#' @export
filter_categorical <- function(data, col, bad_values, grouping_cols) {
    rows_before <- nrow(data)
    
    if (!is.null(grouping_cols) && length(grouping_cols) > 0) {
        # WITH GROUPING: Keep bad values only if group has no good values
        
        # Mark each row as good or bad
        data$.is_bad <- data[[col]] %in% bad_values
        
        # For each group, check if it has any good values
        group_has_good <- stats::aggregate(
            data$.is_bad,
            by = data[grouping_cols],
            FUN = function(x) !all(x)
        )
        names(group_has_good)[ncol(group_has_good)] <- ".group_has_good"
        
        # Merge back
        data <- merge(data, group_has_good, by = grouping_cols, all.x = TRUE, sort = FALSE)
        
        # Filter: keep if (group has no good values) OR (this row is good)
        filtered <- data[!data$.group_has_good | !data$.is_bad, ]
        
        # Count groups
        n_groups_total <- nrow(unique(data[grouping_cols]))
        n_groups_all_bad <- sum(!group_has_good$.group_has_good)
        n_groups_filtered <- n_groups_total - n_groups_all_bad
        
        # Clean up helper columns
        filtered$.is_bad <- NULL
        filtered$.group_has_good <- NULL
        
        rows_after <- nrow(filtered)
        
        message <- paste0(
            "Categorical quality filter applied.\n",
            "Groups: ", n_groups_total, " total â€” ",
            n_groups_all_bad, " kept intact (only bad values), ",
            n_groups_filtered, " cleaned (bad rows removed).\n",
            "Rows: ", rows_before, " â†’ ", rows_after, 
            " (", rows_before - rows_after, " bad rows removed)"
        )
        
    } else {
        # WITHOUT GROUPING: Simply remove bad values
        filtered <- data[!data[[col]] %in% bad_values, ]
        rows_after <- nrow(filtered)
        
        message <- paste0(
            "Categorical quality filter applied (no grouping).\n",
            "Rows: ", rows_before, " â†’ ", rows_after,
            " (", rows_before - rows_after, " bad values removed)"
        )
    }
    
    list(data = filtered, message = message)
}

#' Filter by numeric threshold
#' @keywords internal
#' @export
filter_numeric <- function(data, col, threshold, grouping_cols) {
    rows_before <- nrow(data)
    
    if (!is.null(grouping_cols) && length(grouping_cols) > 0) {
        # WITH GROUPING: Keep below-threshold only if group has no above-threshold values
        
        # Mark each row as good (>= threshold) or bad (< threshold)
        data$.is_bad <- data[[col]] < threshold | is.na(data[[col]])
        
        # For each group, check if it has any good values
        group_has_good <- stats::aggregate(
            data$.is_bad,
            by = data[grouping_cols],
            FUN = function(x) !all(x)
        )
        names(group_has_good)[ncol(group_has_good)] <- ".group_has_good"
        
        # Merge back
        data <- merge(data, group_has_good, by = grouping_cols, all.x = TRUE, sort = FALSE)
        
        # Filter: keep if (group has no good values) OR (this row is good)
        filtered <- data[!data$.group_has_good | !data$.is_bad, ]
        
        # Count groups
        n_groups_total <- nrow(unique(data[grouping_cols]))
        n_groups_all_bad <- sum(!group_has_good$.group_has_good)
        n_groups_filtered <- n_groups_total - n_groups_all_bad
        
        # Clean up helper columns
        filtered$.is_bad <- NULL
        filtered$.group_has_good <- NULL
        
        rows_after <- nrow(filtered)
        
        message <- paste0(
            "Numeric quality filter applied (threshold â‰¥ ", threshold, ").\n",
            "Groups: ", n_groups_total, " total â€” ",
            n_groups_all_bad, " kept intact (all below threshold), ",
            n_groups_filtered, " cleaned (below-threshold rows removed).\n",
            "Rows: ", rows_before, " â†’ ", rows_after,
            " (", rows_before - rows_after, " rows removed)"
        )
        
    } else {
        # WITHOUT GROUPING: Simply remove below-threshold values
        filtered <- data[data[[col]] >= threshold & !is.na(data[[col]]), ]
        rows_after <- nrow(filtered)
        
        message <- paste0(
            "Numeric quality filter applied (threshold â‰¥ ", threshold, ", no grouping).\n",
            "Rows: ", rows_before, " â†’ ", rows_after,
            " (", rows_before - rows_after, " below threshold removed)"
        )
    }
    
    list(data = filtered, message = message)
}
