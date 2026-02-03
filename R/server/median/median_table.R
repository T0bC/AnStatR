# Median table rendering logic
# This file defines a function that calculates and renders the median results table
#
# @param output Shiny output object
# @param output_id Character string for the output ID (e.g., "medianTable")
# @param filtered_data ReactiveVal containing the filtered data
# @param grouping_cols ReactiveVal containing selected grouping columns
# @param quality_col ReactiveVal containing the selected quality column name (to exclude from output)
# @param median_results ReactiveVal to store the calculated median results
# @param removed_cols ReactiveVal to store columns removed due to within-group variation
# @return NULL (side effects: creates output$medianTable and updates median_results)

# Import column utilities
box::use(../../utils/column_utils)

#' @export
render_median_table <- function(output, output_id, filtered_data, grouping_cols, 
                                 quality_col = NULL, median_results, removed_cols = NULL) {
    output[[output_id]] <- DT::renderDataTable({
        shiny::req(filtered_data())

        data <- filtered_data()
        selected_grouping <- grouping_cols()
        quality_column <- if (!is.null(quality_col)) quality_col() else NULL

        # Get column types using utility functions
        measurement_col_names <- column_utils$get_measurement_cols(data)
        descriptive_col_names <- column_utils$get_descriptive_cols(data)
        
        # Remove quality column from descriptive columns (it's no longer needed after filtering)
        if (!is.null(quality_column) && quality_column != "None" && quality_column %in% descriptive_col_names) {
            descriptive_col_names <- setdiff(descriptive_col_names, quality_column)
        }
        
        # Filter to only numeric measurement columns
        measurement_col_names <- measurement_col_names[sapply(measurement_col_names, function(col) {
            is.numeric(data[[col]])
        })]

        if (length(measurement_col_names) == 0) {
            # No measurement columns found
            results <- data.frame(
                Message = "No measurement columns found in the filtered data."
            )
            median_results(NULL)
            if (!is.null(removed_cols)) removed_cols(NULL)
            
        } else if (is.null(selected_grouping) || length(selected_grouping) == 0) {
            # NO GROUPING: Return filtered dataframe as-is (no median calculation)
            # Remove quality column from output (it served its purpose)
            if (!is.null(quality_column) && quality_column != "None" && quality_column %in% names(data)) {
                data <- data[, setdiff(names(data), quality_column), drop = FALSE]
            }
            results <- data
            median_results(results)
            if (!is.null(removed_cols)) removed_cols(NULL)
            
        } else {
            # WITH GROUPING: Calculate medians
            
            # Find descriptive columns that VARY within groups (these must be removed)
            # A column varies within a group if n_distinct > 1 for ANY group
            other_descriptive <- setdiff(descriptive_col_names, selected_grouping)
            
            cols_to_remove <- character(0)
            if (length(other_descriptive) > 0) {
                # Check each non-grouping descriptive column
                for (col in other_descriptive) {
                    # Check if this column varies within any group
                    varies_within_group <- any(
                        tapply(data[[col]], 
                               interaction(data[selected_grouping], drop = TRUE),
                               function(x) length(unique(x)) > 1)
                    )
                    if (varies_within_group) {
                        cols_to_remove <- c(cols_to_remove, col)
                    }
                }
            }
            
            # Store removed columns for user feedback
            if (!is.null(removed_cols)) removed_cols(cols_to_remove)
            
            # Columns to keep: grouping + constant descriptive + measurement
            constant_descriptive <- setdiff(other_descriptive, cols_to_remove)
            
            # Build the result dataframe
            # First, get unique combinations of grouping + constant descriptive columns
            if (length(constant_descriptive) > 0) {
                keep_cols <- c(selected_grouping, constant_descriptive)
            } else {
                keep_cols <- selected_grouping
            }
            
            # Calculate medians grouped by selected columns
            # Use aggregate for base R approach
            median_data <- stats::aggregate(
                data[measurement_col_names],
                by = data[selected_grouping],
                FUN = function(x) median(x, na.rm = TRUE)
            )
            
            # If there are constant descriptive columns, merge them back
            if (length(constant_descriptive) > 0) {
                # Get unique values of constant descriptive columns per group
                constant_data <- unique(data[c(selected_grouping, constant_descriptive)])
                
                # Merge with median data
                results <- merge(median_data, constant_data, by = selected_grouping, all.x = TRUE)
                
                # Reorder columns: grouping, constant descriptive, then measurements
                col_order <- c(selected_grouping, constant_descriptive, measurement_col_names)
                col_order <- col_order[col_order %in% names(results)]
                results <- results[, col_order, drop = FALSE]
            } else {
                results <- median_data
            }
            
            # Round numeric columns
            for (col in measurement_col_names) {
                if (col %in% names(results)) {
                    results[[col]] <- round(results[[col]], 4)
                }
            }
            
            median_results(results)
        }

        # Create DataTable with options
        # dom: l=length, t=table, i=info, p=pagination (no 'f' = no global search)
        DT::datatable(
            results,
            filter = "top",  # Column filters at top of each column
            options = list(
                pageLength = 25,
                lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),
                scrollX = TRUE,
                dom = "ltip"  # Removed 'f' (global search) and 'B' (buttons)
            ),
            rownames = FALSE
        )
    })
}
