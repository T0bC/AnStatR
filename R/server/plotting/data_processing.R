#' Data Processing Component
#'
#' Creates processed data with outlier and trim flags for each selected measurement.
#' This enables downstream modules to use the same exclusion logic as plotting.
#'
#' Following the explicit dependency injection pattern:
#' - All dependencies are passed as explicit parameters
#' - No implicit scoping or global state access
#'
#' @name data_processing
NULL

# Import data utilities for create_interaction function
box::use(../../utils/data_utils)


#' Create Processed Data Reactive
#'
#' Applies outlier detection and trim marking for each selected measurement column.
#' Creates {col}_outlier and {col}_trimmed columns for each measurement.
#'
#' @param filtered_data Reactive returning group-filtered data
#' @param selected_measures Reactive returning selected measurement column names
#' @param x_axis Reactive returning X-axis column(s) for grouping
#' @param trim_percent Reactive returning trim percentage (0-100)
#' @param outlier_options Reactive returning outlier detection options list
#' @return Reactive returning processed data with per-column outlier/trimmed flags
#' @export
create_processed_data_reactive <- function(filtered_data, 
                                            selected_measures, 
                                            x_axis,
                                            trim_percent, 
                                            outlier_options) {
    
    shiny::reactive({
        data <- filtered_data()
        shiny::req(data)
        
        measures <- selected_measures()
        if (is.null(measures) || length(measures) == 0) {
            return(data)
        }
        
        x_cols <- x_axis()
        trim_pct <- trim_percent() %||% 0
        outlier_opts <- outlier_options()
        
        # Create interaction term for grouping (same as in plot_scatter.R)
        if (!is.null(x_cols) && length(x_cols) > 0 && all(x_cols %in% names(data))) {
            interaction_term <- data_utils$create_interaction(data, x_cols)
        } else {
            interaction_term <- factor(rep("all", nrow(data)))
        }
        
        # Process each measurement column
        for (measure_col in measures) {
            if (!measure_col %in% names(data)) next
            
            # Initialize columns
            outlier_col <- paste0(measure_col, "_outlier")
            trimmed_col <- paste0(measure_col, "_trimmed")
            data[[outlier_col]] <- FALSE
            data[[trimmed_col]] <- FALSE
            
            # Step 1: Detect outliers for this measurement
            if (isTRUE(outlier_opts$enabled)) {
                temp_data <- data_utils$detect_outliers(
                    data = data,
                    value_col = measure_col,
                    group_col = interaction_term,
                    method = outlier_opts$method %||% "IQR",
                    factor = outlier_opts$factor %||% 1.5,
                    bootstrap_samples = outlier_opts$bootstrap_samples %||% 1000
                )
                data[[outlier_col]] <- temp_data$.is_outlier
            }
            
            # Step 2: Mark trimmed data (only on non-outlier data)
            if (trim_pct > 0) {
                non_outlier_idx <- which(!data[[outlier_col]])
                if (length(non_outlier_idx) > 0) {
                    # Create subset for trimming calculation
                    non_outlier_data <- data[non_outlier_idx, , drop = FALSE]
                    non_outlier_interaction <- interaction_term[non_outlier_idx]
                    
                    # Mark trimmed points within non-outlier subset
                    non_outlier_data <- data_utils$mark_trimmed_data(
                        data = non_outlier_data,
                        value_col = measure_col,
                        group_col = non_outlier_interaction,
                        trim_percent = trim_pct
                    )
                    
                    # Copy trimmed status back to main data
                    data[[trimmed_col]][non_outlier_idx] <- non_outlier_data$.is_trimmed
                }
            }
        }
        
        # Clean up temporary columns if they exist
        data$.is_outlier <- NULL
        data$.is_trimmed <- NULL
        data$.group <- NULL
        data$.trim_rank <- NULL
        
        data
    })
}
