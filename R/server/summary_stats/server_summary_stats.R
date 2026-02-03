#' Server logic for the Summary Statistics page
#'
#' Orchestrates all summary statistics components using explicit dependency injection.
#' Components are sourced from R/server/summary_stats/
#'
#' Uses data from the Plotting tab as source of truth - this ensures summary statistics
#' are calculated on the same filtered/trimmed/outlier-excluded data shown in plots.
#' The processed_data contains {col}_outlier and {col}_trimmed columns for each
#' selected measurement.
#'
#' @param id Module namespace ID
#' @param processed_data Reactive containing data with {col}_outlier and {col}_trimmed flags
#' @param selected_measures Reactive returning selected measurement columns from plotting
#' @param x_axis Reactive returning X-axis columns from plotting (used as default grouping)
#' @param data_version Reactive integer that increments when new data is loaded
#' @return NULL (side effects only)
#' @export
server_summary_stats <- function(id, processed_data, selected_measures, x_axis, data_version) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Import component modules
        box::use(../summary_stats/summary_utils)
        box::use(../summary_stats/sidebar_logic)
        box::use(../summary_stats/summary_tables)
        box::use(../../utils/column_utils)
        
        # ----- 1. Column Reactives -----
        # Use only the selected measurements from plotting (already have _outlier/_trimmed flags)
        measurement_cols <- shiny::reactive({
            measures <- selected_measures()
            if (is.null(measures) || length(measures) == 0) {
                # Fallback to all measurement cols if none selected
                shiny::req(processed_data())
                cols <- column_utils$get_measurement_cols(processed_data())
                cols[!grepl("_outlier|_trimmed", cols)]
            } else {
                measures
            }
        })
        
        descriptive_cols <- shiny::reactive({
            shiny::req(processed_data())
            column_utils$get_descriptive_cols(processed_data())
        })
        
        # ----- 2. Reset state on new data -----
        if (!is.null(data_version)) {
            shiny::observeEvent(data_version(), {
                # Reset Shapiro checkbox
                shiny::updateCheckboxInput(
                    session = session,
                    inputId = "shapiro",
                    value = FALSE
                )
                
                # Reset filter_options_select to force re-render with new columns
                # The sidebar_logic will set appropriate defaults from new data
                shiny::updateSelectizeInput(
                    session = session,
                    inputId = "filter_options_select",
                    choices = character(0),
                    selected = character(0)
                )
            }, ignoreInit = TRUE)
        }
        
        # ----- 3. Sidebar Logic -----
        # Pass x_axis from plotting as default for filter options
        sidebar_logic$setup_sidebar_logic(
            input = input,
            output = output,
            session = session,
            descriptive_cols = descriptive_cols,
            x_axis = x_axis
        )
        
        # ----- 4. Summary DataFrames -----
        # Always uses "Measurement" mode with filter_options_select as grouping
        summary_dfs <- summary_tables$create_summary_dfs_reactive(
            input = input,
            median_data = processed_data,
            measurement_cols = measurement_cols
        )
        
        # ----- 5. Table Outputs -----
        summary_tables$setup_summary_table_outputs(
            output = output,
            session = session,
            summary_dfs = summary_dfs
        )
        
        # ----- 6. Tables UI Container -----
        summary_tables$setup_summary_tables_ui(
            output = output,
            ns = ns,
            summary_dfs = summary_dfs,
            median_data = processed_data
        )
        
        # ----- 7. Download All Handler -----
        summary_tables$setup_download_all_handler(
            output = output,
            summary_dfs = summary_dfs
        )
    })
}
