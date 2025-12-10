#' Server logic for the Summary Statistics page
#'
#' Orchestrates all summary statistics components using explicit dependency injection.
#' Components are sourced from R/server/modules/pages/summary_stats/
#'
#' Uses data from the Plotting tab as source of truth - this ensures summary statistics
#' are calculated on the same filtered/trimmed/outlier-excluded data shown in plots.
#' The processed_data contains {col}_outlier and {col}_trimmed columns for each
#' selected measurement.
#'
#' @param id Module namespace ID
#' @param processed_data Reactive containing data with {col}_outlier and {col}_trimmed flags
#' @param selected_measures Reactive returning selected measurement columns from plotting
#' @param data_version Reactive integer that increments when new data is loaded
#' @return NULL (side effects only)
server_summary_stats <- function(id, processed_data, selected_measures, data_version) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Source component files
        source("R/server/modules/pages/summary_stats/summary_utils.R", local = TRUE)
        source("R/server/modules/pages/summary_stats/sidebar_logic.R", local = TRUE)
        source("R/server/modules/pages/summary_stats/summary_tables.R", local = TRUE)
        
        # ----- 1. Column Reactives -----
        # Use only the selected measurements from plotting (already have _outlier/_trimmed flags)
        measurement_cols <- shiny::reactive({
            measures <- selected_measures()
            if (is.null(measures) || length(measures) == 0) {
                # Fallback to all measurement cols if none selected
                shiny::req(processed_data())
                cols <- get_measurement_cols(processed_data())
                cols[!grepl("_outlier|_trimmed", cols)]
            } else {
                measures
            }
        })
        
        descriptive_cols <- shiny::reactive({
            shiny::req(processed_data())
            get_descriptive_cols(processed_data())
        })
        
        # X-axis column (first descriptive column as default)
        x_axis_col <- shiny::reactive({
            desc_cols <- descriptive_cols()
            if (length(desc_cols) > 0) desc_cols[1] else NULL
        })
        
        # ----- 2. Reset state on new data -----
        if (!is.null(data_version)) {
            shiny::observeEvent(data_version(), {
                shiny::updateSelectizeInput(
                    session = session,
                    inputId = "sorting_options",
                    selected = "Measurement"
                )
                shiny::updateCheckboxInput(
                    session = session,
                    inputId = "shapiro",
                    value = FALSE
                )
            }, ignoreInit = TRUE)
        }
        
        # ----- 3. Sidebar Logic -----
        setup_sidebar_logic(
            input = input,
            output = output,
            session = session,
            median_data = processed_data,
            descriptive_cols = descriptive_cols,
            x_axis_col = x_axis_col
        )
        
        # ----- 4. Summary DataFrames -----
        # No trim_value needed - data already has {col}_trimmed columns
        summary_dfs <- create_summary_dfs_reactive(
            input = input,
            median_data = processed_data,
            measurement_cols = measurement_cols,
            descriptive_cols = descriptive_cols,
            x_axis_col = x_axis_col
        )
        
        # ----- 5. Table Outputs -----
        setup_summary_table_outputs(
            output = output,
            session = session,
            summary_dfs = summary_dfs
        )
        
        # ----- 6. Tables UI Container -----
        setup_summary_tables_ui(
            output = output,
            ns = ns,
            summary_dfs = summary_dfs,
            median_data = processed_data
        )
        
        # ----- 7. Download All Handler -----
        setup_download_all_handler(
            output = output,
            summary_dfs = summary_dfs
        )
    })
}
