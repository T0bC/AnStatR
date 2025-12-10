#' Server logic for the Summary Statistics page
#'
#' Orchestrates all summary statistics components using explicit dependency injection.
#' Components are sourced from R/server/modules/pages/summary_stats/
#'
#' @param id Module namespace ID
#' @param median_data Reactive containing the median-processed data from server_median
#' @param data_version Reactive integer that increments when new data is loaded
#' @param trim_value Reactive returning the trim percentage (0-100), optional
#' @return NULL (side effects only)
server_summary_stats <- function(id, median_data, data_version, trim_value = NULL) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Source component files
        source("R/server/modules/pages/summary_stats/summary_utils.R", local = TRUE)
        source("R/server/modules/pages/summary_stats/sidebar_logic.R", local = TRUE)
        source("R/server/modules/pages/summary_stats/summary_tables.R", local = TRUE)
        
        # ----- 1. Column Reactives -----
        measurement_cols <- shiny::reactive({
            shiny::req(median_data())
            cols <- get_measurement_cols(median_data())
            # Exclude outlier columns
            cols[!grepl("_outlier", cols)]
        })
        
        descriptive_cols <- shiny::reactive({
            shiny::req(median_data())
            get_descriptive_cols_short(median_data())
        })
        
        # X-axis column (first descriptive column as default)
        x_axis_col <- shiny::reactive({
            desc_cols <- descriptive_cols()
            if (length(desc_cols) > 0) desc_cols[1] else NULL
        })
        
        # Trim value reactive (default to 0 if not provided)
        trim_reactive <- if (is.null(trim_value)) {
            shiny::reactive({ 0 })
        } else {
            trim_value
        }
        
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
            median_data = median_data,
            descriptive_cols = descriptive_cols,
            x_axis_col = x_axis_col
        )
        
        # ----- 4. Summary DataFrames -----
        summary_dfs <- create_summary_dfs_reactive(
            input = input,
            median_data = median_data,
            measurement_cols = measurement_cols,
            descriptive_cols = descriptive_cols,
            x_axis_col = x_axis_col,
            trim_value = trim_reactive
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
            median_data = median_data
        )
        
        # ----- 7. Download All Handler -----
        setup_download_all_handler(
            output = output,
            summary_dfs = summary_dfs
        )
    })
}
