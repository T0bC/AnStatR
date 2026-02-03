#' Server logic for the Statistics page
#'
#' Orchestrates all statistics components using explicit dependency injection.
#' Components are sourced from R/server/statistics/
#'
#' @param id Module namespace ID
#' @param processed_data Reactive containing the processed data from server_plotting
#'   (with {col}_outlier and {col}_trimmed flags)
#' @param selected_measures Reactive containing selected measurement columns from plotting
#' @param x_axis Reactive containing selected X-axis columns from plotting
#' @param trim_percent Reactive containing the trim percentage from plotting
#' @param cached_plot_objects Reactive containing cached ggplot objects from plotting tab
#' @param plot_params Reactive containing plot parameters including window_size from plotting tab
#' @param data_version Reactive integer that increments when new data is loaded
#' @return NULL (side effects only - renders outputs)
#' @export
server_statistics <- function(id, processed_data, selected_measures, x_axis, trim_percent, cached_plot_objects, plot_params, data_version) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Debug flag from environment variable (set TEXAN_DEBUG_REACTIVES=true to enable)
        DEBUG_REACTIVES <- tolower(Sys.getenv("TEXAN_DEBUG_REACTIVES", "false")) == "true"
        
        # Import component modules
        box::use(../statistics/sidebar_logic)
        box::use(../statistics/statistics_output)
        box::use(../statistics/statistics_report)
        
        # ----- 1. Sidebar Logic -----
        # Setup dynamic UI elements in sidebar
        sidebar_logic$setup_sidebar_ui(
            input = input,
            output = output,
            session = session,
            x_axis = x_axis,
            trim_percent = trim_percent
        )
        
        # ----- 2. Statistics Parameters -----
        # Collect all statistics parameters into a single reactive
        stats_params <- sidebar_logic$create_statistics_params(
            input = input,
            x_axis = x_axis
        )
        
        # ----- 3. Statistics Output -----
        # Setup the main output area with plots from plotting tab
        # Returns computation_results reactive for download handlers
        computation_results <- statistics_output$setup_statistics_output(
            input = input,
            output = output,
            session = session,
            processed_data = processed_data,
            selected_measures = selected_measures,
            x_axis = x_axis,
            trim_percent = trim_percent,
            stats_params = stats_params,
            cached_plot_objects = cached_plot_objects,
            plot_params = plot_params,
            debug = DEBUG_REACTIVES,
            data_version = data_version
        )
        
        # ----- 4. Download Handlers -----
        # Setup download handlers for statistics reports
        statistics_report$setup_statistics_download_handlers(
            input = input,
            output = output,
            session = session,
            computation_results = computation_results,
            cached_plot_objects = cached_plot_objects
        )
    })
}
