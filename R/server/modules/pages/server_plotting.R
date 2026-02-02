#' Server logic for the Plotting page
#'
#' Orchestrates all plotting components using explicit dependency injection.
#' Components are sourced from R/server/modules/pages/plotting/
#'
#' @param id Module namespace ID
#' @param median_data Reactive containing the median-processed data from server_median
#' @param data_version Reactive integer that increments when new data is loaded
#' @return List with reactives for downstream modules:
#'   - processed_data: Data with {col}_outlier and {col}_trimmed flags for each selected measure
#'   - selected_measures: Selected measurement columns
#'   - x_axis: Selected X-axis columns for grouping
server_plotting <- function(id, median_data, data_version) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Debug flag from environment variable (set TEXAN_DEBUG_REACTIVES=true to enable)
        DEBUG_REACTIVES <- tolower(Sys.getenv("TEXAN_DEBUG_REACTIVES", "false")) == "true"
        
        # Saved filter state for persistence across median recalculations
        saved_filter_state <- shiny::reactiveVal(list())
        
        # Source component files
        source("R/server/modules/pages/plotting/input_updaters.R", local = TRUE)
        source("R/server/modules/pages/plotting/filter_logic.R", local = TRUE)
        source("R/server/modules/pages/plotting/reactive_params.R", local = TRUE)
        source("R/server/modules/pages/plotting/color_pickers.R", local = TRUE)
        source("R/server/modules/pages/plotting/plots_ui.R", local = TRUE)
        source("R/server/modules/pages/plotting/download_handler.R", local = TRUE)
        source("R/server/modules/pages/plotting/data_processing.R", local = TRUE)
        
        # ----- 1. Column Reactives -----
        column_reactives <- create_column_reactives(median_data)
        descriptive_cols <- column_reactives$descriptive_cols
        measurement_cols <- column_reactives$measurement_cols
        
        # ----- 2. Input Updaters -----
        setup_input_updaters(
            input = input,
            session = session,
            median_data = median_data,
            data_version = data_version,
            descriptive_cols = descriptive_cols,
            measurement_cols = measurement_cols,
            saved_filter_state = saved_filter_state
        )
        
        # ----- 3. Filter Logic -----
        filter_cols <- create_filter_cols_reactive(input)
        filtered_data <- create_filtered_data_reactive(input, median_data, filter_cols)
        
        # Capture filter state before median data changes (for persistence)
        shiny::observeEvent(median_data(), {
            cols <- shiny::isolate(filter_cols())
            if (length(cols) > 0) {
                state <- lapply(cols, function(col) input[[col]])
                names(state) <- cols
                saved_filter_state(state)
            }
        }, priority = 100, ignoreInit = TRUE)
        
        setup_filter_checkboxes_output(output, ns, median_data, filter_cols, saved_filter_state)
        
        # ----- 4. Selection Reactives -----
        selection_reactives <- create_selection_reactives(input)
        
        # ----- 5. Window Size & Export Dimensions -----
        window_size <- create_window_size_reactive(input, debug = DEBUG_REACTIVES)
        export_dims <- create_export_dimension_reactives(input)
        
        # ----- 6. Processing Options -----
        processing_reactives <- create_processing_option_reactives(input)
        
        # ----- 7. Style Reactives -----
        style_reactives <- create_style_reactives(input)
        
        # ----- 8. Color Pickers -----
        color_groups <- create_color_groups_reactive(filtered_data, selection_reactives$color_cols)
        custom_color_map <- create_custom_color_map_reactive(input, color_groups)
        setup_color_pickers_output(
            output = output,
            ns = ns,
            filtered_data = filtered_data,
            color_cols = selection_reactives$color_cols,
            color_groups = color_groups,
            custom_color_map = custom_color_map
        )
        
        # ----- 9. Consolidated Plot Parameters -----
        plot_params <- create_plot_params(
            filtered_data = filtered_data,
            selection_reactives = selection_reactives,
            processing_reactives = processing_reactives,
            style_reactives = style_reactives,
            window_size = window_size,
            custom_color_map = custom_color_map,
            debug = DEBUG_REACTIVES
        )
        
        # ----- 10. Cached Plot Objects -----
        # Create cached ggplot objects that can be shared with statistics tab
        cached_plot_objects <- create_cached_plot_objects(
            plot_params = plot_params,
            measure_cols = selection_reactives$measures,
            create_scatter_plot = create_scatter_plot
        )
        
        # ----- 11. Plot Outputs -----
        setup_plot_outputs(
            output = output,
            ns = ns,
            plot_params = plot_params,
            measure_cols = selection_reactives$measures,
            create_scatter_plot = create_scatter_plot,
            cached_plot_objects = cached_plot_objects,
            export_width = export_dims$export_width,
            export_height = export_dims$export_height
        )
        
        # ----- 12. Plots UI Container -----
        setup_plots_ui_output(
            output = output,
            ns = ns,
            input = input,
            median_data = median_data,
            debug = DEBUG_REACTIVES
        )
        
        # ----- 13. Download Handler -----
        setup_download_handler(output, input, filtered_data)
        
        # ----- 14. Processed Data for Downstream Modules -----
        # Creates {col}_outlier and {col}_trimmed columns for each selected measurement
        processed_data <- create_processed_data_reactive(
            filtered_data = filtered_data,
            selected_measures = selection_reactives$measures,
            x_axis = selection_reactives$x_axis,
            trim_percent = processing_reactives$trim_percent,
            outlier_options = processing_reactives$outlier_options
        )
        
        # ----- 15. Return values for downstream modules -----
        # Plotting tab is the source of truth for filtered/processed data
        # cached_plot_objects allows statistics tab to reuse plots without recomputation
        # plot_params includes window_size for consistent plot sizing across tabs
        list(
            processed_data = processed_data,
            selected_measures = selection_reactives$measures,
            x_axis = selection_reactives$x_axis,
            trim_percent = processing_reactives$trim_percent,
            cached_plot_objects = cached_plot_objects,
            plot_params = plot_params
        )
    })
}
