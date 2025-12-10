#' Server logic for the Plotting page
#'
#' Orchestrates all plotting components using explicit dependency injection.
#' Components are sourced from R/server/modules/pages/plotting/
#'
#' @param id Module namespace ID
#' @param median_data Reactive containing the median-processed data from server_median
#' @param data_version Reactive integer that increments when new data is loaded
#' @return NULL (side effects only)
server_plotting <- function(id, median_data, data_version) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # ===== DEBUG: Toggle to enable/disable debug logging =====
        DEBUG_REACTIVES <- FALSE
        
        # Source component files
        source("R/server/modules/pages/plotting/input_updaters.R", local = TRUE)
        source("R/server/modules/pages/plotting/filter_logic.R", local = TRUE)
        source("R/server/modules/pages/plotting/reactive_params.R", local = TRUE)
        source("R/server/modules/pages/plotting/color_pickers.R", local = TRUE)
        source("R/server/modules/pages/plotting/plots_ui.R", local = TRUE)
        source("R/server/modules/pages/plotting/download_handler.R", local = TRUE)
        
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
            measurement_cols = measurement_cols
        )
        
        # ----- 3. Filter Logic -----
        filter_cols <- create_filter_cols_reactive(input)
        filtered_data <- create_filtered_data_reactive(input, median_data, filter_cols)
        setup_filter_checkboxes_output(output, ns, median_data, filter_cols)
        
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
        
        # ----- 10. Plot Outputs -----
        setup_plot_outputs(
            output = output,
            ns = ns,
            plot_params = plot_params,
            measure_cols = selection_reactives$measures,
            create_scatter_plot = create_scatter_plot,
            export_width = export_dims$export_width,
            export_height = export_dims$export_height
        )
        
        # ----- 11. Plots UI Container -----
        setup_plots_ui_output(
            output = output,
            ns = ns,
            input = input,
            median_data = median_data,
            debug = DEBUG_REACTIVES
        )
        
        # ----- 12. Download Handler -----
        setup_download_handler(output, input, filtered_data)
    })
}
