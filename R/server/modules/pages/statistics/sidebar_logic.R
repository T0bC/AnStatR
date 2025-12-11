#' Sidebar Logic for Statistics Module
#'
#' Handles dynamic UI elements in the statistics sidebar.
#'
#' @param input Shiny input object from the parent module
#' @param output Shiny output object from the parent module
#' @param session Shiny session object from the parent module
#' @param x_axis Reactive containing selected X-axis columns from plotting
#' @param trim_percent Reactive containing the trim percentage from plotting
setup_sidebar_ui <- function(input, output, session, x_axis, trim_percent) {
    ns <- session$ns
    
    # Display current trim value from plotting module
    output$trim_value_display <- shiny::renderUI({
        trim_val <- trim_percent()
        shiny::tags$div(
            class = "alert alert-info py-2",
            shiny::tags$small(
                shiny::tags$strong("Current trim value: "),
                paste0(trim_val, "%"),
                shiny::tags$br(),
                shiny::tags$span(
                    class = "text-muted",
                    "(Set in Plotting tab)"
                )
            )
        )
    })
    
    # Valid comparisons checkbox (only shown for multi-way designs)
    output$valid_comparisons_ui <- shiny::renderUI({
        x_axis_cols <- x_axis()
        
        # Only show if more than 1 X-axis column selected
        if (length(x_axis_cols) > 1) {
            shiny::checkboxInput(
                inputId = ns("valid_comparisons"),
                label = shiny::tags$span(
                    "Compute only valid comparisons ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        paste0(
                            "If you have a two-way or three-way design and you want to compute ",
                            "only the valid comparisons then check this box. This will reduce ",
                            "the number of comparisons. Importantly the p-value is differently adjusted. ",
                            "Valid comparisons are defined as comparisons between groups where only one ",
                            "level is different. E.g. either MATERIAL or TREATMENT is different but not both."
                        )
                    )
                ),
                value = TRUE
            )
        }
    })
}


#' Create Statistics Parameters Reactive
#'
#' Collects all statistics-related input parameters into a single reactive.
#' This follows the unified debouncing pattern but without debouncing since
#' computation is triggered by button click.
#'
#' @param input Shiny input object from the parent module
#' @param x_axis Reactive containing selected X-axis columns from plotting
#' @return Reactive containing all statistics parameters
create_statistics_params <- function(input, x_axis) {
    shiny::reactive({
        list(
            # Options tab
            show_additional_output = input$show_additional_output %||% FALSE,
            use_scientific_notation = input$use_scientific_notation %||% FALSE,
            filter_p_values = input$filter_p_values %||% FALSE,
            valid_comparisons = input$valid_comparisons %||% TRUE,
            
            # Bootstrap tab
            use_bootstrap = input$use_bootstrap %||% FALSE,
            boot_samples = input$boot_samples %||% 599,
            boot_sample_size = input$boot_sample_size,  # NA allowed
            
            # Adjustments tab
            p_val_cor_method = input$p_val_cor_method %||% "bonferroni",
            
            # X-axis info (for valid comparisons logic)
            x_axis_count = length(x_axis())
        )
    })
}
