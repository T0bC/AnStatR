#' Setup sidebar UI logic for Summary Statistics
#'
#' Handles dynamic filter options UI. Always uses "Measurement" mode.
#' Defaults to X-axis selection from Plotting tab for grouping.
#'
#' @param input Shiny input object from the parent module
#' @param output Shiny output object from the parent module
#' @param session Shiny session object from the parent module
#' @param descriptive_cols Reactive returning descriptive column names
#' @param x_axis Reactive returning X-axis columns from plotting tab
setup_sidebar_logic <- function(input, output, session, descriptive_cols, x_axis) {
    ns <- session$ns
    
    # Render filter options UI - uses X-axis from plotting as default
    output$filter_options_ui <- shiny::renderUI({
        desc_cols <- descriptive_cols()
        shiny::req(length(desc_cols) > 0)
        
        # Use X-axis selection from plotting as default
        x_axis_cols <- x_axis()
        
        # Filter to only include valid descriptive columns
        selected <- if (!is.null(x_axis_cols) && length(x_axis_cols) > 0) {
            x_axis_cols[x_axis_cols %in% desc_cols]
        } else {
            character(0)
        }
        
        # Fallback to first descriptive col if no valid x_axis selection
        if (length(selected) == 0 && length(desc_cols) > 0) {
            selected <- desc_cols[1]
        }
        
        shiny::selectizeInput(
            inputId = ns("filter_options_select"),
            label = "Group by:",
            choices = desc_cols,
            selected = selected,
            multiple = TRUE
        )
    })
}
