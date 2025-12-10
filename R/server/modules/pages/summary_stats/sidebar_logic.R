#' Setup sidebar UI logic for Summary Statistics
#'
#' Handles dynamic filter options UI and sorting options updates.
#'
#' @param input Shiny input object from the parent module
#' @param output Shiny output object from the parent module
#' @param session Shiny session object from the parent module
#' @param median_data Reactive containing the median-processed data
#' @param descriptive_cols Reactive returning descriptive column names
#' @param x_axis_col Reactive returning the default X-axis column name
setup_sidebar_logic <- function(input, output, session, median_data, 
                                 descriptive_cols, x_axis_col) {
    ns <- session$ns
    
    # Update sorting options when data changes
    shiny::observe({
        shiny::req(median_data())
        desc_cols <- descriptive_cols()
        
        shiny::updateSelectizeInput(
            session = session,
            inputId = "sorting_options",
            choices = c("Measurement", desc_cols),
            selected = "Measurement"
        )
    })
    
    # Track whether filter options are currently shown
    filter_shown <- shiny::reactiveVal(FALSE)
    
    # Render filter options UI conditionally
    output$filter_options_ui <- shiny::renderUI({
        shiny::req(input$sorting_options)
        
        # Only show filter options when "Measurement" is the only selection
        if (length(input$sorting_options) == 1 && input$sorting_options == "Measurement") {
            desc_cols <- descriptive_cols()
            default_col <- x_axis_col()
            
            # Use default X-axis column if available, otherwise first descriptive col
            selected <- if (!is.null(default_col) && default_col %in% desc_cols) {
                default_col
            } else if (length(desc_cols) > 0) {
                desc_cols[1]
            } else {
                NULL
            }
            
            shiny::tagList(
                shiny::selectizeInput(
                    inputId = ns("filter_options_select"),
                    label = "Filter by:",
                    choices = desc_cols,
                    selected = selected,
                    multiple = TRUE
                )
            )
        } else {
            NULL
        }
    })
    
    # Return reactive for current sorting mode
    list(
        is_measurement_mode = shiny::reactive({
            shiny::req(input$sorting_options)
            length(input$sorting_options) == 1 && input$sorting_options == "Measurement"
        })
    )
}
