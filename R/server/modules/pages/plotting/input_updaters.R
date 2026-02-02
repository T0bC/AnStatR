#' Input Updaters Component
#'
#' Handles resetting inputs when new data is loaded and updating selectize
#' input choices based on data columns and user selections.
#'
#' Following the explicit dependency injection pattern:
#' - All dependencies are passed as explicit parameters
#' - No implicit scoping or global state access
#'
#' @param input Shiny input object from parent module
#' @param session Shiny session object from parent module
#' @param median_data Reactive containing the median-processed data
#' @param data_version Reactive integer that increments when new data is loaded
#' @param descriptive_cols Reactive returning descriptive column names
#' @param measurement_cols Reactive returning measurement column names
#' @param saved_filter_state ReactiveVal for filter persistence (reset on new data)
#' @return NULL (side effects only - registers observers)
setup_input_updaters <- function(input, 
                                  session, 
                                  median_data, 
                                  data_version,
                                  descriptive_cols,
                                  measurement_cols,
                                  saved_filter_state = NULL) {
    
    # Reset all inputs when new data is loaded
    if (!is.null(data_version)) {
        shiny::observeEvent(data_version(), {
            # Clear saved filter state for fresh start with new dataset
            if (!is.null(saved_filter_state)) saved_filter_state(list())
            
            shiny::updateSelectizeInput(session, "metaData", selected = character(0))
            shiny::updateSelectizeInput(session, "measureVar", selected = character(0))
            shiny::updateSelectizeInput(session, "hideCols", selected = character(0))
            shiny::updateSelectizeInput(session, "xAxis", selected = character(0))
            shiny::updateSelectizeInput(session, "tooltip", selected = character(0))
            shiny::updateSliderInput(session, "trim_slider", value = 0)
            shiny::updateCheckboxInput(session, "enableOutlierDetection", value = FALSE)
            shiny::updateRadioButtons(session, "detectOutlier", selected = "IQR")
            shiny::updateSliderInput(session, "standardFactor", value = 1.5)
            shiny::updateSliderInput(session, "probabilityFactor", value = 0.05)
            shiny::updateNumericInput(session, "bootstrapSamples", value = 1000)
        }, ignoreInit = TRUE)
    }
    
    # Update metaData choices when data changes
    shiny::observe({
        cols <- descriptive_cols()
        shiny::updateSelectizeInput(
            session, "metaData",
            choices = cols,
            selected = input$metaData
        )
    })
    
    # Update measureVar choices when data changes
    shiny::observe({
        cols <- measurement_cols()
        shiny::updateSelectizeInput(
            session, "measureVar",
            choices = cols,
            selected = input$measureVar
        )
    })
    
    # Update hideCols, xAxis, tooltip, pointShape choices based on selected metaData
    shiny::observe({
        selected_meta <- input$metaData
        if (is.null(selected_meta)) selected_meta <- character(0)
        
        # Update hideCols
        shiny::updateSelectizeInput(
            session, "hideCols",
            choices = selected_meta,
            selected = input$hideCols[input$hideCols %in% selected_meta]
        )
        
        # Update xAxis
        shiny::updateSelectizeInput(
            session, "xAxis",
            choices = selected_meta,
            selected = input$xAxis[input$xAxis %in% selected_meta]
        )
        
        # Update tooltip
        shiny::updateSelectizeInput(
            session, "tooltip",
            choices = selected_meta,
            selected = input$tooltip[input$tooltip %in% selected_meta]
        )
        
        # Update pointShape choices
        shiny::updateSelectizeInput(
            session, "pointShape",
            choices = selected_meta,
            selected = input$pointShape[input$pointShape %in% selected_meta]
        )
    })
    
    # Update pointColor choices based on X-axis selection
    shiny::observe({
        x_axis <- input$xAxis
        if (is.null(x_axis) || length(x_axis) == 0) {
            shiny::updateSelectizeInput(session, "pointColor", choices = character(0))
        } else {
            shiny::updateSelectizeInput(
                session, "pointColor",
                choices = x_axis,
                selected = if (is.null(input$pointColor)) x_axis[1] else input$pointColor
            )
        }
    })
}
