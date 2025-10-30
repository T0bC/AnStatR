# Missing values plot rendering logic
# This file defines a function that renders a missing values plot
#
# @param output Shiny output object
# @param output_id Character string for the output ID (e.g., "missing_values_plot")
# @param loaded_data ReactiveVal containing the loaded data
# @return NULL (side effects: creates output$missing_values_plot)

render_missing_values_plot <- function(output, output_id, loaded_data) {
  output[[output_id]] <- shiny::renderPlot({
    shiny::req(loaded_data())
    
    data <- loaded_data()
    
    # Generate the missing values plot
    DataExplorer::plot_missing(data)
  })
}
