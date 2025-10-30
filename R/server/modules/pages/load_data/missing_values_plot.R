# Missing values plot rendering logic
# This file handles data quality visualization using DataExplorer

output$missing_values_plot <- shiny::renderPlot({
  shiny::req(loaded_data())
  
  data <- loaded_data()
  
  # Generate the missing values plot
  DataExplorer::plot_missing(data)
})
