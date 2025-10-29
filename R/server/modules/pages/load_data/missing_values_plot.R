# Missing values plot rendering logic
# This file handles data quality visualization using DataExplorer

output$missing_values_plot <- shiny::renderPlot({
  shiny::req(loaded_data())
  
  # Wrap plot generation in safe_run to catch any errors
  safe_run(
    expr = {
      data <- loaded_data()
      
      log_info("Generating missing values plot for dataset with {rows} rows and {cols} columns",
               rows = nrow(data),
               cols = ncol(data))
      
      # Validate data before plotting
      if (!is.data.frame(data)) {
        stop("Data must be a data frame to generate missing values plot.")
      }
      
      if (ncol(data) == 0) {
        stop("Dataset has no columns to analyze for missing values.")
      }
      
      # Generate the missing values plot
      plot <- DataExplorer::plot_missing(data)
      
      log_info("Missing values plot generated successfully")
      
      plot
    },
    context = "load_data:render_missing_plot",
    session = session,
    user_msg = "Unable to generate the missing values plot. Please check your data format.",
    show_modal = FALSE  # Use notification instead of modal for rendering errors
  )
})
