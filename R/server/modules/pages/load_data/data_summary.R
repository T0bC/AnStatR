# Data summary rendering logic
# This file handles statistical summary generation using summarytools

output$data_summary <- shiny::renderUI({
  shiny::req(loaded_data())
  
  # Wrap summary generation in safe_run to catch any errors
  result <- safe_run(
    expr = {
      data <- loaded_data()
      
      log_info("Generating data summary for dataset with {rows} rows and {cols} columns",
               rows = nrow(data),
               cols = ncol(data))
      
      # Validate data before generating summary
      if (!is.data.frame(data)) {
        stop("Data must be a data frame to generate summary.")
      }
      
      if (ncol(data) == 0) {
        stop("Dataset has no columns to summarize.")
      }
      
      if (nrow(data) == 0) {
        stop("Dataset has no rows to summarize.")
      }
      
      # Generate the summary using summarytools
      # Use print method to get HTML string instead of view()
      summary_obj <- summarytools::dfSummary(
        data,
        max.distinct.values = 25
      )
      
      # Capture the HTML output as a character string
      summary_html <- capture.output(
        print(
          summary_obj,
          method = 'render',
          plain.ascii = FALSE,
          varnumbers = FALSE,
          valid.col = FALSE,
          graph.magnif = 0.5,
          style = 'grid',
          footnote = ''
        )
      )
      
      # Combine into single string
      summary_html <- paste(summary_html, collapse = "\n")
      
      log_info("Data summary generated successfully")
      
      # Return as HTML
      shiny::HTML(summary_html)
    },
    context = "load_data:render_data_summary",
    session = session,
    user_msg = "Unable to generate the data summary. Please check your data format.",
    show_modal = FALSE  # Use notification instead of modal for rendering errors
  )
  
  # Return result or a placeholder message if error occurred
  if (!is.null(result)) {
    result
  } else {
    shiny::HTML("<p style='color: #999; text-align: center; padding: 20px;'>Unable to generate summary. Please check the data.</p>")
  }
})
