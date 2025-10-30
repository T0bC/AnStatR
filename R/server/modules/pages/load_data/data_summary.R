# Data summary rendering logic
# This file handles statistical summary generation using summarytools

output$data_summary <- shiny::renderUI({
  shiny::req(loaded_data())
  
  data <- loaded_data()
  
  # Generate the summary using summarytools
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
  
  # Return as HTML
  shiny::HTML(summary_html)
})
