# Data preview table rendering logic
# This file handles the DT::renderDataTable output

output$data_preview <- DT::renderDataTable({
  shiny::req(loaded_data())
  
  data <- loaded_data()
  
  # Create DataTable with options
  DT::datatable(
    data,
    options = list(
      pageLength = 10,
      lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),
      scrollX = TRUE,
      dom = 'Blfrtip'
    ),
    rownames = FALSE
  )
})
