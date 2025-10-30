# Data preview table rendering logic
# This file defines a function that renders a data preview table
#
# @param output Shiny output object
# @param output_id Character string for the output ID (e.g., "data_preview")
# @param loaded_data ReactiveVal containing the loaded data
# @return NULL (side effects: creates output$data_preview)

render_data_preview <- function(output, output_id, loaded_data) {
  output[[output_id]] <- DT::renderDataTable({
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
}
