# Data preview table rendering logic
# This file handles the DT::renderDataTable output

output$data_preview <- DT::renderDataTable({
  shiny::req(loaded_data())
  
  # Wrap rendering in safe_run to catch any display errors
  safe_run(
    expr = {
      data <- loaded_data()
      
      # Create DataTable with options
      DT::datatable(
        data,
        options = list(
          pageLength = 10,
          lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),
          scrollX = TRUE,
          dom = 'Blfrtip'  # Added 'l' for length menu
        ),
        rownames = FALSE
      )
    },
    context = "load_data:render_datatable",
    session = session,
    user_msg = "Unable to display the data table. The data may be corrupted.",
    show_modal = FALSE  # Use notification instead of modal for rendering errors
  )
})
