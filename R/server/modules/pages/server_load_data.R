server_load_data <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$data_file, {
      # TODO: Implement data loading logic triggered on upload.
    })

    invisible(NULL)
  })
}
