server_load_data <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    loaded_data <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$data_file, {
      shiny::req(input$data_file)

      file_info <- input$data_file
      file_ext <- tolower(tools::file_ext(file_info$name))

      if (identical(file_ext, "xlsx")) {
        tryCatch(
          {
            data <- openxlsx::read.xlsx(
              xlsxFile = file_info$datapath,
              sheet = 1
            )
            loaded_data(data)
          },
          error = function(err) {
            loaded_data(NULL)
            shiny::showNotification(
              paste("Failed to read Excel file:", err$message),
              type = "error"
            )
          }
        )
      } else {
        loaded_data(NULL)
        shiny::showNotification(
          "Only XLSX files are currently supported.",
          type = "warning"
        )
      }
    })

    output$data_preview <- DT::renderDataTable({
      shiny::req(loaded_data())
      loaded_data()
    })

    shiny::reactive({
      loaded_data()
    })
  })
}
