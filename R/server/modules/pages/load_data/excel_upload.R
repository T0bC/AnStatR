# Excel file upload and validation logic
# This file handles the observeEvent for file uploads

shiny::observeEvent(input$data_file, {
  shiny::req(input$data_file)

  file_info <- input$data_file
  file_ext <- tolower(tools::file_ext(file_info$name))

  # Validate file type
  if (!identical(file_ext, "xlsx")) {
    loaded_data(NULL)
    shiny::showNotification(
      "Only XLSX files are currently supported.",
      type = "warning"
    )
    return()
  }

  # Read the Excel file
  data <- openxlsx::read.xlsx(
    xlsxFile = file_info$datapath,
    sheet = 1
  )

  # Validate the data structure
  if (!is.data.frame(data) || nrow(data) == 0) {
    loaded_data(NULL)
    shiny::showNotification(
      "The uploaded file appears to be empty or invalid.",
      type = "error"
    )
    return()
  }

  # Update reactive value with result
  loaded_data(data)
  shiny::showNotification(
    "Data loaded successfully!",
    type = "message",
    duration = 3
  )
})
