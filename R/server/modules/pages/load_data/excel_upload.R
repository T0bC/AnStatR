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

    log_warn("User attempted to upload unsupported file type: {ext}", ext = file_ext)
    return()
  }

  # Use safe_run to handle Excel file reading
  result <- safe_run(
    expr = {
      # Log the operation (using clean wrapper)
      log_info("Reading Excel file: {name}", name = file_info$name)

      # Read the Excel file
      data <- openxlsx::read.xlsx(
        xlsxFile = file_info$datapath,
        sheet = 1
      )

      # Validate the data structure
      # When this stop() is called, safe_run catches it and shows the modal!
      if (!is.data.frame(data) || nrow(data) == 0) {
        stop("The uploaded file appears to be empty or invalid.")
      }

      # Log success
      log_info(
        "Successfully loaded data: {rows} rows, {cols} columns",
        rows = nrow(data),
        cols = ncol(data)
      )

      data
    },
    context = "load_data:read_excel",
    session = session,
    user_msg = paste(
      "Unable to read the uploaded Excel file.",
      "Please ensure it is a valid XLSX file with data in the first sheet."
    ),
    show_modal = TRUE,
    on_error = function(e) {
      # Custom error handling: reset loaded data
      loaded_data(NULL)
    }
  )
  
  # Update reactive value with result (NULL if error occurred)
  if (!is.null(result)) {
    loaded_data(result)
    shiny::showNotification(
      "Data loaded successfully!",
      type = "message",
      duration = 3
    )
  }
})
