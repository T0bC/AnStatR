# File upload and validation logic
# This file defines a function that handles file uploads (CSV and XLSX)
#
# @param data_file_input Reactive input from fileInput (input$data_file)
# @param csv_has_header Reactive input for CSV header setting
# @param csv_delimiter Reactive input for CSV delimiter
# @param csv_quote Reactive input for CSV quote character
# @param loaded_data ReactiveVal to store the loaded data
# @return NULL (side effects: updates loaded_data and shows notifications)

handle_file_upload <- function(data_file_input, 
                                csv_has_header, 
                                csv_delimiter, 
                                csv_quote, 
                                loaded_data) {
  shiny::observeEvent(data_file_input, {
    shiny::req(data_file_input)

    file_info <- data_file_input
    file_ext <- tolower(tools::file_ext(file_info$name))

    # Validate file type
    if (!file_ext %in% c("csv", "xlsx")) {
      loaded_data(NULL)
      shiny::showNotification(
        "Only CSV and XLSX files are supported.",
        type = "warning"
      )
      return()
    }

    # Read the file based on extension
    data <- tryCatch(
      {
        if (file_ext == "csv") {
          # Read CSV with user-specified settings
          quote_char <- csv_quote
          # Handle "None" option for quote character
          if (is.null(quote_char) || quote_char == "") {
            quote_char <- ""
          }
          
          read.csv(
            file = file_info$datapath,
            header = csv_has_header,
            sep = csv_delimiter,
            quote = quote_char,
            stringsAsFactors = FALSE
          )
        } else {
          # Read XLSX file
          openxlsx::read.xlsx(
            xlsxFile = file_info$datapath,
            sheet = 1
          )
        }
      },
      error = function(e) {
        shiny::showNotification(
          paste("Error reading file:", e$message),
          type = "error",
          duration = 5
        )
        return(NULL)
      }
    )

    # Check if reading failed
    if (is.null(data)) {
      loaded_data(NULL)
      return()
    }

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
      paste0("Data loaded successfully! (", nrow(data), " rows, ", ncol(data), " columns)"),
      type = "message",
      duration = 3
    )
  })
}
