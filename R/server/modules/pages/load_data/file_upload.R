# File upload and validation logic
# This file defines a function that handles file uploads (CSV and XLSX)
#
# @param input Shiny input object from the parent module (server_load_data).
#   This object is provided by moduleServer() and contains reactive references
#   to all UI inputs defined in ui_load_data.R, including:
#   - input$data_file: The uploaded file from fileInput(ns("data_file"))
#   - input$csv_has_header: Checkbox for CSV header setting
#   - input$csv_delimiter: Radio button for CSV delimiter
#   - input$csv_quote: Radio button for CSV quote character
#   We pass the entire input object (rather than individual values) because
#   observeEvent() needs a reactive reference to detect file uploads.
# @param loaded_data ReactiveVal to store the loaded data
# @param session Shiny session object (optional, for showing modals)
# @param data_version ReactiveVal to increment when new data is loaded (for downstream reset)
# @return NULL (side effects: updates loaded_data, increments data_version, shows notifications)

handle_file_upload <- function(input, loaded_data, session = NULL, data_version = NULL) {
  shiny::observeEvent(input$data_file, {
    shiny::req(input$data_file)

    file_info <- input$data_file
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
          quote_char <- input$csv_quote
          # Handle "None" option and invalid quote characters
          if (is.null(quote_char) || !is.character(quote_char) || 
              length(quote_char) != 1 || quote_char == "" || quote_char == "None") {
            quote_char <- ""
          }
          
          read.csv(
            file = file_info$datapath,
            header = input$csv_has_header,
            sep = input$csv_delimiter,
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

    # Validate column naming conventions
    validation <- validate_column_naming(data)
    
    if (!validation$valid && !is.null(session)) {
      # Show modal dialog with column naming warnings
      shiny::showModal(create_column_validation_modal(validation, session))
    }

    # Increment data version to signal downstream modules to reset
    if (!is.null(data_version)) {
      data_version(data_version() + 1)
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
