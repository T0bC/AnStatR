server_load_data <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    loaded_data <- shiny::reactiveVal(NULL)

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

    # Render data table with safe error handling
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

    # Render missing values plot with safe error handling
    output$missing_values_plot <- shiny::renderPlot({
      shiny::req(loaded_data())
      
      # Wrap plot generation in safe_run to catch any errors
      safe_run(
        expr = {
          data <- loaded_data()
          
          log_info("Generating missing values plot for dataset with {rows} rows and {cols} columns",
                   rows = nrow(data),
                   cols = ncol(data))
          
          # Validate data before plotting
          if (!is.data.frame(data)) {
            stop("Data must be a data frame to generate missing values plot.")
          }
          
          if (ncol(data) == 0) {
            stop("Dataset has no columns to analyze for missing values.")
          }
          
          # Generate the missing values plot
          plot <- DataExplorer::plot_missing(data)
          
          log_info("Missing values plot generated successfully")
          
          plot
        },
        context = "load_data:render_missing_plot",
        session = session,
        user_msg = "Unable to generate the missing values plot. Please check your data format.",
        show_modal = FALSE  # Use notification instead of modal for rendering errors
      )
    })

    # Return reactive with loaded data
    shiny::reactive({
      loaded_data()
    })
  })
}
