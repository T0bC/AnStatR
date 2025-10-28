#' Admin Panel Server Module
#'
#' Handles log file reading, display, and management.

#' Admin Panel Server
#'
#' @param id Module namespace ID
#' @return Reactive containing admin panel state
server_admin <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values to store log data
    log_data <- shiny::reactiveValues(
      files = character(0),
      current_content = character(0),
      stats = list(
        total = 0,
        errors = 0,
        warnings = 0,
        info = 0
      )
    )
    
    # Load available log files
    load_log_files <- function() {
      source("R/utils/logging.R", local = TRUE)
      files <- get_log_files()
      
      if (length(files) > 0) {
        # Create nice labels with date and file size
        labels <- sapply(files, function(f) {
          size <- file.size(f)
          size_kb <- round(size / 1024, 1)
          date <- basename(f)
          sprintf("%s (%s KB)", date, size_kb)
        })
        names(files) <- labels
      }
      
      return(files)
    }
    
    # Initialize log file list
    shiny::observe({
      files <- load_log_files()
      log_data$files <- files
      
      shiny::updateSelectInput(
        session = session,
        inputId = "log_file_select",
        choices = if (length(files) > 0) files else list("No log files found" = ""),
        selected = if (length(files) > 0) files[1] else ""
      )
    })
    
    # Read and display log file
    read_current_log <- shiny::reactive({
      # Trigger on refresh button or file selection change
      input$refresh_logs
      selected_file <- input$log_file_select
      n_lines <- input$n_lines
      
      if (is.null(selected_file) || selected_file == "" || !file.exists(selected_file)) {
        return(character(0))
      }
      
      source("R/utils/logging.R", local = TRUE)
      content <- read_log_file(selected_file, n_lines = n_lines)
      
      return(content)
    })
    
    # Update log content and statistics
    shiny::observe({
      content <- read_current_log()
      log_data$current_content <- content
      
      # Calculate statistics
      if (length(content) > 0) {
        log_data$stats$total <- length(content)
        log_data$stats$errors <- sum(grepl("\\[ERROR\\]", content, ignore.case = TRUE))
        log_data$stats$warnings <- sum(grepl("\\[WARN", content, ignore.case = TRUE))
        log_data$stats$info <- sum(grepl("\\[INFO\\]", content, ignore.case = TRUE))
      } else {
        log_data$stats <- list(total = 0, errors = 0, warnings = 0, info = 0)
      }
    })
    
    # Render log contents
    output$log_contents <- shiny::renderText({
      content <- log_data$current_content
      
      if (length(content) == 0) {
        return("No log content available. Select a log file and click Refresh.")
      }
      
      paste(content, collapse = "\n")
    })
    
    # Render statistics
    output$total_lines <- shiny::renderText({
      format(log_data$stats$total, big.mark = ",")
    })
    
    output$error_count <- shiny::renderText({
      format(log_data$stats$errors, big.mark = ",")
    })
    
    output$warning_count <- shiny::renderText({
      format(log_data$stats$warnings, big.mark = ",")
    })
    
    output$info_count <- shiny::renderText({
      format(log_data$stats$info, big.mark = ",")
    })
    
    # Download handler
    output$download_log <- shiny::downloadHandler(
      filename = function() {
        selected_file <- input$log_file_select
        if (!is.null(selected_file) && selected_file != "") {
          basename(selected_file)
        } else {
          paste0("app_log_", Sys.Date(), ".log")
        }
      },
      content = function(file) {
        selected_file <- input$log_file_select
        if (!is.null(selected_file) && file.exists(selected_file)) {
          file.copy(selected_file, file)
        } else {
          writeLines("No log file selected", file)
        }
      }
    )
    
    # Cleanup old logs
    shiny::observeEvent(input$cleanup_logs, {
      days <- input$days_to_keep
      
      shiny::showModal(
        shiny::modalDialog(
          title = "Confirm Log Cleanup",
          sprintf(
            "Are you sure you want to delete log files older than %d days?",
            days
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("confirm_cleanup"),
              "Yes, Delete",
              class = "btn-danger"
            )
          )
        )
      )
    })
    
    # Confirm cleanup
    shiny::observeEvent(input$confirm_cleanup, {
      days <- input$days_to_keep
      
      source("R/utils/logging.R", local = TRUE)
      n_deleted <- cleanup_old_logs(days_to_keep = days)
      
      shiny::removeModal()
      
      shiny::showNotification(
        sprintf("Deleted %d old log file(s)", n_deleted),
        type = if (n_deleted > 0) "message" else "warning",
        duration = 5
      )
      
      # Refresh file list
      files <- load_log_files()
      log_data$files <- files
      
      shiny::updateSelectInput(
        session = session,
        inputId = "log_file_select",
        choices = if (length(files) > 0) files else list("No log files found" = ""),
        selected = if (length(files) > 0) files[1] else ""
      )
    })
    
    # Return reactive with current log state
    shiny::reactive({
      list(
        current_file = input$log_file_select,
        stats = log_data$stats
      )
    })
  })
}
