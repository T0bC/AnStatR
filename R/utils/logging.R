#' Logging Utilities for TexAn 2.0
#'
#' Initializes and configures the logging system for the application.

# Helper operator for string concatenation
`%R%` <- function(x, y) paste0(x, y)

#' Safe logging wrappers
#'
#' These functions check if logger is available before logging,
#' making code cleaner and less verbose.

#' Log info message
#' @param msg Message template
#' @param ... Named arguments for message interpolation
log_info <- function(msg, ...) {
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_info(msg, ...)
  }
}

#' Log warning message
#' @param msg Message template
#' @param ... Named arguments for message interpolation
log_warn <- function(msg, ...) {
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_warn(msg, ...)
  }
}

#' Log error message
#' @param msg Message template
#' @param ... Named arguments for message interpolation
log_error <- function(msg, ...) {
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_error(msg, ...)
  }
}

#' Log debug message
#' @param msg Message template
#' @param ... Named arguments for message interpolation
log_debug <- function(msg, ...) {
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_debug(msg, ...)
  }
}

#' Initialize the application logging system
#'
#' Sets up daily log files with appropriate appenders and formatters.
#' Logs are written to the logs/ directory with date-based filenames.
#'
#' @param log_dir Directory where log files should be stored (default: "logs")
#' @param log_level Minimum log level to record (default: "INFO")
#' @param console_log Whether to also log to console (default: TRUE)
#' @return NULL invisibly
init_logging <- function(log_dir = "logs",
                         log_level = "INFO",
                         console_log = TRUE) {
  
  # Check if logger package is available
  if (!requireNamespace("logger", quietly = TRUE)) {
    warning(
      "The 'logger' package is not installed. ",
      "Logging will be limited. Install with: install.packages('logger')"
    )
    return(invisible(NULL))
  }
  
  # Create log directory if it doesn't exist
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Set log level
  logger::log_threshold(log_level)
  
  # Create daily log file path
  log_file <- file.path(log_dir, paste0(Sys.Date(), ".log"))
  
  # Configure appender based on console_log preference
  if (console_log) {
    # Log to both file and console
    logger::log_appender(logger::appender_tee(log_file))
  } else {
    # Log to file only
    logger::log_appender(logger::appender_file(log_file))
  }
  
  # Set a detailed formatter
  logger::log_formatter(logger::formatter_glue_or_sprintf)
  
  # Set layout with timestamp
  logger::log_layout(logger::layout_glue_generator(
    format = '[{time}] [{level}] {msg}'
  ))
  
  # Log initialization
  logger::log_info("Logging system initialized")
  logger::log_info("Log file: {log_file}", log_file = log_file)
  logger::log_info("Log level: {log_level}", log_level = log_level)
  
  invisible(NULL)
}

#' Get list of available log files
#'
#' @param log_dir Directory where log files are stored
#' @return Character vector of log file paths, sorted by date (newest first)
get_log_files <- function(log_dir = "logs") {
  if (!dir.exists(log_dir)) {
    return(character(0))
  }
  
  log_files <- list.files(
    log_dir,
    pattern = "\\.log$",
    full.names = TRUE
  )
  
  # Sort by modification time, newest first
  if (length(log_files) > 0) {
    log_files <- log_files[order(file.mtime(log_files), decreasing = TRUE)]
  }
  
  return(log_files)
}

#' Read log file contents
#'
#' @param log_file Path to the log file to read
#' @param n_lines Number of lines to read from the end of the file (default: all)
#' @return Character vector of log lines
read_log_file <- function(log_file, n_lines = NULL) {
  if (!file.exists(log_file)) {
    return(character(0))
  }
  
  lines <- readLines(log_file, warn = FALSE)
  
  if (!is.null(n_lines) && n_lines > 0) {
    # Return last n_lines
    start_idx <- max(1, length(lines) - n_lines + 1)
    lines <- lines[start_idx:length(lines)]
  }
  
  return(lines)
}

#' Clean up old log files
#'
#' Removes log files older than a specified number of days.
#'
#' @param log_dir Directory where log files are stored
#' @param days_to_keep Number of days of logs to retain (default: 30)
#' @return Number of files deleted
cleanup_old_logs <- function(log_dir = "logs", days_to_keep = 30) {
  if (!dir.exists(log_dir)) {
    return(0)
  }
  
  log_files <- list.files(
    log_dir,
    pattern = "\\.log$",
    full.names = TRUE
  )
  
  if (length(log_files) == 0) {
    return(0)
  }
  
  # Calculate cutoff date
  cutoff_date <- Sys.Date() - days_to_keep
  
  # Find files to delete
  files_to_delete <- character(0)
  for (log_file in log_files) {
    file_date <- tryCatch(
      {
        # Extract date from filename (assumes YYYY-MM-DD.log format)
        basename_file <- basename(log_file)
        date_str <- sub("\\.log$", "", basename_file)
        as.Date(date_str)
      },
      error = function(e) {
        # If we can't parse the date, use file modification time
        as.Date(file.mtime(log_file))
      }
    )
    
    if (file_date < cutoff_date) {
      files_to_delete <- c(files_to_delete, log_file)
    }
  }
  
  # Delete old files
  if (length(files_to_delete) > 0) {
    unlink(files_to_delete)
    if (requireNamespace("logger", quietly = TRUE)) {
      logger::log_info(
        "Cleaned up {n} old log files",
        n = length(files_to_delete)
      )
    }
  }
  
  return(length(files_to_delete))
}

#' Log application startup information
#'
#' Records useful diagnostic information when the app starts.
#'
#' @return NULL invisibly
log_app_startup <- function() {
  if (!requireNamespace("logger", quietly = TRUE)) {
    return(invisible(NULL))
  }
  
  logger::log_info("=" %R% strrep("=", 60))
  logger::log_info("TexAn 2.0 Application Starting")
  logger::log_info("=" %R% strrep("=", 60))
  logger::log_info("R version: {version}", version = R.version.string)
  logger::log_info("Platform: {platform}", platform = R.version$platform)
  logger::log_info("Working directory: {wd}", wd = getwd())
  
  # Log loaded packages
  loaded_pkgs <- (.packages())
  logger::log_info("Loaded packages: {pkgs}", pkgs = paste(loaded_pkgs, collapse = ", "))
  
  invisible(NULL)
}

#' Setup session logging
#'
#' Configures automatic logging of session start and end events.
#' Call this in the app server function.
#'
#' @param session The Shiny session object
#' @return NULL invisibly
setup_session_logging <- function(session) {
  # Log session start
  log_info("New session started: {token}", token = session$token)
  
  # Log session end
  session$onSessionEnded(function() {
    log_info("Session ended: {token}", token = session$token)
  })
  
  invisible(NULL)
}
