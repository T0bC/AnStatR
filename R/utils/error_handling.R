#' Error Handling Utilities for TexAn 2.0
#'
#' This module provides centralized error handling for the Shiny application,
#' including stack trace capture, logging, and user-friendly error displays.

#' Configure global Shiny error handling options
#'
#' Sets up the global shiny.error handler and related options.
#' Call this once during app initialization.
#'
#' @return NULL invisibly
setup_global_error_handler <- function() {
  # Set global Shiny error handler
  options(
    shiny.error = function() {
      # Global fallback error handler
      # This catches errors that bubble up without being handled by safe_run()
      
      # Capture the error
      err <- geterrmessage()
      
      # Log the unhandled error
      log_error("Unhandled Shiny error: {msg}", msg = err)
      
      # Try to get the current session (may not always be available)
      session <- shiny::getDefaultReactiveDomain()
      
      if (!is.null(session)) {
        # Show a generic error modal to the user
        shiny::showModal(
          shiny::modalDialog(
            title = shiny::tags$span(
              shiny::icon("exclamation-triangle"),
              " Unexpected Error"
            ),
            shiny::tagList(
              shiny::p(
                "An unexpected error occurred. The application is still running,",
                "but you may need to refresh the page if things don't work correctly."
              ),
              shiny::tags$details(
                shiny::tags$summary(
                  shiny::tags$strong("Technical details (click to expand)")
                ),
                shiny::tags$pre(
                  style = "max-height: 300px; overflow-y: auto; background-color: #f5f5f5; padding: 10px;",
                  err
                )
              ),
              shiny::tags$hr(),
              shiny::tags$small(
                shiny::tags$em(
                  "This error has been logged. If you see this message repeatedly,",
                  "please contact support."
                )
              )
            ),
            easyClose = TRUE,
            footer = shiny::modalButton("Close")
          )
        )
      }
    },
    # Prevent Shiny's default red error messages in the UI
    # We handle all error display through our custom handlers
    shiny.sanitize.errors = FALSE
  )
  
  log_info("Global error handler configured")
  invisible(NULL)
}

#' Format a stack trace for display
#'
#' @param trace A trace object from rlang::trace_back() or sys.calls()
#' @return A formatted string representation of the trace
format_trace <- function(trace) {
  if (inherits(trace, "rlang_trace")) {
    # rlang trace object
    return(format(trace))
  } else if (is.list(trace)) {
    # sys.calls() output
    calls <- sapply(trace, function(call) {
      paste(deparse(call, width.cutoff = 500), collapse = " ")
    })
    return(paste(seq_along(calls), calls, sep = ": ", collapse = "\n"))
  } else {
    return(as.character(trace))
  }
}

#' Central error handler for the application
#'
#' Captures errors, logs them with context, and displays user-friendly messages
#' with optional technical details.
#'
#' @param error The error object caught by tryCatch
#' @param context String describing where the error occurred (e.g., "load_data:read_excel")
#' @param session The Shiny session object
#' @param user_msg User-friendly error message to display
#' @param show_modal Logical; if TRUE, shows a modal dialog (default). If FALSE, shows a notification.
#' @return NULL invisibly
handle_app_error <- function(error,
                              context,
                              session,
                              user_msg = "An unexpected error occurred.",
                              show_modal = TRUE) {
  
  # Capture stack trace
  trace <- tryCatch(
    {
      if (requireNamespace("rlang", quietly = TRUE)) {
        rlang::trace_back()
      } else {
        sys.calls()
      }
    },
    error = function(e) {
      "Stack trace unavailable"
    }
  )
  
  # Format error details
  error_msg <- conditionMessage(error)
  trace_text <- format_trace(trace)
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # Get session info (safely)
  session_info <- tryCatch(
    {
      list(
        session_token = session$token,
        client_data = session$clientData
      )
    },
    error = function(e) {
      list(session_token = "unavailable")
    }
  )
  
  # Log the error with full details
  if (requireNamespace("logger", quietly = TRUE)) {
    # Log the main error message
    logger::log_error(
      "Error in {context}: {error_msg}",
      context = context,
      error_msg = error_msg
    )
    
    # Log the stack trace on separate lines for better readability
    logger::log_error("Stack trace:")
    trace_lines <- strsplit(trace_text, "\n")[[1]]
    for (line in trace_lines) {
      logger::log_error("  {line}", line = line)
    }
    
    # Log session info
    logger::log_error("Session: {token}", token = session_info$session_token)
  } else {
    # Fallback logging to console
    cat(
      sprintf(
        "[ERROR] %s | Context: %s | Message: %s\n",
        timestamp, context, error_msg
      ),
      file = stderr()
    )
  }
  
  # Display to user
  if (show_modal) {
    shiny::showModal(
      shiny::modalDialog(
        title = shiny::tags$span(
          shiny::icon("exclamation-triangle"),
          " Something went wrong"
        ),
        shiny::tagList(
          shiny::p(user_msg),
          shiny::tags$details(
            shiny::tags$summary(
              shiny::tags$strong("Technical details (click to expand)")
            ),
            shiny::tags$div(
              style = "margin-top: 10px;",
              shiny::tags$p(
                shiny::tags$strong("Error message:"),
                shiny::tags$br(),
                error_msg
              ),
              shiny::tags$p(
                shiny::tags$strong("Context:"),
                shiny::tags$br(),
                context
              ),
              shiny::tags$p(
                shiny::tags$strong("Timestamp:"),
                shiny::tags$br(),
                timestamp
              ),
              shiny::tags$p(
                shiny::tags$strong("Stack trace:"),
                shiny::tags$br(),
                shiny::tags$pre(
                  style = "max-height: 300px; overflow-y: auto; background-color: #f5f5f5; padding: 10px; border-radius: 4px;",
                  trace_text
                )
              )
            )
          ),
          shiny::tags$hr(),
          shiny::tags$small(
            shiny::tags$em(
              "This error has been logged. If the problem persists, please contact support with the timestamp above."
            )
          )
        ),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      )
    )
  } else {
    # Show notification instead
    shiny::showNotification(
      ui = shiny::tagList(
        shiny::tags$strong(user_msg),
        shiny::tags$br(),
        shiny::tags$small(
          shiny::tags$em("Error logged at ", timestamp)
        )
      ),
      type = "error",
      duration = 10
    )
  }
  
  invisible(NULL)
}

#' Safe execution wrapper
#'
#' Wraps risky code in a tryCatch and delegates errors to handle_app_error.
#' This keeps the app responsive and provides consistent error handling.
#'
#' @param expr Expression to execute safely
#' @param context String describing the operation context
#' @param session The Shiny session object
#' @param user_msg User-friendly error message
#' @param show_modal Logical; whether to show modal (TRUE) or notification (FALSE)
#' @param on_error Optional callback function to execute on error (receives the error object)
#' @return The result of expr if successful, NULL if an error occurred
safe_run <- function(expr,
                     context,
                     session,
                     user_msg = "An unexpected error occurred.",
                     show_modal = TRUE,
                     on_error = NULL) {
  
  tryCatch(
    {
      expr
    },
    error = function(e) {
      # Handle the error
      handle_app_error(
        error = e,
        context = context,
        session = session,
        user_msg = user_msg,
        show_modal = show_modal
      )
      
      # Execute custom error callback if provided
      if (!is.null(on_error) && is.function(on_error)) {
        tryCatch(
          on_error(e),
          error = function(callback_error) {
            if (requireNamespace("logger", quietly = TRUE)) {
              logger::log_warn(
                "Error in on_error callback: {msg}",
                msg = conditionMessage(callback_error)
              )
            }
          }
        )
      }
      
      # Return NULL to signal failure
      NULL
    }
  )
}

#' Validate with safe error handling
#'
#' Combines Shiny's validate() with safe error handling for validation logic
#' that might itself throw errors.
#'
#' @param ... Validation expressions to evaluate
#' @param context String describing the validation context
#' @param session The Shiny session object
#' @return TRUE if all validations pass, FALSE otherwise
safe_validate <- function(..., context = "validation", session = NULL) {
  tryCatch(
    {
      shiny::validate(...)
      TRUE
    },
    error = function(e) {
      if (!is.null(session)) {
        if (requireNamespace("logger", quietly = TRUE)) {
          logger::log_warn(
            "Validation failed in {context}: {msg}",
            context = context,
            msg = conditionMessage(e)
          )
        }
      }
      FALSE
    }
  )
}
