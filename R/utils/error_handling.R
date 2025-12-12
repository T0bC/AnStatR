#' Global Error Handling Utilities
#'
#' Provides standardized error handling functions for use across all modules.
#' Includes error creation, safe execution wrappers, and error checking utilities.


#' Check if an object is a structured app error
#'
#' @param obj Object to check
#' @return Logical, TRUE if obj is a structured error from create_app_error()
#' @export
is_app_error <- function(obj) {
    is.list(obj) && isTRUE(obj$is_error)
}


#' Create a structured error object
#'
#' Returns a standardized error structure with message, raw error,
#' a filtered stack trace, and context about the operation.
#'
#' @param user_msg Character, user-friendly error message
#' @param raw_msg Character, original error message from R
#' @param error_obj The error condition object (optional, for stack trace capture)
#' @param operation_name Character, name of the operation that failed
#' @param context List, optional context with parameters for debugging
#' @return List with is_error=TRUE and structured error information
#' @export
create_app_error <- function(user_msg, raw_msg = NULL, error_obj = NULL, 
                              operation_name = "Operation", context = NULL) {
    # Capture stack trace filtered to app code only (frames with file references)
    stack_trace <- NULL
    if (!is.null(error_obj)) {
        stack_trace <- tryCatch(
            {
                raw_output <- utils::capture.output(shiny::printStackTrace(error_obj), type = "message")
                if (length(raw_output) > 0) {
                    # Filter to only lines containing file references [path#line]
                    # These are the frames from our application code
                    filtered <- raw_output[grepl("\\[.*#[0-9]+\\]", raw_output)]
                    if (length(filtered) > 0) {
                        paste(cli::ansi_html(filtered), collapse = "\n")
                    } else {
                        # Fallback: show top frames if no file refs found
                        paste(cli::ansi_html(head(raw_output, 20)), collapse = "\n")
                    }
                } else {
                    NULL
                }
            },
            error = function(e) NULL
        )
    }
    
    list(
        is_error = TRUE,
        operation_name = operation_name,
        message = user_msg,
        raw_message = raw_msg,
        context = context,
        traces = list(
            stack_trace = stack_trace
        ),
        timestamp = Sys.time()
    )
}


#' Safe execution wrapper for any operation
#'
#' Wraps an operation in error handling, returning a standardized
#' result structure with either results or a structured error object.
#'
#' @param expr Expression to evaluate
#' @param operation_name Character, name of the operation for error messages
#' @param context List, optional context with parameters for debugging
#' @param error_parser Function, optional custom function to parse error messages
#'   into user-friendly versions. Receives error message string, returns user message.
#' @return List with success flag and either result or structured error
#' @export
safe_execute <- function(expr, operation_name = "Operation", context = NULL,
                          error_parser = NULL) {
    # Capture the expression for evaluation with stack trace capture
    expr_quoted <- substitute(expr)
    
    tryCatch(
        {
            # Use captureStackTraces to enable stack trace capture on errors
            result <- shiny::captureStackTraces(eval(expr_quoted, envir = parent.frame()))
            list(success = TRUE, result = result, error = NULL)
        },
        error = function(e) {
            error_msg <- conditionMessage(e)
            
            # Use custom parser if provided, otherwise use default
            user_msg <- if (!is.null(error_parser) && is.function(error_parser)) {
                error_parser(error_msg, operation_name)
            } else {
                # Default: just prefix with operation name
                paste0(operation_name, " failed: ", error_msg)
            }
            
            # Create structured error with context
            error_struct <- create_app_error(
                user_msg = user_msg,
                raw_msg = error_msg,
                error_obj = e,
                operation_name = operation_name,
                context = context
            )
            
            list(success = FALSE, result = NULL, error = error_struct)
        }
    )
}


#' Default error message parser for common R errors
#'
#' Translates common R error messages into user-friendly versions.
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
default_error_parser <- function(error_msg, operation_name = "Operation") {
    if (grepl("cannot open|No such file", error_msg, ignore.case = TRUE)) {
        paste0(operation_name, ": File not found or cannot be opened.")
    } else if (grepl("permission denied", error_msg, ignore.case = TRUE)) {
        paste0(operation_name, ": Permission denied. Check file permissions.")
    } else if (grepl("connection|timeout", error_msg, ignore.case = TRUE)) {
        paste0(operation_name, ": Connection error. Please try again.")
    } else if (grepl("memory|allocation", error_msg, ignore.case = TRUE)) {
        paste0(operation_name, ": Out of memory. Try with smaller data.")
    } else if (grepl("NA|missing|NULL", error_msg, ignore.case = TRUE)) {
        paste0(operation_name, ": Missing or invalid data encountered.")
    } else {
        paste0(operation_name, " failed: ", error_msg)
    }
}


#' Error parser for statistical tests
#'
#' Translates common statistical test error messages into user-friendly versions.
#'
#' @param error_msg Character, the original error message
#' @param test_name Character, name of the statistical test
#' @return Character, user-friendly error message
#' @export
stat_error_parser <- function(error_msg, test_name = "Test") {
    if (grepl("groups", error_msg, ignore.case = TRUE)) {
        paste0(test_name, ": Insufficient groups for comparison. Need at least 2 groups with data.")
    } else if (grepl("sample size|observations", error_msg, ignore.case = TRUE)) {
        paste0(test_name, ": Insufficient sample size in one or more groups.")
    } else if (grepl("NA|missing", error_msg, ignore.case = TRUE)) {
        paste0(test_name, ": Too many missing values in the data.")
    } else if (grepl("variance|constant", error_msg, ignore.case = TRUE)) {
        paste0(test_name, ": Cannot compute - one or more groups have zero variance.")
    } else {
        paste0(test_name, " failed: ", error_msg)
    }
}


#' Create a simple error without stack trace
#'
#' For cases where you want to return an error structure without
#' an actual R error condition (e.g., validation failures).
#'
#' @param message Character, the error message
#' @param operation_name Character, name of the operation
#' @param context List, optional context
#' @return Structured error object
#' @export
simple_error <- function(message, operation_name = "Validation", context = NULL) {
    create_app_error(
        user_msg = message,
        raw_msg = message,
        error_obj = NULL,
        operation_name = operation_name,
        context = context
    )
}
