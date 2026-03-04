box::use(
  cli,
  rhino,
  shiny,
  utils[capture.output],
)

#' Sanitize file paths in stack traces
#'
#' Converts absolute paths to relative app paths for security.
#' Removes server-specific path prefixes like /app/, /home/user/, C:\\Users\\...
#'
#' @param trace_text Character, raw stack trace text
#' @return Character, sanitized stack trace
sanitize_stack_paths <- function(trace_text) {
  if (is.null(trace_text) || length(trace_text) == 0) {
    return(trace_text)
  }

  # Replace any absolute path containing /app/ or \app\ with relative path
  # Handles: [C:/Users/.../app/...], [/home/.../app/...], etc.
  sanitized <- gsub(
    "\\[.+?[/\\\\](app[/\\\\].+?)\\]",
    "[\\1]",
    trace_text,
    perl = TRUE
  )

  # Normalize backslashes to forward slashes
  sanitized <- gsub("\\\\", "/", sanitized)

  # Remove /tmp/... paths (R session temp files)
  sanitized <- gsub(
    "\\[/tmp/.+?\\]",
    "[internal]",
    sanitized,
    perl = TRUE
  )

  # Remove Windows temp paths (with forward or back slashes)
  sanitized <- gsub(
    "\\[[A-Za-z]:.+?Temp[/\\\\].+?\\]",
    "[internal]",
    sanitized,
    ignore.case = TRUE,
    perl = TRUE
  )

  sanitized
}

#' Check if an object is a structured app error
#'
#' Uses "is_error" %in% names() to avoid warnings when checking data frames,
#' since data frames are lists and obj$is_error on a df triggers
#' "Unknown column" warning.
#'
#' @param obj Object to check
#' @return Logical, TRUE if obj is a structured error from create_app_error()
#' @export
is_app_error <- function(obj) {
  is.list(obj) &&
    !is.data.frame(obj) &&
    "is_error" %in% names(obj) &&
    isTRUE(obj$is_error)
}

#' Create a structured error object
#'
#' Returns a standardized error structure with user-friendly message,
#' raw error, a filtered stack trace, and context about the operation.
#'
#' Stack trace filtering:
#' - Captures output from shiny$printStackTrace()
#' - Filters to frames with file references [path#line] (app code only)
#' - Falls back to top 20 frames if no file refs found
#' - Converts ANSI colors to HTML via cli$ansi_html()
#'
#' @param user_msg Character, user-friendly error message
#' @param raw_msg Character, original error message from R
#' @param error_obj The error condition object (optional, for stack trace)
#' @param operation_name Character, name of the operation that failed
#' @param context List, optional context with parameters for debugging
#' @return List with is_error=TRUE and structured error information
#' @export
create_app_error <- function(user_msg, raw_msg = NULL, error_obj = NULL,
                             operation_name = "Operation", context = NULL) {
  stack_trace <- NULL
  if (!is.null(error_obj)) {
    stack_trace <- tryCatch(
      {
        raw_output <- capture.output(
          shiny$printStackTrace(error_obj),
          type = "message"
        )
        if (length(raw_output) > 0) {
          # Filter to only lines containing file references [path#line]
          # These are the frames from our application code
          filtered <- raw_output[grepl("\\[.*#[0-9]+\\]", raw_output)]
          trace_text <- if (length(filtered) > 0) {
            paste(cli$ansi_html(filtered), collapse = "\n")
          } else {
            # Fallback: show top frames if no file refs found
            paste(cli$ansi_html(head(raw_output, 20)), collapse = "\n")
          }
          # Sanitize paths for security (remove absolute paths)
          sanitize_stack_paths(trace_text)
        } else {
          NULL
        }
      },
      error = function(e) NULL
    )
  }

  rhino$log$error(
    "{operation_name}: {user_msg}"
  )
  if (!is.null(raw_msg) && raw_msg != user_msg) {
    rhino$log$error("  Raw: {raw_msg}")
  }
  if (!is.null(context) && length(context) > 0) {
    ctx_str <- paste(
      names(context),
      vapply(context, function(v) paste(v, collapse = ", "), character(1)),
      sep = "=", collapse = "; "
    )
    rhino$log$error("  Context: {ctx_str}")
  }
  if (!is.null(stack_trace)) {
    # Strip HTML tags for plain-text log output
    plain_trace <- gsub("<[^>]+>", "", stack_trace)
    rhino$log$error("  Stack trace:\n{plain_trace}")
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
#' Wraps an expression in error handling, returning a standardized
#' result structure. Uses shiny$captureStackTraces to enable stack
#' trace capture on errors.
#'
#' @param expr Expression to evaluate
#' @param operation_name Character, name of the operation for error messages
#' @param context List, optional context with parameters for debugging
#' @param error_parser Function, optional custom function to parse error
#'   messages into user-friendly versions. Receives (error_msg, operation_name).
#' @return List with success (logical), result (or NULL), error (or NULL)
#' @export
safe_execute <- function(expr, operation_name = "Operation", context = NULL,
                         error_parser = NULL) {
  expr_quoted <- substitute(expr)

  tryCatch(
    {
      result <- shiny$captureStackTraces(
        eval(expr_quoted, envir = parent.frame())
      )
      list(success = TRUE, result = result, error = NULL)
    },
    error = function(e) {
      error_msg <- conditionMessage(e)

      # Use custom parser if provided, otherwise default prefix
      user_msg <- if (!is.null(error_parser) && is.function(error_parser)) {
        error_parser(error_msg, operation_name)
      } else {
        paste0(operation_name, " failed: ", error_msg)
      }

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
  } else if (grepl("memory|allocat", error_msg, ignore.case = TRUE)) {
    paste0(operation_name, ": Out of memory. Try with smaller data.")
  } else if (grepl("NA|missing|NULL", error_msg, ignore.case = TRUE)) {
    paste0(operation_name, ": Missing or invalid data encountered.")
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}

#' Error parser for statistical tests
#'
#' Translates common statistical test error messages into user-friendly
#' versions. Parameter naming matches safe_execute() which passes
#' operation_name as second argument.
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
stat_error_parser <- function(error_msg, operation_name = "Test") {
  if (grepl("groups", error_msg, ignore.case = TRUE)) {
    paste0(
      operation_name,
      ": Insufficient groups for comparison. Need at least 2 groups."
    )
  } else if (grepl("sample size|observations", error_msg, ignore.case = TRUE)) {
    paste0(
      operation_name,
      ": Insufficient sample size in one or more groups."
    )
  } else if (grepl(
    "\\bNA\\b|\\bNAs\\b|NA values|missing values|is\\.na",
    error_msg,
    ignore.case = TRUE
  )) {
    paste0(operation_name, ": Too many missing values in the data.")
  } else if (grepl("variance|constant", error_msg, ignore.case = TRUE)) {
    paste0(
      operation_name,
      ": Cannot compute - one or more groups have zero variance."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}

#' Create a simple error without stack trace
#'
#' For validation failures where there is no R error condition,
#' e.g. empty data, missing columns, invalid user input.
#'
#' @param message Character, the error message
#' @param operation_name Character, name of the operation
#' @param context List, optional context
#' @return Structured error object
#' @export
simple_error <- function(message, operation_name = "Validation",
                         context = NULL) {
  create_app_error(
    user_msg = message,
    raw_msg = message,
    error_obj = NULL,
    operation_name = operation_name,
    context = context
  )
}
