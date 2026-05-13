box::use(
  config,
  logger,
  shiny,
)

#' Configure a session-aware log layout
#'
#' Sets a global logger layout that automatically includes a short session
#' token when called inside a Shiny reactive context. Outside of a reactive
#' context (e.g. during app startup), the session field shows "global".
#'
#' This must be called once per session (i.e. in main.R server()).
#' rhino_log_file in config.yml must point to a **directory**, not a file.
#' Each session writes to its own file: <log_dir>/YYYY_MM_DD_<sessid>.log
#'
#' Log output format:
#'   LEVEL [timestamp] [sess_abc123] Message text
#'
#' @export
configure_session_logging <- function() {
  log_dir <- config$get("rhino_log_file")
  is_production <- identical(Sys.getenv("R_CONFIG_ACTIVE"), "production")

  if (is.null(log_dir) || is.na(log_dir) || log_dir == "") {
    # No directory configured: output to console only
    logger$log_appender(logger$appender_stderr)
  } else {
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE, showWarnings = TRUE)
    }

    if (!dir.exists(log_dir)) {
      warning(paste0("Could not create log directory '", log_dir, "'. Falling back to console logging."))
      logger$log_appender(logger$appender_stderr)
    } else {
      # Build per-session filename: YYYY_MM_DD_<sessid>.log
      session <- shiny$getDefaultReactiveDomain()
      sess_id <- if (!is.null(session)) substr(session$token, 1, 8) else "global"
      date_str <- format(Sys.time(), "%Y_%m_%d")
      log_file <- file.path(log_dir, paste0(date_str, "_", sess_id, ".log"))

      if (is_production) {
        # Production: file only
        logger$log_appender(logger$appender_file(log_file))
      } else {
        # Local development: both console AND file
        logger$log_appender(logger$appender_tee(log_file))
      }
    }
  }

  logger$log_layout(session_layout)
}

#' Custom layout that prepends a short session ID
#' @param level Log level
#' @param msg Log message
#' @param namespace Logger namespace
#' @param .logcall The logging call
#' @param .topcall The top-level call
#' @param .topenv The top-level environment
session_layout <- function(level, msg, namespace = NA_character_,
                           .logcall = sys.call(), .topcall = sys.call(-1),
                           .topenv = parent.frame()) {
  session <- shiny$getDefaultReactiveDomain()
  sess_id <- if (!is.null(session)) {
    # Use first 8 chars of the session token for brevity
    substr(session$token, 1, 8)
  } else {
    "global"
  }

  paste0(
    attr(level, "level"), " ",
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
    "[", sess_id, "] ",
    paste(msg, collapse = "")
  )
}
