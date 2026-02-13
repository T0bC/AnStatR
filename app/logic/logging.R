box::use(
  logger,
  shiny,
)

#' Configure a session-aware log layout
#'
#' Sets a global logger layout that automatically includes a short session
#' token when called inside a Shiny reactive context. Outside of a reactive
#' context (e.g. during app startup), the session field shows "global".
#'
#' This must be called once at app startup (e.g. in main.R server).
#' No changes are needed to existing rhino$log$* calls — the layout
#' reads the current session via shiny::getDefaultReactiveDomain().
#'
#' Log output format:
#'   LEVEL [timestamp] [sess_abc123] Message text
#'
#' @export
configure_session_logging <- function() {
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
