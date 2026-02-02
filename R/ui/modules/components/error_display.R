#' Global Error Display UI Components
#'
#' Provides standardized UI components for displaying errors across all modules.
#' Works with structured error objects from error_handling.R.


#' Render a structured error with expandable details
#'
#' Creates HTML output showing the error message with an expandable
#' details section showing context and stack trace (hidden by default).
#'
#' @param error_obj Structured error object from create_app_error()
#' @param show_icon Logical, whether to show an icon before the message
#' @param icon_name Character, Bootstrap icon name (default: "exclamation-triangle-fill")
#' @return Shiny tags object with error display
#' @export
render_app_error <- function(error_obj, show_icon = TRUE, 
                              icon_name = "exclamation-triangle-fill") {
    # Ensure message is length-one character
    error_message <- if (length(error_obj$message) > 1) {
        paste(error_obj$message, collapse = " ")
    } else if (is.null(error_obj$message)) {
        "An unknown error occurred"
    } else {
        as.character(error_obj$message)
    }
    
    # Main error message
    icon_element <- if (show_icon) {
        bsicons::bs_icon(icon_name, class = "me-2")
    } else {
        NULL
    }
    
    error_header <- shiny::tags$div(
        class = "app-error-message",
        icon_element,
        shiny::tags$strong(error_message)
    )
    
    # Build context info if available
    context_section <- NULL
    if (!is.null(error_obj$context) && length(error_obj$context) > 0) {
        context_items <- lapply(names(error_obj$context), function(key) {
            value <- error_obj$context[[key]]
            # Format value nicely - MUST be length-one character for Shiny tags
            formatted_value <- if (is.logical(value)) {
                if (length(value) > 1) paste(ifelse(value, "Yes", "No"), collapse = ", ")
                else ifelse(value, "Yes", "No")
            } else if (is.numeric(value)) {
                if (length(value) > 1) paste(as.character(value), collapse = ", ")
                else as.character(value)
            } else {
                # Ensure single string even if vector
                if (length(value) > 1) paste(as.character(value), collapse = ", ")
                else as.character(value)
            }
            shiny::tags$div(
                class = "app-error-context-item",
                style = "margin-left: 1rem; display: flex; gap: 0.5rem;",
                shiny::tags$span(
                    class = "app-error-context-key",
                    style = "min-width: 130px; color: #6c757d;",
                    paste0(key, ":")
                ),
                shiny::tags$span(
                    class = "app-error-context-value",
                    style = "font-family: monospace;",
                    formatted_value
                )
            )
        })
        context_section <- shiny::tags$div(
            class = "app-error-context-info mb-2",
            shiny::tags$div(class = "app-error-context-title", "Parameters:"),
            context_items
        )
    }
    
    # Stack trace section (filtered to app code only)
    trace_section <- NULL
    if (!is.null(error_obj$traces$stack_trace) && nchar(error_obj$traces$stack_trace) > 0) {
        trace_section <- shiny::tags$div(
            class = "app-error-trace-wrapper",
            shiny::tags$div(class = "app-error-context-title", "Stack Trace:"),
            shiny::tags$pre(
                class = "app-error-trace-pre",
                shiny::HTML(error_obj$traces$stack_trace)
            )
        )
    }
    
    # Combine context and trace into expandable details
    details_content <- NULL
    if (!is.null(context_section) || !is.null(trace_section)) {
        details_content <- shiny::tags$details(
            class = "app-error-details mt-2",
            shiny::tags$summary(
                bsicons::bs_icon("code-square"),
                " Details"
            ),
            shiny::tags$div(
                class = "app-error-details-content",
                context_section,
                trace_section
            )
        )
    }
    
    # Combine all sections
    shiny::tags$div(
        class = "app-error-container",
        error_header,
        details_content
    )
}


#' Create a simple error alert
#'
#' Creates a Bootstrap alert with an error message. For quick error display
#' without the full structured error format.
#'
#' @param message Character, the error message to display
#' @param type Character, alert type: "danger", "warning", "info" (default: "danger")
#' @param dismissible Logical, whether the alert can be dismissed
#' @param icon_name Character, Bootstrap icon name (NULL for no icon)
#' @return Shiny tags object with alert
#' @export
error_alert <- function(message, type = "danger", dismissible = FALSE,
                         icon_name = "exclamation-triangle-fill") {
    alert_class <- paste0("alert alert-", type)
    if (dismissible) {
        alert_class <- paste0(alert_class, " alert-dismissible fade show")
    }
    
    icon_element <- if (!is.null(icon_name)) {
        bsicons::bs_icon(icon_name, class = "me-2")
    } else {
        NULL
    }
    
    dismiss_button <- if (dismissible) {
        shiny::tags$button(
            type = "button",
            class = "btn-close",
            `data-bs-dismiss` = "alert",
            `aria-label` = "Close"
        )
    } else {
        NULL
    }
    
    shiny::tags$div(
        class = alert_class,
        role = "alert",
        icon_element,
        message,
        dismiss_button
    )
}


#' Create an error alert from a structured error object
#'
#' Wraps render_app_error() in a Bootstrap alert container.
#'
#' @param error_obj Structured error object from create_app_error()
#' @param type Character, alert type: "danger", "warning", "info" (default: "danger")
#' @return Shiny tags object with alert containing structured error
#' @export
error_alert_structured <- function(error_obj, type = "danger") {
    shiny::tags$div(
        class = paste0("alert alert-", type),
        role = "alert",
        render_app_error(error_obj, show_icon = FALSE)
    )
}


#' Show an error in a modal dialog
#'
#' Displays a structured error in a modal dialog. Useful for critical errors
#' that need user acknowledgment.
#'
#' @param error_obj Structured error object from create_app_error()
#' @param title Character, modal title (default: "Error")
#' @param session Shiny session object
#' @export
show_error_modal <- function(error_obj, title = "Error", session = shiny::getDefaultReactiveDomain()) {
    shiny::showModal(
        shiny::modalDialog(
            title = shiny::tags$span(
                bsicons::bs_icon("exclamation-triangle-fill", class = "text-danger me-2"),
                title
            ),
            render_app_error(error_obj, show_icon = FALSE),
            easyClose = TRUE,
            footer = shiny::modalButton("Close")
        ),
        session = session
    )
}


#' Show a simple error message in a modal dialog
#'
#' @param message Character, the error message
#' @param title Character, modal title (default: "Error")
#' @param session Shiny session object
#' @export
show_error_message_modal <- function(message, title = "Error", 
                                      session = shiny::getDefaultReactiveDomain()) {
    shiny::showModal(
        shiny::modalDialog(
            title = shiny::tags$span(
                bsicons::bs_icon("exclamation-triangle-fill", class = "text-danger me-2"),
                title
            ),
            shiny::tags$p(message),
            easyClose = TRUE,
            footer = shiny::modalButton("Close")
        ),
        session = session
    )
}
