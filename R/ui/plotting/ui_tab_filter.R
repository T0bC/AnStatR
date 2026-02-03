#' Filter Tab UI Component
#'
#' Creates the Filter tab for the plotting sidebar.
#'
#' @param ns Namespace function from parent module
#' @return A bslib::nav_panel element
#' @export
create_filter_tab <- function(ns) {
    bslib::nav_panel(
        title = bslib::tooltip(
            bsicons::bs_icon("funnel", size = "1.2em"),
            "Filter Data"
        ),
        value = "filter_tab",
        shiny::tags$div(
            class = "pt-3",
            shiny::h6(class = "text-muted mb-3", "Filter Data"),
            # Hide from filter option
            shiny::selectizeInput(
                inputId = ns("hideCols"),
                label = shiny::tags$span(
                    "Hide columns ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        "Hide selected descriptive columns from filtering but keep them for tooltips."
                    )
                ),
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Optional...")
            ),
            shiny::tags$hr(),
            # Filter checkboxes (rendered by server)
            shiny::uiOutput(ns("checkboxes"))
        )
    )
}
