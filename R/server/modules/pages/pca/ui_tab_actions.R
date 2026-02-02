#' PCA Actions Tab UI Component
#'
#' Creates the Actions tab for the PCA sidebar.
#'
#' @param ns Namespace function from parent module
#' @return A bslib::nav_panel element
create_pca_actions_tab <- function(ns) {
    bslib::nav_panel(
        title = bslib::tooltip(
            bsicons::bs_icon("gear", size = "1.2em"),
            "Actions & Options"
        ),
        value = "actions_tab",
        shiny::tags$div(
            class = "pt-3",
            shiny::h6(class = "text-muted mb-3", "Actions & Options"),
            # Help button
            shiny::actionButton(
                inputId = ns("helpButton"),
                label = shiny::tags$span(
                    bsicons::bs_icon("question-circle"),
                    " Help"
                ),
                class = "btn-outline-primary btn-sm w-100 mb-3"
            ),
            shiny::tags$hr(),
            # Show additional output checkbox
            shiny::checkboxInput(
                inputId = ns("show_additional_pca_output"),
                label = shiny::tags$span(
                    "Show Additional Output ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        "Display additional PCA statistics and diagnostic plots."
                    )
                ),
                value = TRUE
            )
        )
    )
}
