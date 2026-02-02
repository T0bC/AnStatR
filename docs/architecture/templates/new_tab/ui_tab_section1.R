#' {Section1} Tab UI Component
#'
#' Creates the first sidebar tab for the {TabName} page.
#'
#' @param ns Namespace function from parent module
#' @return A bslib::nav_panel element
#'
#' USAGE: Copy to R/server/modules/pages/{tabname}/ui_tab_section1.R
#'        Rename function and update content as needed
create_{tabname}_section1_tab <- function(ns) {
    bslib::nav_panel(
        title = bslib::tooltip(
            bsicons::bs_icon("table", size = "1.2em"),
            "Section 1"
        ),
        value = "section1_tab",
        shiny::tags$div(
            class = "pt-3",
            shiny::h6(class = "text-muted mb-3", "Section 1 Title"),
            
            # Example: SelectizeInput with tooltip
            shiny::selectizeInput(
                inputId = ns("input1"),
                label = shiny::tags$span(
                    "Input Label ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        "Help text explaining this input."
                    )
                ),
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Select...")
            ),
            
            # Example: Checkbox with tooltip
            shiny::checkboxInput(
                inputId = ns("checkbox1"),
                label = shiny::tags$span(
                    "Option Label ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        "Help text for this option."
                    )
                ),
                value = FALSE
            )
        )
    )
}
