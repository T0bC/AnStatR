#' {Section2} Tab UI Component
#'
#' Creates the second sidebar tab for the {TabName} page.
#'
#' @param ns Namespace function from parent module
#' @return A bslib::nav_panel element
#'
#' USAGE: Copy to R/ui/{tabname}/ui_tab_section2.R
#'        Rename function and update content as needed
create_{tabname}_section2_tab <- function(ns) {
    bslib::nav_panel(
        title = bslib::tooltip(
            bsicons::bs_icon("gear", size = "1.2em"),
            "Section 2"
        ),
        value = "section2_tab",
        shiny::tags$div(
            class = "pt-3",
            shiny::h6(class = "text-muted mb-3", "Section 2 Title"),
            
            # Example: Numeric input
            shiny::numericInput(
                inputId = ns("numeric1"),
                label = shiny::tags$span(
                    "Numeric Value ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        "Enter a numeric value."
                    )
                ),
                value = 10,
                min = 1,
                max = 100
            ),
            
            # Example: FluidRow with columns
            shiny::tags$hr(),
            shiny::fluidRow(
                shiny::column(
                    6,
                    shiny::selectInput(
                        inputId = ns("select1"),
                        label = "Option A",
                        choices = c("Choice 1", "Choice 2", "Choice 3"),
                        selected = "Choice 1"
                    )
                ),
                shiny::column(
                    6,
                    shiny::selectInput(
                        inputId = ns("select2"),
                        label = "Option B",
                        choices = c("Choice 1", "Choice 2", "Choice 3"),
                        selected = "Choice 2"
                    )
                )
            )
        )
    )
}
