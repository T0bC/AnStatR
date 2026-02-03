#' Options Tab UI Component
#'
#' Creates the Options tab for the statistics sidebar.
#' Contains main options like trimming display and output toggles.
#'
#' @param ns Namespace function from parent module
#' @return A bslib::nav_panel element
#' @export
create_options_tab <- function(ns) {
    bslib::nav_panel(
        title = bslib::tooltip(
            bsicons::bs_icon("gear", size = "1.2em"),
            "Options"
        ),
        value = "options_tab",
        shiny::tags$div(
            class = "pt-3",
            shiny::h6(class = "text-muted mb-3", "Options"),
            
            # Trim value display (read from plotting module)
            shiny::uiOutput(ns("trim_value_display")),
            
            shiny::tags$hr(),
            
            # Output options
            shiny::fluidRow(
                shiny::column(
                    6,
                    shiny::checkboxInput(
                        inputId = ns("show_additional_output"),
                        label = shiny::tags$span(
                            "Additional Output ",
                            bslib::tooltip(
                                bsicons::bs_icon("info-circle", class = "text-muted"),
                                "Show additional output like QQ and Density plots, and detailed information about the statistical tests."
                            )
                        ),
                        value = FALSE
                    )
                ),
                shiny::column(
                    6,
                    shiny::checkboxInput(
                        inputId = ns("use_scientific_notation"),
                        label = shiny::tags$span(
                            "Scientific Notation ",
                            bslib::tooltip(
                                bsicons::bs_icon("info-circle", class = "text-muted"),
                                "Use scientific notation for the results. E.g. 2.34e-05"
                            )
                        ),
                        value = FALSE
                    )
                )
            ),
            
            shiny::tags$hr(),
            
            # Statistical approach selection
            shiny::radioButtons(
                inputId = ns("test_approach"),
                label = "Statistical Approach:",
                choices = list(
                    "Robust Tests" = "robust",
                    "Parametric Tests" = "parametric"
                ),
                selected = "robust"
            ),
            
            # Accordion with detailed information
            bslib::accordion(
                id = ns("test_approach_accordion"),
                bslib::accordion_panel(
                    title = "When to use which approach?",
                    value = "test_approach_info",
                    shiny::uiOutput(ns("approach_details"))
                )
            ),
            
            shiny::tags$hr(),
            
            # Filter significant p-values
            shiny::checkboxInput(
                inputId = ns("filter_p_values"),
                label = shiny::tags$span(
                    "Show only significant p-values ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        "Filter the results table for p-values smaller than 0.07. Please check the p.hat (effect size) always when interpreting p-values!"
                    )
                ),
                value = FALSE
            ),
            
            # Valid comparisons (conditionally shown based on X-axis count)
            shiny::uiOutput(ns("valid_comparisons_ui"))
        )
    )
}
