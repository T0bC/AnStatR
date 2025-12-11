#' Bootstrap Tab UI Component
#'
#' Creates the Bootstrap tab for the statistics sidebar.
#' Contains bootstrap options for statistical tests.
#'
#' @param ns Namespace function from parent module
#' @return A bslib::nav_panel element
create_bootstrap_tab <- function(ns) {
    bslib::nav_panel(
        title = bslib::tooltip(
            bsicons::bs_icon("arrow-repeat", size = "1.2em"),
            "Bootstrap"
        ),
        value = "bootstrap_tab",
        shiny::tags$div(
            class = "pt-3",
            shiny::h6(class = "text-muted mb-3", "Bootstrap Options"),
            
            # Bootstrap toggle
            shiny::checkboxInput(
                inputId = ns("use_bootstrap"),
                label = shiny::tags$span(
                    "Use bootstrap version ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        "If sample size of one group is very small or smaller than the other groups it is recommended to use the bootstrap version. This will give you a better approximation of the test results. Takes time to compute..."
                    )
                ),
                value = FALSE
            ),
            
            # Bootstrap options (shown when enabled)
            shiny::conditionalPanel(
                condition = "input.use_bootstrap == true",
                ns = ns,
                shiny::tags$div(
                    class = "mt-3",
                    shiny::fluidRow(
                        shiny::column(
                            6,
                            shiny::numericInput(
                                inputId = ns("boot_samples"),
                                label = shiny::tags$span(
                                    "Bootstrap samples ",
                                    bslib::tooltip(
                                        bsicons::bs_icon("info-circle", class = "text-muted"),
                                        "Number of bootstrap iterations. A value over 599 seems not to change the results significantly. Use this wisely, it takes a lot of time to compute the results."
                                    )
                                ),
                                value = 599,
                                min = 100,
                                max = 10000,
                                step = 100
                            )
                        ),
                        shiny::column(
                            6,
                            shiny::numericInput(
                                inputId = ns("boot_sample_size"),
                                label = shiny::tags$span(
                                    "Samples per bootstrap ",
                                    bslib::tooltip(
                                        bsicons::bs_icon("info-circle", class = "text-muted"),
                                        "Specify the number of samples for bootstrapping. Leaving this blank defaults to the size of the smallest group in your data. If a larger number is entered, the smallest group size will be used."
                                    )
                                ),
                                value = NA,
                                min = 1
                            )
                        )
                    )
                )
            ),
            
            # Info text
            shiny::tags$div(
                class = "small text-muted mt-3",
                shiny::tags$p(
                    "Bootstrap methods are recommended when:",
                    shiny::tags$ul(
                        shiny::tags$li("Sample sizes are very small"),
                        shiny::tags$li("Group sizes are unequal"),
                        shiny::tags$li("Data is heavily skewed")
                    )
                )
            )
        )
    )
}
