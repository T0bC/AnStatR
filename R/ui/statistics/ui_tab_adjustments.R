#' Adjustments Tab UI Component
#'
#' Creates the Adjustments tab for the statistics sidebar.
#' Contains p-value adjustment method selection.
#'
#' @param ns Namespace function from parent module
#' @return A bslib::nav_panel element
#' @export
create_adjustments_tab <- function(ns) {
    bslib::nav_panel(
        title = bslib::tooltip(
            bsicons::bs_icon("sliders2", size = "1.2em"),
            "P-Value Adjustment"
        ),
        value = "adjustments_tab",
        shiny::tags$div(
            class = "pt-3",
            shiny::h6(class = "text-muted mb-3", "P-Value Adjustment"),
            
            # P-value correction method
            shiny::radioButtons(
                inputId = ns("p_val_cor_method"),
                label = shiny::tags$span(
                    "Adjustment method ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        "Method to correct for multiple comparisons"
                    )
                ),
                choices = c(
                    "Holm" = "holm",
                    "Hochberg" = "hochberg",
                    "Hommel" = "hommel",
                    "Bonferroni" = "bonferroni",
                    "Benjamini-Hochberg (BH)" = "BH",
                    "Benjamini-Yekutieli (BY)" = "BY",
                    "FDR" = "fdr",
                    "None" = "none"
                ),
                selected = "bonferroni"
            ),
            
            shiny::tags$hr(),
            
            # Help text
            shiny::tags$div(
                class = "small text-muted",
                shiny::tags$p(
                    shiny::tags$strong("About p-value adjustment:")
                ),
                shiny::tags$p(
                    "The p-value adjustment is used to correct for multiple comparisons."
                ),
                shiny::tags$ul(
                    shiny::tags$li(
                        shiny::tags$strong("Bonferroni:"),
                        " Most conservative method"
                    ),
                    shiny::tags$li(
                        shiny::tags$strong("BH/BY:"),
                        " Less conservative, controls false discovery rate"
                    ),
                    shiny::tags$li(
                        shiny::tags$strong("Holm:"),
                        " Step-down procedure, more powerful than Bonferroni"
                    ),
                    shiny::tags$li(
                        shiny::tags$strong("None:"),
                        " No adjustment (use with caution)"
                    )
                ),
                shiny::tags$p(
                    class = "text-warning",
                    shiny::icon("exclamation-triangle"),
                    " Don't be evil and do p-hacking!"
                )
            )
        )
    )
}
