box::use(
  bsicons,
  bslib,
  shiny,
)

box::use(
  app/view/components/sidebar_tabs,
)

#' @export
tab_ui <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "sliders2",
    tooltip_text = "P-Value Adjustment",
    value = "adjustments_tab",
    shiny$h6(class = "text-muted mb-3", "P-Value Adjustment"),
    # P-value correction method
    shiny$radioButtons(
      inputId = ns("p_val_cor_method"),
      label = shiny$tags$span(
        "Adjustment method ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          "Method to correct for multiple comparisons."
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
    shiny$tags$hr(),
    # Help text
    shiny$tags$div(
      class = "small text-muted",
      shiny$tags$p(
        shiny$tags$strong("About p-value adjustment:")
      ),
      shiny$tags$p(
        "Corrects for multiple comparisons."
      ),
      shiny$tags$ul(
        shiny$tags$li(
          shiny$tags$strong("Bonferroni:"),
          " Most conservative method"
        ),
        shiny$tags$li(
          shiny$tags$strong("BH/BY:"),
          " Less conservative, controls FDR"
        ),
        shiny$tags$li(
          shiny$tags$strong("Holm:"),
          " Step-down, more powerful than Bonferroni"
        ),
        shiny$tags$li(
          shiny$tags$strong("None:"),
          " No adjustment (use with caution)"
        )
      ),
      shiny$tags$p(
        class = "text-warning",
        bsicons$bs_icon("exclamation-triangle"),
        " Don't be evil and do p-hacking!"
      )
    )
  )
}
