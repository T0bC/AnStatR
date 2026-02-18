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
    icon = "arrow-repeat",
    tooltip_text = "Bootstrap",
    value = "bootstrap_tab",
    shiny$h6(class = "text-muted mb-3", "Bootstrap Options"),
    # Bootstrap toggle
    shiny$checkboxInput(
      inputId = ns("use_bootstrap"),
      label = shiny$tags$span(
        "Use bootstrap version ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Recommended when sample sizes are",
            "very small or unequal. Gives better",
            "approximation but takes longer."
          )
        )
      ),
      value = FALSE
    ),
    # Bootstrap options (shown when enabled)
    shiny$conditionalPanel(
      condition = "input.use_bootstrap == true",
      ns = ns,
      shiny$tags$div(
        class = "mt-3",
        shiny$fluidRow(
          shiny$column(
            6,
            shiny$numericInput(
              inputId = ns("boot_samples"),
              label = shiny$tags$span(
                "Bootstrap samples ",
                bslib$tooltip(
                  bsicons$bs_icon(
                    "info-circle",
                    class = "text-muted"
                  ),
                  paste(
                    "Number of bootstrap iterations.",
                    "Values over 599 rarely change",
                    "results significantly."
                  )
                )
              ),
              value = 599,
              min = 100,
              max = 10000,
              step = 100
            )
          ),
          shiny$column(
            6,
            shiny$numericInput(
              inputId = ns("boot_sample_size"),
              label = shiny$tags$span(
                "Samples per bootstrap ",
                bslib$tooltip(
                  bsicons$bs_icon(
                    "info-circle",
                    class = "text-muted"
                  ),
                  paste(
                    "Leave blank to default to the",
                    "smallest group size. Larger",
                    "values are capped automatically."
                  )
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
    shiny$tags$div(
      class = "small text-muted mt-3",
      shiny$tags$p(
        "Bootstrap methods are recommended when:",
        shiny$tags$ul(
          shiny$tags$li("Sample sizes are very small"),
          shiny$tags$li("Group sizes are unequal"),
          shiny$tags$li("Data is heavily skewed")
        )
      )
    )
  )
}
