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
    icon = "gear",
    tooltip_text = "Actions & Options",
    value = "actions_tab",
    shiny$h6(
      class = "text-muted mb-3",
      "Actions & Options"
    ),
    # Help button
    shiny$actionButton(
      inputId = ns("helpButton"),
      label = shiny$tags$span(
        bsicons$bs_icon("question-circle"),
        " Help"
      ),
      class = "btn-outline-primary btn-sm w-100 mb-3"
    ),
    shiny$tags$hr(),
    # Show additional output checkbox
    shiny$checkboxInput(
      inputId = ns("show_additional_pca_output"),
      label = shiny$tags$span(
        "Show Additional Output ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Display additional PCA statistics",
            "and diagnostic plots."
          )
        )
      ),
      value = TRUE
    )
  )
}
