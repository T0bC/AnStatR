box::use(
  bsicons,
  shiny,
)

# =============================================================================
# Shared "apply recommended parameters" banner, used by PCA and LDA
# data-selection tabs to surface the parameter set recommended by the
# Statistics tab's screening-mode ranking.
# =============================================================================

#' Render a recommendation banner with an "Apply" button
#'
#' @param recommended Character vector of recommended measurement column
#'   names
#' @param ns Namespace function from the parent module
#' @param button_id Character, the (unnamespaced) input id for the apply
#'   action button
#' @return Shiny tags object
#' @export
render_recommendation_banner <- function(recommended, ns, button_id) {
  shiny$tags$div(
    class = "alert alert-info py-2 px-2 small mb-2",
    bsicons$bs_icon("lightbulb", class = "me-1"),
    shiny$tags$strong("Recommendation: "),
    paste0(
      length(recommended),
      " parameter(s) were identified in the Statistics tab as good ",
      "group separators."
    ),
    shiny$tags$div(
      class = "mt-1",
      shiny$actionButton(
        inputId = ns(button_id),
        label = "Apply recommended parameters",
        class = "btn-outline-primary btn-sm"
      )
    )
  )
}
