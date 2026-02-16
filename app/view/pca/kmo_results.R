box::use(
  bsicons,
  bslib,
  shiny,
)

box::use(
  app/logic/pca/kmo,
)

#' Render KMO results in collapsible accordion panels
#'
#' Builds a bslib accordion with two panels: overall KMO (badge +
#' interpretation) and individual variable KMO (sorted table).
#'
#' @param kmo_result List with $overall (numeric) and
#'   $individual (named numeric vector)
#' @return Shiny tags object with formatted KMO display
#' @export
render_kmo_results <- function(kmo_result) {
  overall_kmo <- kmo_result$overall
  individual_kmo <- kmo_result$individual

  shiny$tagList(
    shiny$tags$div(
      class = "d-flex align-items-center gap-3 mb-3",
      shiny$tags$div(
        class = paste(
          "badge fs-5",
          kmo$kmo_badge_class(overall_kmo)
        ),
        sprintf("%.3f", overall_kmo)
      ),
      shiny$tags$span(
        class = "text-muted",
        kmo$kmo_interpretation(overall_kmo)
      )
    ),
    shiny$tags$h6(
      class = "mb-2",
      bsicons$bs_icon("list-ul", class = "me-1"),
      "Individual Variable KMO"
    ),
    render_kmo_table(individual_kmo)
  )
}

# =============================================================================
# Internal helper (not exported)
# =============================================================================

render_kmo_table <- function(individual_kmo) {
  sorted_kmo <- sort(individual_kmo, decreasing = TRUE)

  rows <- lapply(names(sorted_kmo), function(var) {
    val <- sorted_kmo[[var]]
    shiny$tags$tr(
      shiny$tags$td(var),
      shiny$tags$td(
        class = "text-end",
        shiny$tags$span(
          class = paste("badge", kmo$kmo_badge_class(val)),
          sprintf("%.3f", val)
        )
      )
    )
  })

  shiny$tags$table(
    class = "table table-sm table-striped",
    shiny$tags$thead(
      shiny$tags$tr(
        shiny$tags$th("Variable"),
        shiny$tags$th(class = "text-end", "KMO")
      )
    ),
    shiny$tags$tbody(rows)
  )
}
