box::use(
  shiny,
)

box::use(
  app/logic/cluster/hopkins[compute_hopkins],
  app/logic/shared/error_handling,
  app/view/shared/error_display,
)

#' Render Hopkins statistic output panel
#'
#' Renders the Hopkins clusterability assessment as a UI card
#' with the H value, interpretation badge, details, and
#' contextual warnings. Called by the parent cluster module
#' using dependency injection.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param hopkins_result Reactive returning the Hopkins
#'   computation result (from compute_hopkins)
#' @export
render_output <- function(input, output, session,
                          hopkins_result) {
  ns <- session$ns

  output$hopkins_panel <- shiny$renderUI({
    result <- hopkins_result()
    if (is.null(result)) return(NULL)

    if (!result$success) {
      return(
        error_display$error_alert_structured(
          result$error, type = "danger"
        )
      )
    }

    res <- result$result
    render_hopkins_card(res)
  })
}

# =============================================================================
# Internal helpers (not exported)
# =============================================================================

render_hopkins_card <- function(res) {
  h_value <- res$H
  interp <- res$interpretation
  warnings <- res$warnings

  badge_class <- switch(
    interp$level,
    success = "bg-success",
    warning = "bg-warning text-dark",
    danger  = "bg-danger",
    "bg-secondary"
  )

  # Build warning alerts
  warning_ui <- NULL
  if (length(warnings) > 0) {
    warning_ui <- shiny$tags$div(
      class = "mt-3",
      lapply(warnings, function(w) {
        shiny$tags$div(
          class = paste0(
            "alert alert-info alert-dismissible ",
            "py-2 px-3 small mb-2"
          ),
          role = "alert",
          shiny$tags$i(
            class = "bi bi-info-circle me-1"
          ),
          w
        )
      })
    )
  }

  shiny$tags$div(
    class = "p-3",
    # H value and interpretation
    shiny$tags$div(
      class = "text-center mb-3",
      shiny$tags$h3(
        class = "mb-1",
        sprintf("H = %.4f", h_value)
      ),
      shiny$tags$span(
        class = paste("badge fs-6", badge_class),
        interp$label
      ),
      shiny$tags$p(
        class = "text-muted mt-2 mb-0",
        interp$description
      )
    ),
    shiny$tags$hr(),
    # Details
    shiny$tags$div(
      class = "row text-center",
      shiny$tags$div(
        class = "col-4",
        shiny$tags$small(
          class = "text-muted d-block", "Samples (n)"
        ),
        shiny$tags$strong(res$n)
      ),
      shiny$tags$div(
        class = "col-4",
        shiny$tags$small(
          class = "text-muted d-block", "Sampled (m)"
        ),
        shiny$tags$strong(res$m)
      ),
      shiny$tags$div(
        class = "col-4",
        shiny$tags$small(
          class = "text-muted d-block", "Dimensions"
        ),
        shiny$tags$strong(res$n_dims)
      )
    ),
    # Warnings
    warning_ui
  )
}
