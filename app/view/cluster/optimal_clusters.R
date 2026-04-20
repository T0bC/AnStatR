box::use(
  bsicons,
  ggiraph,
  shiny,
)

box::use(
  app/logic/cluster/optimal_clusters[
    create_optimal_clusters_ggplot,
  ],
  app/logic/shared/error_handling,
  app/view/shared/error_display,
)

#' Render optimal clusters panel content
#'
#' Handles NULL, error, and success cases for the optimal
#' clusters accordion panel. Includes interactive plot and
#' methods summary table.
#'
#' @param optimal_result Result from compute_optimal_clusters()
#'   (the $result field, not the wrapper)
#' @param ns Namespace function for output IDs
#' @return Shiny tags object with formatted display
#' @export
render_optimal_clusters <- function(optimal_result, ns) {
  if (is.null(optimal_result)) {
    return(shiny$tags$div(
      class = "text-muted p-3",
      "Optimal cluster estimation not available."
    ))
  }

  shiny$tagList(
    render_optimal_summary(optimal_result),
    shiny$tags$div(
      class = "mt-3",
      ggiraph$girafeOutput(
        ns("optimal_clusters_plot"), height = "600px"
      )
    ),
    shiny$tags$div(
      class = "mt-3",
      render_methods_table(optimal_result$methods)
    )
  )
}

#' Render optimal clusters plot as interactive girafe
#'
#' @param optimal_result Result list with methods and plot_data
#' @return girafe object
#' @export
render_optimal_girafe <- function(optimal_result) {
  p <- create_optimal_clusters_ggplot(optimal_result)

  ggiraph$girafe(
    ggobj = p,
    width_svg = 8,
    height_svg = 8,
    options = list(
      ggiraph$opts_hover(
        css = "fill:#0d6efd;stroke:#0d6efd;"
      ),
      ggiraph$opts_tooltip(
        css = paste(
          "background-color:#212529;",
          "color:white;padding:8px;",
          "border-radius:4px;font-size:12px;"
        ),
        opacity = 0.9
      ),
      ggiraph$opts_selection(type = "none")
    )
  )
}

# =============================================================================
# Internal helpers (not exported)
# =============================================================================

render_optimal_summary <- function(optimal_result) {
  summary_data <- optimal_result$summary

  if (
    is.null(summary_data) ||
    summary_data$methods_computed == 0
  ) {
    return(shiny$tags$div(
      class = "alert alert-warning",
      bsicons$bs_icon(
        "exclamation-triangle-fill", class = "me-2"
      ),
      "Could not compute optimal cluster estimates."
    ))
  }

  rec_text <- if (
    summary_data$min_k == summary_data$max_k
  ) {
    sprintf(
      "All methods suggest %d cluster(s).",
      summary_data$min_k
    )
  } else {
    sprintf(
      paste(
        "Methods suggest between %d and %d",
        "clusters (median: %d)."
      ),
      summary_data$min_k,
      summary_data$max_k,
      summary_data$median_k
    )
  }

  shiny$tags$div(
    class = "alert alert-info d-flex align-items-center",
    bsicons$bs_icon(
      "lightbulb-fill",
      class = "me-2 flex-shrink-0"
    ),
    shiny$tags$div(
      shiny$tags$strong("Recommendation: "),
      rec_text,
      shiny$tags$small(
        class = "d-block text-muted mt-1",
        sprintf(
          "Based on %d estimation methods.",
          summary_data$methods_computed
        )
      )
    )
  )
}

render_methods_table <- function(methods) {
  rows <- lapply(names(methods), function(method_name) {
    m <- methods[[method_name]]

    if (!is.null(m$error)) {
      k_display <- shiny$tags$span(
        class = "text-muted",
        title = m$error,
        "N/A"
      )
    } else {
      k_display <- shiny$tags$span(
        class = "badge bg-primary",
        m$optimal_k
      )
    }

    shiny$tags$tr(
      shiny$tags$td(m$name),
      shiny$tags$td(
        class = "text-center", k_display
      ),
      shiny$tags$td(
        class = "text-muted small",
        m$description
      )
    )
  })

  shiny$tags$div(
    class = "table-responsive",
    shiny$tags$table(
      class = "table table-sm table-hover",
      shiny$tags$thead(
        shiny$tags$tr(
          shiny$tags$th("Method"),
          shiny$tags$th(
            class = "text-center", "Optimal k"
          ),
          shiny$tags$th("Description")
        )
      ),
      shiny$tags$tbody(rows)
    )
  )
}
