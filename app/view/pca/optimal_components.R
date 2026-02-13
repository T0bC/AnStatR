box::use(
  bsicons,
  bslib,
  ggiraph,
  ggplot2,
  shiny,
)

box::use(
  app/logic/error_handling,
  app/view/error_display,
)

#' Render optimal components panel content
#'
#' Handles NULL, error, and success cases for the optimal
#' components accordion panel.
#'
#' @param optimal_result Result from calculate_optimal_components()
#'   (the $result field, not the wrapper)
#' @param ns Namespace function for output IDs
#' @return Shiny tags object with formatted display
#' @export
render_optimal_components <- function(optimal_result, ns) {
  if (is.null(optimal_result)) {
    return(shiny$tags$div(
      class = "text-muted p-3",
      "Optimal component estimation not available."
    ))
  }

  shiny$tagList(
    render_optimal_summary(optimal_result),
    shiny$tags$div(
      class = "mt-3",
      ggiraph$girafeOutput(
        ns("optimal_scree_plot"), height = "400px"
      )
    ),
    shiny$tags$div(
      class = "mt-3",
      render_methods_table(optimal_result$methods)
    )
  )
}

#' Render scree plot as interactive girafe
#'
#' @param optimal_result Result list with eigenvalues and methods
#' @return girafe object
#' @export
render_scree_girafe <- function(optimal_result) {
  p <- create_scree_plot(optimal_result)

  ggiraph$girafe(
    ggobj = p,
    width_svg = 8,
    height_svg = 5,
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
      "Could not compute optimal component estimates."
    ))
  }

  rec_text <- if (
    summary_data$min_ncp == summary_data$max_ncp
  ) {
    sprintf(
      "All methods suggest %d component(s).",
      summary_data$min_ncp
    )
  } else {
    sprintf(
      paste(
        "Methods suggest between %d and %d",
        "components (median: %d)."
      ),
      summary_data$min_ncp,
      summary_data$max_ncp,
      summary_data$median_ncp
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
      ncp_display <- shiny$tags$span(
        class = "text-muted",
        title = m$error,
        "N/A"
      )
    } else {
      ncp_display <- shiny$tags$span(
        class = "badge bg-primary",
        m$ncp
      )
    }

    shiny$tags$tr(
      shiny$tags$td(m$name),
      shiny$tags$td(
        class = "text-center", ncp_display
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
            class = "text-center", "Components"
          ),
          shiny$tags$th("Description")
        )
      ),
      shiny$tags$tbody(rows)
    )
  )
}


create_scree_plot <- function(optimal_result) {
  eigenvalues <- optimal_result$eigenvalues
  n_comp <- length(eigenvalues)
  methods <- optimal_result$methods

  df <- data.frame(
    Component = seq_len(n_comp),
    Eigenvalue = eigenvalues,
    Variance = eigenvalues / sum(eigenvalues) * 100,
    Cumulative = cumsum(eigenvalues) /
      sum(eigenvalues) * 100
  )

  # Base plot: bars + line + points
  p <- ggplot2$ggplot(
    df, ggplot2$aes(x = Component, y = Eigenvalue)
  ) +
    ggiraph$geom_col_interactive(
      ggplot2$aes(
        tooltip = sprintf(
          paste(
            "Component %d",
            "Eigenvalue: %.3f",
            "Variance: %.1f%%",
            "Cumulative: %.1f%%",
            sep = "\n"
          ),
          Component, Eigenvalue, Variance, Cumulative
        ),
        data_id = Component
      ),
      fill = "#6c757d",
      alpha = 0.7
    ) +
    ggplot2$geom_line(
      color = "#212529", linewidth = 1
    ) +
    ggiraph$geom_point_interactive(
      ggplot2$aes(
        tooltip = sprintf("%.3f", Eigenvalue),
        data_id = paste0("point_", Component)
      ),
      size = 3,
      color = "#212529"
    )

  # Kaiser criterion line (eigenvalue = 1)
  if (!is.null(methods$kaiser)) {
    p <- p +
      ggplot2$geom_hline(
        yintercept = 1,
        linetype = "dashed",
        color = "#0d6efd",
        linewidth = 0.8
      ) +
      ggplot2$annotate(
        "text",
        x = n_comp * 0.95,
        y = 1.1,
        label = "Kaiser (\u03bb=1)",
        hjust = 1,
        size = 3,
        color = "#0d6efd"
      )
  }

  # Parallel analysis line
  if (
    !is.null(methods$parallel) &&
    !is.null(methods$parallel$random_eigenvalues)
  ) {
    random_eigs <- methods$parallel$random_eigenvalues
    pa_df <- data.frame(
      Component = seq_along(random_eigs),
      RandomEig = random_eigs
    )
    label_idx <- min(3, length(random_eigs))
    p <- p +
      ggplot2$geom_line(
        data = pa_df,
        ggplot2$aes(x = Component, y = RandomEig),
        linetype = "dashed",
        color = "#dc3545",
        linewidth = 0.8
      ) +
      ggplot2$annotate(
        "text",
        x = label_idx,
        y = random_eigs[label_idx] +
          max(eigenvalues) * 0.05,
        label = "Parallel Analysis (95th pctl)",
        hjust = 0,
        size = 3,
        color = "#dc3545"
      )
  }

  # Elbow vertical marker
  if (
    !is.null(methods$elbow) &&
    !is.null(methods$elbow$ncp) &&
    !is.na(methods$elbow$ncp)
  ) {
    p <- p +
      ggplot2$geom_vline(
        xintercept = methods$elbow$ncp + 0.5,
        linetype = "dotdash",
        color = "#fd7e14",
        linewidth = 0.6,
        alpha = 0.7
      ) +
      ggplot2$annotate(
        "text",
        x = methods$elbow$ncp + 0.6,
        y = max(eigenvalues) * 0.95,
        label = paste(
          "Elbow (n =",
          methods$elbow$ncp, ")"
        ),
        hjust = 0,
        size = 3,
        color = "#fd7e14"
      )
  }

  # Theme and labels
  p <- p +
    ggplot2$scale_x_continuous(
      breaks = seq_len(n_comp)
    ) +
    ggplot2$labs(
      title = paste(
        "Scree Plot with Optimal",
        "Component Thresholds"
      ),
      x = "Principal Component",
      y = "Eigenvalue"
    ) +
    ggplot2$theme_minimal() +
    ggplot2$theme(
      plot.title = ggplot2$element_text(
        size = 12, face = "bold"
      ),
      panel.grid.minor = ggplot2$element_blank()
    )

  p
}
