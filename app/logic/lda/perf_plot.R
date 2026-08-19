box::use(
  ggplot2,
  ggiraph,
  rhino,
)

box::use(
  app/logic/shared/error_handling,
)

# =============================================================================
# Component error-rate curve for PLS-DA/sPLS-DA perf() diagnostics.
# The classification-error analogue of the PCA scree plot: read the elbow to
# choose how many components to keep.
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Suggest a component count from a cross-validated error curve
#'
#' Picks the component count with the lowest BER, then applies a
#' one-standard-error-style parsimony rule: prefer the smallest
#' component count whose BER is within `tol` of that minimum, so a
#' negligible improvement does not justify an extra component.
#'
#' @param ber Numeric vector of balanced error rates, one per component
#' @param tol Numeric, absolute BER tolerance (default 0.01 = 1 pp)
#' @return Integer component count, or NA when ber is unusable
#' @export
suggest_ncomp <- function(ber, tol = 0.01) {
  if (is.null(ber) || length(ber) == 0 || all(is.na(ber))) {
    return(NA_integer_)
  }
  best <- min(ber, na.rm = TRUE)
  within <- which(!is.na(ber) & ber <= best + tol)
  if (length(within) == 0) return(NA_integer_)
  as.integer(min(within))
}


#' Build the per-component error-rate curve
#'
#' @param errors_df Data frame from run_plsda_perf()$errors with
#'   columns Component, "Overall Error", BER
#' @return List with $success, $result (girafe object) or $error
#' @export
create_perf_error_plot <- function(errors_df) {
  error_handling$safe_execute(
    {
      if (is.null(errors_df) || nrow(errors_df) == 0) {
        stop("No component diagnostics available to plot.")
      }
      required <- c("Component", "Overall Error", "BER")
      missing_cols <- setdiff(required, names(errors_df))
      if (length(missing_cols) > 0) {
        stop(
          "Component diagnostics missing column(s): ",
          paste(missing_cols, collapse = ", "), "."
        )
      }

      n_comp <- nrow(errors_df)
      # Long form so both metrics share one legend and colour scale.
      plot_df <- data.frame(
        comp_index = rep(seq_len(n_comp), 2),
        Component = rep(errors_df$Component, 2),
        Metric = rep(
          c("Overall Error", "Balanced Error Rate (BER)"),
          each = n_comp
        ),
        Error = c(errors_df[["Overall Error"]], errors_df$BER),
        stringsAsFactors = FALSE
      )

      suggested <- suggest_ncomp(errors_df$BER)

      p <- ggplot2$ggplot(
        plot_df,
        ggplot2$aes(
          x = comp_index,
          y = Error,
          colour = Metric
        )
      )

      # Recommended count first so it sits behind the data.
      if (!is.na(suggested)) {
        p <- p +
          ggplot2$geom_vline(
            xintercept = suggested,
            linetype = "dashed",
            colour = "#198754",
            linewidth = 0.6
          )
      }

      p <- p +
        # group by metric so a single-component fit does not warn
        ggplot2$geom_line(
          ggplot2$aes(group = Metric), linewidth = 0.7
        ) +
        ggiraph$geom_point_interactive(
          ggplot2$aes(
            tooltip = sprintf(
              "%s\n%s: %.1f%%",
              Component, Metric, Error * 100
            ),
            data_id = paste0(Metric, "_", Component)
          ),
          size = 2.5
        ) +
        ggplot2$scale_x_continuous(
          breaks = seq_len(n_comp),
          labels = errors_df$Component
        ) +
        ggplot2$scale_y_continuous(
          labels = function(x) paste0(round(x * 100), "%")
        ) +
        ggplot2$scale_colour_manual(
          values = c(
            "Overall Error" = "#6c757d",
            "Balanced Error Rate (BER)" = "#0d6efd"
          )
        ) +
        ggplot2$labs(
          title = "Cross-validated classification error per component",
          subtitle = if (!is.na(suggested)) {
            paste0(
              "Lowest BER within tolerance at ", suggested,
              " component", if (suggested == 1) "" else "s",
              " (dashed line). Error rising after a point indicates",
              " components adding noise rather than signal."
            )
          } else {
            "Read the elbow: the point after which error stops falling."
          },
          x = "Components retained",
          y = "Cross-validated error",
          colour = NULL
        ) +
        ggplot2$theme_minimal() +
        ggplot2$theme(
          plot.title = ggplot2$element_text(size = 12, face = "bold"),
          plot.subtitle = ggplot2$element_text(
            size = 9, colour = "#6c757d"
          ),
          legend.position = "bottom",
          panel.grid.minor = ggplot2$element_blank()
        )

      rhino$log$info(
        "perf_plot: built error curve for {n_comp} components"
      )

      ggiraph$girafe(
        ggobj = p,
        width_svg = 8,
        height_svg = 5,
        options = list(
          ggiraph$opts_sizing(rescale = TRUE, width = 1),
          ggiraph$opts_hover(
            css = "stroke-width:3px;"
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
    },
    operation_name = "Component Error Curve"
  )
}
