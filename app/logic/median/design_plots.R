# =============================================================================
# Design balance / group missingness plot builders for the Median tab
#
# Both builders degrade gracefully as the number of grouping columns
# grows: labels are dropped once they would be unreadable, and the
# caption always carries the numbers that matter.
# =============================================================================

box::use(
  ggplot2,
  scales,
)

box::use(
  app/logic/plotting/plot_helpers,
)

thin_fill <- "#d98c2b"
ok_fill <- "#4c7a9c"
missing_high <- "#b3521f"
missing_low <- "#eef2f5"

#' Common theme for the design plots
#' @return A ggplot2 theme object
design_theme <- function() {
  ggplot2$theme_minimal(base_size = 12) +
    ggplot2$theme(
      panel.grid.minor = ggplot2$element_blank(),
      plot.caption = ggplot2$element_text(color = "gray40", hjust = 0),
      plot.title.position = "plot"
    )
}

#' Bar chart of observations per grouping-column combination
#'
#' The x axis uses a legendry nested axis so the grouping hierarchy is
#' visible as brackets underneath the bars. Above `max_labelled`
#' combinations the tick labels are dropped: the shape of the
#' distribution and the caption still answer "is my design balanced?",
#' which unreadable labels would not.
#'
#' @param balance Result list from `design_balance$compute_group_balance()`
#' @param max_labelled Maximum combinations to label on the x axis
#' @return A ggplot2 object
#' @export
plot_group_balance <- function(balance, max_labelled = 80) {
  df <- balance$data

  if (is.null(df) || nrow(df) == 0) {
    return(plot_helpers$create_empty_plot(
      "Select grouping columns to see the design balance"
    ))
  }

  n_combinations <- balance$n_combinations
  labelled <- n_combinations <= max_labelled

  caption <- paste0(
    format(n_combinations, big.mark = ","),
    " observed combinations of ",
    format(balance$n_possible, big.mark = ","),
    " possible (", balance$pct_coverage, "% coverage) · ",
    balance$n_thin, " thin (n < ", balance$thin_threshold, ")",
    if (!labelled) " · labels hidden: too many combinations" else ""
  )

  p <- ggplot2$ggplot(
    df,
    ggplot2$aes(
      x = .data[["group_label"]],
      y = .data[["n"]],
      fill = .data[["is_thin"]]
    )
  ) +
    ggplot2$geom_col(width = 0.8) +
    ggplot2$scale_fill_manual(
      values = c("FALSE" = ok_fill, "TRUE" = thin_fill),
      labels = c(
        "FALSE" = "Adequate",
        "TRUE" = paste0("Thin (n < ", balance$thin_threshold, ")")
      ),
      name = NULL,
      drop = FALSE
    ) +
    ggplot2$scale_y_continuous(
      labels = scales$comma,
      expand = ggplot2$expansion(mult = c(0, 0.08))
    ) +
    ggplot2$labs(
      x = paste(balance$grouping_cols, collapse = " | "),
      y = "Observations",
      caption = caption
    ) +
    design_theme() +
    ggplot2$theme(legend.position = "top")

  if (!labelled) {
    return(
      p + ggplot2$theme(
        axis.text.x = ggplot2$element_blank(),
        axis.ticks.x = ggplot2$element_blank()
      )
    )
  }

  if (length(balance$grouping_cols) > 1) {
    p <- plot_helpers$add_nested_axis(p)
  } else {
    p <- p + ggplot2$theme(
      axis.text.x = ggplot2$element_text(angle = 45, hjust = 1)
    )
  }

  p
}

#' Heatmap of missing measurement values per group
#'
#' @param missingness Result list from
#'   `design_balance$compute_group_missingness()`
#' @param max_labelled Maximum groups to label on the y axis
#' @return A ggplot2 object
#' @export
plot_group_missingness <- function(missingness, max_labelled = 60) {
  df <- missingness$data

  if (is.null(df) || nrow(df) == 0) {
    return(plot_helpers$create_empty_plot(
      paste(
        "No missing values in the measurement columns",
        "for this grouping"
      )
    ))
  }

  labelled <- missingness$n_groups <= max_labelled

  caption <- paste0(
    missingness$n_cols,
    " measurement columns with gaps × ",
    format(missingness$n_groups, big.mark = ","), " groups",
    if (!labelled) " · group labels hidden: too many groups" else ""
  )

  p <- ggplot2$ggplot(
    df,
    ggplot2$aes(
      x = .data[["column"]],
      y = .data[["group_label"]],
      fill = .data[["pct_missing"]]
    )
  ) +
    ggplot2$geom_tile(color = "white", linewidth = 0.3) +
    ggplot2$scale_fill_gradient(
      low = missing_low,
      high = missing_high,
      limits = c(0, 100),
      labels = function(x) paste0(x, "%"),
      name = "Missing"
    ) +
    ggplot2$scale_x_discrete(expand = c(0, 0)) +
    ggplot2$scale_y_discrete(expand = c(0, 0), limits = rev) +
    ggplot2$labs(
      x = NULL,
      y = paste(missingness$grouping_cols, collapse = " | "),
      caption = caption
    ) +
    design_theme() +
    ggplot2$theme(
      axis.text.x = ggplot2$element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2$element_blank()
    )

  if (!labelled) {
    p <- p + ggplot2$theme(
      axis.text.y = ggplot2$element_blank(),
      axis.ticks.y = ggplot2$element_blank()
    )
  }

  p
}
