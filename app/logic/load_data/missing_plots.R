# =============================================================================
# Missingness plot builders for the Load tab
#
# Each builder takes the corresponding result list from data_overview and
# returns a ggplot object. All of them degrade to a placeholder plot when
# there is nothing to draw, so the UI never has to special-case a clean
# data set.
# =============================================================================

box::use(
  ggplot2,
  scales,
  stats[setNames],
)

box::use(
  app/logic/plotting/plot_helpers,
)

# Shared palette: amber for missing, muted grey-blue for present.
missing_fill <- "#d98c2b"
present_fill <- "#cfd8de"

#' Common theme for the missingness plots
#' @return A ggplot2 theme object
overview_theme <- function() {
  ggplot2$theme_minimal(base_size = 13) +
    ggplot2$theme(
      panel.grid.minor = ggplot2$element_blank(),
      plot.caption = ggplot2$element_text(
        color = "gray40", hjust = 0
      ),
      plot.title.position = "plot"
    )
}

#' Ranked bar chart of missing values per affected column
#'
#' @param result Result list from `data_overview$missing_by_column()`
#' @return A ggplot2 object
#' @export
plot_missing_by_column <- function(result) {
  df <- result$data

  if (is.null(df) || nrow(df) == 0) {
    return(plot_helpers$create_empty_plot(
      paste0(
        "No missing values — all ",
        result$n_total_cols,
        " columns are complete"
      )
    ))
  }

  df$column <- factor(df$column, levels = rev(df$column))
  df$label <- paste0(df$n_missing, " (", df$pct_missing, "%)")

  caption <- paste0(
    nrow(df), " of ", result$n_total_cols,
    " columns have missing values · ",
    result$n_complete_cols, " are complete"
  )

  ggplot2$ggplot(
    df,
    ggplot2$aes(x = .data[["column"]], y = .data[["n_missing"]])
  ) +
    ggplot2$geom_col(fill = missing_fill, width = 0.7) +
    ggplot2$geom_text(
      ggplot2$aes(label = .data[["label"]]),
      hjust = -0.1, size = 3.5, color = "gray30"
    ) +
    ggplot2$coord_flip(clip = "off") +
    ggplot2$scale_y_continuous(expand = ggplot2$expansion(mult = c(0, 0.18))) +
    ggplot2$labs(x = NULL, y = "Missing values", caption = caption) +
    overview_theme()
}

#' Raster of missing cells: one row per observation, one column per
#' NA-bearing variable
#'
#' @param result Result list from `data_overview$missing_raster_data()`
#' @return A ggplot2 object
#' @export
plot_missing_raster <- function(result) {
  df <- result$data

  if (is.null(df) || nrow(df) == 0) {
    return(plot_helpers$create_empty_plot(
      "No missing values — nothing to map"
    ))
  }

  caption <- if (isTRUE(result$downsampled)) {
    paste0(
      "Showing ", result$n_rows_shown, " of ", result$n_rows_total,
      " rows (evenly sampled) · rows sorted by missingness pattern"
    )
  } else {
    paste0(
      result$n_rows_total,
      " rows, sorted by missingness pattern"
    )
  }

  ggplot2$ggplot(
    df,
    ggplot2$aes(
      x = .data[["column"]],
      y = .data[["row_id"]],
      fill = .data[["is_missing"]]
    )
  ) +
    ggplot2$geom_raster() +
    ggplot2$scale_fill_manual(
      values = c("FALSE" = present_fill, "TRUE" = missing_fill),
      labels = c("FALSE" = "Present", "TRUE" = "Missing"),
      name = NULL
    ) +
    ggplot2$scale_y_reverse(expand = c(0, 0)) +
    ggplot2$scale_x_discrete(expand = c(0, 0)) +
    ggplot2$labs(x = NULL, y = "Rows", caption = caption) +
    overview_theme() +
    ggplot2$theme(
      axis.text.x = ggplot2$element_text(angle = 45, hjust = 1),
      axis.text.y = ggplot2$element_blank(),
      panel.grid = ggplot2$element_blank(),
      legend.position = "top"
    )
}

#' Bar chart of the most frequent co-occurring missingness patterns
#'
#' @param result Result list from `data_overview$missing_patterns()`
#' @return A ggplot2 object
#' @export
plot_missing_patterns <- function(result) {
  df <- result$data

  if (is.null(df) || nrow(df) == 0) {
    return(plot_helpers$create_empty_plot(
      "No missing values — no patterns to report"
    ))
  }

  if (isTRUE(result$all_singleton)) {
    return(plot_helpers$create_empty_plot(
      paste0(
        "Missingness is scattered: each of the ", result$n_patterns,
        " patterns affects a single row.\n",
        "No columns are consistently missing together."
      )
    ))
  }

  # Long patterns are shortened for display, which can make two labels
  # identical. Plot against the unique pattern id and map the display
  # text on through the scale, so the axis can never collide.
  df$display <- ifelse(
    df$n_cols_missing > 3,
    paste0(
      sub("^((?:[^+]+\\+){2}[^+]+).*$", "\\1", df$label),
      "+ ", df$n_cols_missing - 3, " more"
    ),
    df$label
  )
  axis_labels <- setNames(df$display, df$pattern_id)

  df$pattern_id <- factor(df$pattern_id, levels = rev(df$pattern_id))
  df$bar_label <- paste0(df$n_rows, " rows (", df$pct_rows, "%)")
  df$is_complete <- df$n_cols_missing == 0

  caption <- paste0(
    result$n_patterns, " distinct patterns across ",
    result$n_na_cols, " affected columns",
    if (result$n_hidden > 0) {
      paste0(" · ", result$n_hidden, " smaller patterns not shown")
    } else {
      ""
    }
  )

  ggplot2$ggplot(
    df,
    ggplot2$aes(
      x = .data[["pattern_id"]],
      y = .data[["n_rows"]],
      fill = .data[["is_complete"]]
    )
  ) +
    ggplot2$geom_col(width = 0.7) +
    ggplot2$geom_text(
      ggplot2$aes(label = .data[["bar_label"]]),
      hjust = -0.1, size = 3.5, color = "gray30"
    ) +
    ggplot2$scale_fill_manual(
      values = c("FALSE" = missing_fill, "TRUE" = "#6aa84f"),
      guide = "none"
    ) +
    ggplot2$scale_x_discrete(labels = axis_labels) +
    ggplot2$coord_flip(clip = "off") +
    ggplot2$scale_y_continuous(
      labels = scales$comma,
      expand = ggplot2$expansion(mult = c(0, 0.22))
    ) +
    ggplot2$labs(
      x = NULL, y = "Rows sharing this pattern", caption = caption
    ) +
    overview_theme()
}
