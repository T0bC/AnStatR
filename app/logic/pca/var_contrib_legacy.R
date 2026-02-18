box::use(
  ggplot2,
  ggiraph,
  rhino,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Legacy variable contribution plots (bar chart + overview dot plot)
# Kept for reference / potential reuse. Not used in the app.
# =============================================================================

#' Create a variable contribution bar chart
#'
#' Horizontal bar chart showing each variable's contribution (%)
#' to a given dimension. Bars are colored by whether the
#' contribution exceeds the expected average (100/p %).
#'
#' @param pca_result PCA result list (the $result field from run_pca)
#' @param dim Character, dimension to display (e.g. "Dim.1")
#' @param show_title Logical, whether to show the plot title
#' @return List with $success, $result (ggplot) or $error
#' @export
create_var_contrib_plot <- function(pca_result,
                                    dim = "Dim.1",
                                    show_title = TRUE) {
  error_context <- list(dim = dim)

  error_handling$safe_execute(
    expr = {
      validate_var_contrib_inputs(pca_result, dim)

      plot_data <- build_var_contrib_data(pca_result, dim)
      threshold <- expected_average(pca_result)

      p <- ggplot2$ggplot(
        plot_data,
        ggplot2$aes(
          x = contrib,
          y = stats::reorder(variable, contrib)
        )
      ) +
        ggiraph$geom_col_interactive(
          ggplot2$aes(
            fill = above_avg,
            tooltip = tooltip,
            data_id = data_id
          ),
          width = 0.7
        ) +
        ggplot2$geom_vline(
          xintercept = threshold,
          linetype = "dashed",
          color = "grey40",
          linewidth = 0.5
        ) +
        ggplot2$scale_fill_manual(
          values = c(
            "Above average" = "#2166AC",
            "Below average" = "#D1E5F0"
          ),
          name = NULL
        ) +
        var_contrib_theme() +
        ggplot2$labs(
          x = "Contribution (%)",
          y = NULL
        )

      if (show_title) {
        eig <- pca_result$eig
        var_label <- axis_label_with_variance(dim, eig)
        p <- p + ggplot2$ggtitle(
          paste("Variable Contributions to", var_label)
        )
      }

      # Annotate the threshold line
      p <- p + ggplot2$annotate(
        "text",
        x = threshold,
        y = 0.5,
        label = sprintf("Expected avg = %.1f%%", threshold),
        hjust = -0.05,
        vjust = -0.5,
        size = 3,
        color = "grey40"
      )

      p
    },
    operation_name = "Variable Contribution Plot",
    context = error_context,
    error_parser = var_contrib_error_parser
  )
}


#' Create a variable contribution overview plot across dimensions
#'
#' Point plot with repelled text labels showing each variable's
#' contribution (%) across the first `display_ncp` dimensions.
#' A horizontal dashed line marks the expected average (100/p %).
#'
#' @param pca_result PCA result list (the $result field from run_pca)
#' @param display_ncp Integer, number of dimensions to show
#' @param show_title Logical, whether to show the plot title
#' @return List with $success, $result (ggplot) or $error
#' @export
create_var_contrib_overview_plot <- function(pca_result,
                                             display_ncp = 5L,
                                             show_title = TRUE) {
  error_context <- list(display_ncp = display_ncp)

  error_handling$safe_execute(
    expr = {
      if (is.null(pca_result)) stop("pca_result is NULL")

      contrib <- pca_result$var$contrib
      n_dims <- min(display_ncp, ncol(contrib))
      dims <- colnames(contrib)[seq_len(n_dims)]

      plot_data <- build_overview_data(
        contrib, dims, pca_result$eig
      )
      threshold <- expected_average(pca_result)

      # X-axis tick labels
      dim_labels <- levels(plot_data$dim_label)
      dim_breaks <- seq_along(dim_labels)

      p <- ggplot2$ggplot(
        plot_data,
        ggplot2$aes(
          x = x_pos, y = contrib
        )
      ) +
        ggiraph$geom_point_interactive(
          ggplot2$aes(
            tooltip = tooltip,
            data_id = data_id
          ),
          shape = 21,
          size = 8,
          fill = "white",
          color = "black",
          stroke = 0.8
        ) +
        ggiraph$geom_label_repel_interactive(
          ggplot2$aes(
            label = variable,
            tooltip = tooltip,
            data_id = data_id
          ),
          size = 4.2,
          fill = "white",
          color = "#333333",
          max.overlaps = 30,
          point.size = 5,
          segment.color = "grey60",
          segment.size = 0.3,
          min.segment.length = 0.2,
          box.padding = 0.35,
          point.padding = 0.3,
          force = 2,
          direction = "y",
          nudge_x = 0.4,
          label.size = 0.15,
          label.padding = ggplot2$unit(0.12, "lines"),
          seed = 42
        ) +
        ggplot2$geom_hline(
          yintercept = threshold,
          linetype = "dashed",
          color = "grey40",
          linewidth = 0.5
        ) +
        ggplot2$annotate(
          "text",
          x = Inf,
          y = threshold,
          label = sprintf("Expected avg = %.1f%%", threshold),
          hjust = 1.05,
          vjust = -0.5,
          size = 2.8,
          color = "grey40"
        ) +
        ggplot2$scale_x_continuous(
          breaks = dim_breaks,
          labels = dim_labels,
          expand = ggplot2$expansion(mult = 0.1)
        ) +
        ggplot2$geom_vline(
          xintercept = dim_breaks,
          color = "grey85",
          linewidth = 0.3
        ) +
        var_contrib_theme() +
        ggplot2$theme(
          legend.position = "none",
          panel.grid.major.y = ggplot2$element_line(),
          panel.grid.major.x = ggplot2$element_blank()
        ) +
        ggplot2$labs(
          x = NULL,
          y = "Contribution (%)"
        )

      if (show_title) {
        p <- p + ggplot2$ggtitle(
          "Variable Contributions Across Dimensions"
        )
      }

      p
    },
    operation_name = "Variable Contribution Overview Plot",
    context = error_context,
    error_parser = var_contrib_error_parser
  )
}


# =============================================================================
# Internal helpers
# =============================================================================

#' Error parser for variable contribution plot errors
var_contrib_error_parser <- function(error_msg,
                                     operation_name =
                                       "Variable Contribution Plot") {
  if (grepl(
    "dimension|dim|not found",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Invalid dimension selection.",
      " Please check available components."
    )
  } else if (grepl(
    "NULL|missing|pca_result",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": No PCA result available.",
      " Please compute PCA first."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}

#' Validate inputs for variable contribution plot
validate_var_contrib_inputs <- function(pca_result, dim) {
  if (is.null(pca_result)) {
    stop("pca_result is NULL")
  }

  available_dims <- colnames(pca_result$var$contrib)
  if (!dim %in% available_dims) {
    stop(paste("Dimension not found:", dim))
  }
}

#' Build plot data for variable contribution chart
build_var_contrib_data <- function(pca_result, dim) {
  contrib <- pca_result$var$contrib
  vals <- contrib[, dim]
  threshold <- expected_average(pca_result)

  df <- data.frame(
    variable = rownames(contrib),
    contrib = vals,
    above_avg = ifelse(
      vals >= threshold,
      "Above average",
      "Below average"
    ),
    stringsAsFactors = FALSE
  )

  df$tooltip <- sprintf(
    "<b>%s</b><br/>Contribution to %s: %.2f%%<br/>%s",
    df$variable, dim, df$contrib,
    ifelse(
      df$above_avg == "Above average",
      "Above expected average",
      "Below expected average"
    )
  )
  df$data_id <- paste0("vc_", seq_len(nrow(df)))

  df
}

#' Build long-format data for the overview plot
build_overview_data <- function(contrib, dims, eig) {
  rows <- lapply(seq_along(dims), function(i) {
    dim <- dims[i]
    vals <- contrib[, dim]
    dim_idx <- which(rownames(eig) == dim)
    var_pct <- if (length(dim_idx) == 1) {
      eig[dim_idx, "variance.percent"]
    } else {
      NA_real_
    }
    label <- if (!is.na(var_pct)) {
      sprintf("%s (%.1f%%)", dim, var_pct)
    } else {
      dim
    }
    data.frame(
      variable = rownames(contrib),
      dim = dim,
      dim_idx = i,
      dim_label = label,
      contrib = vals,
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)

  df$tooltip <- sprintf(
    "<b>%s</b><br/>%s<br/>Contribution: %.2f%%",
    df$variable, df$dim_label, df$contrib
  )
  df$data_id <- paste0(
    "vco_", df$variable, "_", df$dim
  )

  # Preserve dimension order
  df$dim_label <- factor(
    df$dim_label,
    levels = unique(df$dim_label)
  )

  # Compute x offsets to dodge overlapping points
  df$x_pos <- dodge_x_positions(df)

  df
}

#' Dodge x positions for points with similar y values
dodge_x_positions <- function(df, threshold_pct = 5,
                              max_offset = 0.3) {
  x_pos <- numeric(nrow(df))

  for (dim_i in unique(df$dim_idx)) {
    idx <- which(df$dim_idx == dim_i)
    n <- length(idx)
    if (n <= 1) {
      x_pos[idx] <- dim_i
      next
    }

    # Sort by contrib within this dimension
    ord <- order(df$contrib[idx])
    sorted_idx <- idx[ord]
    sorted_vals <- df$contrib[sorted_idx]

    # Group consecutive sorted points within threshold
    groups <- list()
    current_group <- sorted_idx[1]
    for (k in seq_along(sorted_idx)[-1]) {
      if (sorted_vals[k] - sorted_vals[k - 1] <=
          threshold_pct) {
        current_group <- c(current_group, sorted_idx[k])
      } else {
        groups <- c(groups, list(current_group))
        current_group <- sorted_idx[k]
      }
    }
    groups <- c(groups, list(current_group))

    # Assign offsets per group
    for (grp in groups) {
      g_n <- length(grp)
      if (g_n == 1) {
        x_pos[grp] <- dim_i
      } else {
        offsets <- seq(
          -max_offset, max_offset,
          length.out = g_n
        )
        x_pos[grp] <- dim_i + offsets
      }
    }
  }

  x_pos
}

#' Compute expected average contribution (100/p %)
expected_average <- function(pca_result) {
  p <- nrow(pca_result$var$contrib)
  100 / p
}

#' Theme for variable contribution chart
var_contrib_theme <- function() {
  ggplot2$theme_minimal() +
    ggplot2$theme(
      plot.title = ggplot2$element_text(
        hjust = 0.5, size = 14, face = "bold"
      ),
      axis.title = ggplot2$element_text(size = 12),
      axis.text = ggplot2$element_text(size = 10),
      legend.position = "bottom",
      legend.text = ggplot2$element_text(size = 10),
      panel.grid.major.y = ggplot2$element_blank(),
      panel.grid.minor = ggplot2$element_blank()
    )
}

#' Build axis label with variance percentage
axis_label_with_variance <- function(dim_name, eig) {
  dim_idx <- which(rownames(eig) == dim_name)
  if (length(dim_idx) == 1) {
    var_pct <- eig[dim_idx, "variance.percent"]
    sprintf("%s (%.1f%%)", dim_name, var_pct)
  } else {
    dim_name
  }
}
