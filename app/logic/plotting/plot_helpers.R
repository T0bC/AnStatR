box::use(
  ggplot2,
  legendry,
  rhino,
)

box::use(
  app/logic/shared/data_utils,
  app/logic/plotting/data_processing,
)

# =============================================================================
# Validation
# =============================================================================

#' Validate required plot inputs; returns empty plot or NULL if valid
#' @param data Data frame to validate
#' @param x_cols Character vector of X-axis column names
#' @param y_col Character string of Y-axis column name
#' @return Empty plot with message if invalid, NULL if valid
#' @export
validate_plot_inputs <- function(data, x_cols, y_col) {
  if (is.null(data) || nrow(data) == 0) {
    rhino$log$info("Plot: skipped, no data available")
    return(create_empty_plot("No data available"))
  }
  if (is.null(x_cols) || length(x_cols) == 0) {
    rhino$log$info("Plot: skipped, no X-axis selected")
    return(create_empty_plot("No X-axis column selected"))
  }
  missing_x <- x_cols[!x_cols %in% names(data)]
  if (length(missing_x) > 0) {
    rhino$log$warn(
      "Plot: X-axis column(s) not found: ",
      "{paste(missing_x, collapse = ', ')}"
    )
    return(create_empty_plot(paste(
      "X-axis column(s) not found:",
      paste(missing_x, collapse = ", ")
    )))
  }
  if (is.null(y_col) || !y_col %in% names(data)) {
    rhino$log$warn("Plot: Y column '{y_col}' not found")
    return(create_empty_plot(paste("Column", y_col, "not found")))
  }
  NULL
}

#' Create an empty placeholder plot with a message
#' @param message Character string to display
#' @return A ggplot2 object
#' @export
create_empty_plot <- function(message = "No data to display") {
  ggplot2$ggplot() +
    ggplot2$annotate(
      "text", x = 0.5, y = 0.5,
      label = message, size = 4, color = "gray50"
    ) +
    ggplot2$theme_void() +
    ggplot2$xlim(0, 1) +
    ggplot2$ylim(0, 1)
}


# =============================================================================
# Default resolvers
# =============================================================================

#' Resolve point style parameters with defaults
#' @param ps List of point style parameters
#' @return List with resolved values
#' @export
resolve_point_style <- function(ps) {
  list(
    size       = ps$size       %||% 4,
    spread     = ps$spread     %||% 0.15,
    alpha      = ps$alpha      %||% 0.6,
    shape_cols = ps$shape_cols
  )
}

#' Resolve processing parameters with defaults
#' @param proc List of processing parameters
#' @return List with resolved values
#' @export
resolve_processing <- function(proc) {
  list(
    trim_percent      = proc$trim_percent      %||% 0,
    outlier_enabled   = proc$outlier_enabled    %||% FALSE,
    outlier_method    = proc$outlier_method     %||% "IQR",
    outlier_factor    = proc$outlier_factor     %||% 1.5,
    bootstrap_samples = proc$bootstrap_samples  %||% 1000
  )
}

#' Resolve grid and legend parameters with defaults
#' @param gl List of grid/legend parameters
#' @return List with resolved values
#' @export
resolve_grid_legend <- function(gl) {
  list(
    legend_position    = gl$legend_position    %||% "none",
    h_grid             = gl$h_grid             %||% TRUE,
    v_grid             = gl$v_grid             %||% TRUE,
    top_right_borders  = gl$top_right_borders  %||% TRUE,
    show_median        = gl$show_median        %||% TRUE,
    show_sd            = gl$show_sd            %||% TRUE,
    aspect_ratio       = gl$aspect_ratio       %||% FALSE
  )
}

#' Resolve stat line style parameters with defaults
#' @param sls List of stat line style parameters
#' @return List with resolved values
#' @export
resolve_stat_line_style <- function(sls) {
  list(
    median_thickness = sls$median_thickness %||% 0.5,
    median_width     = sls$median_width     %||% 0.15,
    sd_thickness     = sls$sd_thickness     %||% 0.5,
    sd_width         = sls$sd_width         %||% 0.15
  )
}

#' Resolve axis style parameters with defaults
#' @param ax List of axis style parameters
#' @return List with resolved values
#' @export
resolve_axis_style <- function(ax) {
  list(
    tick_length    = ax$tick_length    %||% 0.15,
    line_thickness = ax$line_thickness %||% 0.5
  )
}

#' Resolve boxplot-specific parameters with defaults
#' @param bp List of boxplot parameters
#' @return List with resolved values
#' @export
resolve_boxplot_style <- function(bp) {
  list(
    box_width     = bp$box_width     %||% 0.7,
    show_outliers = bp$show_outliers %||% FALSE,
    notch         = bp$notch         %||% FALSE
  )
}

#' Resolve violin-specific parameters with defaults
#' @param vp List of violin parameters
#' @return List with resolved values
#' @export
resolve_violin_style <- function(vp) {
  list(
    violin_width = vp$violin_width %||% 0.9,
    trim         = vp$trim         %||% TRUE,
    scale        = vp$scale        %||% "width"
  )
}


# =============================================================================
# Data preparation
# =============================================================================

#' Prepare x-axis: single column or nested interaction
#' @param data Data frame
#' @param x_cols Character vector of X-axis column names
#' @param factor_order Optional named list of custom factor level orderings
#' @return List with data, x_var, and x_label
#' @export
prepare_x_axis <- function(data, x_cols, factor_order = NULL) {
  if (length(x_cols) > 1) {
    # Reverse for legendry: first selected = outer grouping
    x_nested <- data_utils$create_interaction(
      data, base::rev(x_cols), factor_order
    )
    data$.x_nested <- x_nested
    list(
      data = data,
      x_var = ".x_nested",
      x_label = paste(x_cols, collapse = " | ")
    )
  } else {
    # Single column: apply factor ordering if provided
    col <- x_cols[1]
    if (!is.null(factor_order) && col %in% names(factor_order)) {
      values <- data[[col]]
      values[is.na(values)] <- "NA"
      custom_levels <- factor_order[[col]]
      data_levels <- unique(as.character(values))
      all_levels <- c(custom_levels, setdiff(data_levels, custom_levels))
      data[[col]] <- factor(values, levels = all_levels)
    }
    list(
      data = data,
      x_var = x_cols,
      x_label = x_cols
    )
  }
}

#' Prepare color grouping column
#' @param data Data frame
#' @param color_cols Character vector of color column names
#' @param x_cols Character vector of X-axis column names (fallback)
#' @param factor_order Optional named list of custom factor level orderings
#' @return List with data and color_legend_title
#' @export
prepare_color_group <- function(data, color_cols, x_cols, factor_order = NULL) {
  if (is.null(color_cols) || length(color_cols) == 0) {
    color_cols <- x_cols
  }
  data$.color_group <- as.character(
    data_utils$create_interaction(data, color_cols, factor_order)
  )
  list(
    data = data,
    color_legend_title = paste(color_cols, collapse = " | ")
  )
}

#' Prepare shape mapping
#' @param data Data frame
#' @param shape_cols Character vector of shape column names
#' @return List with data, use_shape flag, and legend_title
#' @export
prepare_shape <- function(data, shape_cols) {
  if (is.null(shape_cols) || length(shape_cols) == 0) {
    return(list(data = data, use_shape = FALSE, legend_title = NULL))
  }
  valid <- shape_cols[shape_cols %in% names(data)]
  if (length(valid) == 0) {
    return(list(data = data, use_shape = FALSE, legend_title = NULL))
  }
  data$.shape_group <- as.character(
    data_utils$create_interaction(data, valid)
  )
  n_shapes <- length(unique(data$.shape_group))
  if (n_shapes > 6) {
    rhino$log$warn(
      "Plot: shape mapping has {n_shapes} groups ",
      "(>6 may reduce readability)"
    )
  }
  list(
    data = data,
    use_shape = TRUE,
    legend_title = paste(valid, collapse = " | ")
  )
}

#' Prepare custom shape mapping from shape_map
#' @param data Data frame with .color_group column
#' @param shape_map Named integer vector mapping group names to shape values
#' @return Data frame with .point_shape column added
#' @export
prepare_custom_shapes <- function(data, shape_map) {
  data$.point_shape <- vapply(
    data$.color_group,
    function(g) if (g %in% names(shape_map)) shape_map[[g]] else 19L,
    integer(1)
  )
  data
}


# =============================================================================
# Processing
# =============================================================================

#' Apply outlier detection + trimming to data
#' @param data Data frame
#' @param y_col Name of Y column
#' @param interaction_term Factor for grouping
#' @param proc Resolved processing parameters
#' @return Data frame with .is_outlier and .is_trimmed columns
#' @export
apply_processing <- function(data, y_col, interaction_term, proc) {
  # Outlier detection
  if (isTRUE(proc$outlier_enabled)) {
    data$.is_outlier <- data_processing$detect_outliers(
      data = data,
      value_col = y_col,
      group_col = interaction_term,
      method = proc$outlier_method,
      factor = proc$outlier_factor,
      bootstrap_samples = proc$bootstrap_samples
    )
  } else {
    data$.is_outlier <- FALSE
  }

  # Trimming (only on non-outlier rows)
  data$.is_trimmed <- FALSE
  if (proc$trim_percent > 0) {
    non_outlier <- which(!data$.is_outlier)
    if (length(non_outlier) > 0) {
      data$.is_trimmed[non_outlier] <- data_processing$mark_trimmed(
        values = data[[y_col]][non_outlier],
        group_col = interaction_term[non_outlier],
        trim_percent = proc$trim_percent
      )
    }
  }

  data
}


# =============================================================================
# Tooltip
# =============================================================================

#' Build tooltip HTML for each data point
#' @param data Data frame (must contain .is_trimmed, .is_outlier columns)
#' @param x_var Name of x variable in data
#' @param x_label Display label for x axis
#' @param y_col Name of y column
#' @param tooltip_cols Additional columns to include
#' @return Character vector of tooltip HTML strings
#' @export
build_tooltip_text <- function(data, x_var, x_label, y_col,
                               tooltip_cols = NULL) {
  parts <- paste0(
    "<strong>", x_label, ":</strong> ", data[[x_var]], "<br/>",
    "<strong>", y_col, ":</strong> ", round(data[[y_col]], 4)
  )

  # Extra tooltip columns
  if (!is.null(tooltip_cols) && length(tooltip_cols) > 0) {
    valid_cols <- tooltip_cols[tooltip_cols %in% names(data)]
    if (length(valid_cols) > 0) {
      extra <- vapply(base::seq_len(nrow(data)), function(i) {
        col_parts <- vapply(valid_cols, function(col) {
          paste0("<strong>", col, ":</strong> ", data[[col]][i])
        }, character(1))
        paste(col_parts, collapse = "<br/>")
      }, character(1))
      parts <- paste0(parts, "<br/>", extra)
    }
  }

  # Status flags
  is_trimmed <- data[[".is_trimmed"]]
  is_outlier <- data[[".is_outlier"]]
  status <- vapply(base::seq_len(nrow(data)), function(i) {
    flags <- character(0)
    if (isTRUE(is_trimmed[i])) {
      flags <- c(flags,
        "<span style='color:#dc3545;'>Trimmed</span>")
    }
    if (isTRUE(is_outlier[i])) {
      flags <- c(flags,
        "<span style='color:#fd7e14;'>Outlier</span>")
    }
    if (length(flags) > 0) {
      paste0("<br/><em>", paste(flags, collapse = ", "), "</em>")
    } else {
      ""
    }
  }, character(1))
  paste0(parts, status)
}


# =============================================================================
# Theme and scales
# =============================================================================

#' Apply theme, grid, and border settings
#' @param p ggplot object
#' @param x_cols Character vector of X-axis column names
#' @param gl Resolved grid/legend parameters
#' @param ax Resolved axis style parameters
#' @return ggplot object with theme applied
#' @export
apply_theme <- function(p, x_cols, gl, ax) {
  p <- p +
    ggplot2$theme_bw() +
    ggplot2$theme(
      axis.text.x = ggplot2$element_text(
        angle = if (length(x_cols) == 1) 45 else 0,
        hjust = 1
      ),
      panel.grid.major.x = if (!gl$v_grid) {
        ggplot2$element_blank()
      } else {
        ggplot2$element_line(color = "gray90")
      },
      panel.grid.minor.x = if (!gl$v_grid) {
        ggplot2$element_blank()
      } else {
        ggplot2$element_line(color = "gray90")
      },
      panel.grid.major.y = if (!gl$h_grid) {
        ggplot2$element_blank()
      } else {
        ggplot2$element_line(color = "gray90")
      },
      panel.grid.minor.y = if (!gl$h_grid) {
        ggplot2$element_blank()
      } else {
        ggplot2$element_line(color = "gray90")
      },
      axis.ticks.length = ggplot2$unit(ax$tick_length, "cm"),
      axis.line = if (gl$top_right_borders) {
        ggplot2$element_blank()
      } else {
        ggplot2$element_line(
          color = "black", linewidth = ax$line_thickness
        )
      },
      panel.border = if (gl$top_right_borders) {
        ggplot2$element_rect(
          color = "black", fill = NA,
          linewidth = ax$line_thickness * 2
        )
      } else {
        ggplot2$element_blank()
      },
      legend.position = gl$legend_position,
      plot.margin = ggplot2$margin(10, 10, 10, 10)
    )

  if (isTRUE(gl$aspect_ratio)) {
    p <- p + ggplot2$theme(aspect.ratio = 1)
  }

  p
}

#' Apply color scales to plot
#' @param p ggplot object
#' @param color_map Named character vector of colors
#' @param color_legend_title Title for color legend
#' @return ggplot object with color scales applied
#' @export
apply_color_scales <- function(p, color_map, color_legend_title) {
  if (!is.null(color_map) && length(color_map) > 0) {
    p <- p +
      ggplot2$scale_color_manual(
        values = color_map, name = color_legend_title
      ) +
      ggplot2$scale_fill_manual(
        values = color_map, name = color_legend_title
      )
  } else {
    p <- p +
      ggplot2$scale_color_discrete(name = color_legend_title) +
      ggplot2$scale_fill_discrete(name = color_legend_title)
  }
  p
}

#' Apply shape scale to plot
#' @param p ggplot object
#' @param data Data frame with .shape_group column
#' @param shape_legend_title Title for shape legend
#' @return ggplot object with shape scale applied
#' @export
apply_shape_scale <- function(p, data, shape_legend_title) {
  n <- length(unique(data$.shape_group))
  fillable <- c(21, 22, 23, 24, 25, 3)
  vals <- fillable[base::seq_len(min(n, length(fillable)))]
  p + ggplot2$scale_shape_manual(
    values = vals, name = shape_legend_title
  )
}

#' Add legendry nested axis guide for multi-column x-axis
#' @param p ggplot object
#' @return ggplot object with nested axis guide
#' @export
add_nested_axis <- function(p) {
  centered_text <- replicate(
    10,
    ggplot2$element_text(hjust = 0.5),
    simplify = FALSE
  )
  p + ggplot2$guides(
    x = legendry$guide_axis_nested(levels_text = centered_text)
  )
}
