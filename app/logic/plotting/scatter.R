box::use(
  ggplot2,
  ggiraph,
  legendry,
  rhino,
)

box::use(
  app/logic/data_utils,
  app/logic/plotting/data_processing,
)

# =============================================================================
# Public API
# =============================================================================

#' Create an interactive scatter plot for a single measurement variable
#'
#' Pure logic function — no Shiny dependencies.
#' Uses ggiraph for interactivity and legendry for nested X-axis labels.
#'
#' @param data Data frame containing the data to plot
#' @param x_cols Character vector of column name(s) for X-axis
#' @param y_col Character string of column name for Y-axis (measurement)
#' @param color_map Named character vector mapping group names to hex colors
#' @param color_cols Character vector of column name(s) for color grouping.
#'   Defaults to x_cols when NULL.
#' @param tooltip_cols Character vector of additional column names for tooltip
#' @param point_style List: size, spread (jitter width), alpha, shape_cols
#' @param processing List: trim_percent, outlier_enabled, outlier_method,
#'   outlier_factor, bootstrap_samples
#' @param grid_legend List: legend_position, h_grid, v_grid,
#'   top_right_borders, show_median, show_sd, aspect_ratio
#' @param stat_line_style List: median_thickness, median_width,
#'   sd_thickness, sd_width
#' @param axis_style List: tick_length, line_thickness
#' @return A ggplot2 object (ready for ggiraph::girafe)
#' @export
create_scatter_plot <- function(data,
                                x_cols,
                                y_col,
                                color_map = NULL,
                                color_cols = NULL,
                                tooltip_cols = NULL,
                                point_style = list(),
                                processing = list(),
                                grid_legend = list(),
                                stat_line_style = list(),
                                axis_style = list()) {
  # --- Validate inputs ---
  validation <- validate_plot_inputs(data, x_cols, y_col)
  if (!is.null(validation)) return(validation)

  rhino$log$info(
    "Plot: {y_col} by {paste(x_cols, collapse = ' | ')} ",
    "({nrow(data)} rows)"
  )

  # --- Defaults ---
  ps <- resolve_point_style(point_style)
  proc <- resolve_processing(processing)
  gl <- resolve_grid_legend(grid_legend)
  sls <- resolve_stat_line_style(stat_line_style)
  ax <- resolve_axis_style(axis_style)

  # --- Prepare x-axis variable ---
  x_prep <- prepare_x_axis(data, x_cols)
  data <- x_prep$data
  x_var <- x_prep$x_var
  x_label <- x_prep$x_label

  # --- Prepare color grouping ---
  if (is.null(color_cols) || length(color_cols) == 0) {
    color_cols <- x_cols
  }
  data$.color_group <- as.character(
    data_utils$create_interaction(data, color_cols)
  )
  color_legend_title <- paste(color_cols, collapse = " | ")

  # --- Prepare shape grouping ---
  shape_prep <- prepare_shape(data, ps$shape_cols)
  data <- shape_prep$data
  use_shape <- shape_prep$use_shape
  shape_legend_title <- shape_prep$legend_title

  # --- Process data: outliers + trimming ---
  interaction_term <- data_utils$create_interaction(data, x_cols)
  data <- apply_processing(data, y_col, interaction_term, proc)

  # --- Build tooltip ---
  data$.tooltip <- build_tooltip_text(
    data, x_var, x_label, y_col, tooltip_cols
  )

  # --- Build plot layers ---
  p <- build_plot(
    data = data,
    x_var = x_var,
    y_col = y_col,
    ps = ps,
    use_shape = use_shape,
    color_map = color_map,
    color_legend_title = color_legend_title,
    shape_legend_title = shape_legend_title
  )

  # --- Statistical overlays ---
  p <- add_stat_overlays(p, data, gl, sls)

  # --- Labels ---
  p <- p + ggplot2$labs(
    x = x_label,
    y = y_col,
    color = color_legend_title,
    fill = color_legend_title
  )

  # --- Theme ---
  p <- apply_theme(p, x_cols, gl, ax)

  # --- Nested axis ---
  if (length(x_cols) > 1) {
    p <- add_nested_axis(p)
  }

  p
}


#' Create an empty placeholder plot with a message
#'
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


#' Build tooltip HTML for each data point
#'
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
# Internal helpers — defaults
# =============================================================================

resolve_point_style <- function(ps) {
  list(
    size       = ps$size       %||% 4,
    spread     = ps$spread     %||% 0.15,
    alpha      = ps$alpha      %||% 0.6,
    shape_cols = ps$shape_cols
  )
}

resolve_processing <- function(proc) {
  list(
    trim_percent      = proc$trim_percent      %||% 0,
    outlier_enabled   = proc$outlier_enabled    %||% FALSE,
    outlier_method    = proc$outlier_method     %||% "IQR",
    outlier_factor    = proc$outlier_factor     %||% 1.5,
    bootstrap_samples = proc$bootstrap_samples  %||% 1000
  )
}

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

resolve_stat_line_style <- function(sls) {
  list(
    median_thickness = sls$median_thickness %||% 0.5,
    median_width     = sls$median_width     %||% 0.15,
    sd_thickness     = sls$sd_thickness     %||% 0.5,
    sd_width         = sls$sd_width         %||% 0.15
  )
}

resolve_axis_style <- function(ax) {
  list(
    tick_length    = ax$tick_length    %||% 0.15,
    line_thickness = ax$line_thickness %||% 0.5
  )
}


# =============================================================================
# Internal helpers — data preparation
# =============================================================================

#' Validate required plot inputs; returns empty plot or NULL if valid
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

#' Prepare x-axis: single column or nested interaction
prepare_x_axis <- function(data, x_cols) {
  if (length(x_cols) > 1) {
    # Reverse for legendry: first selected = outer grouping
    x_nested <- data_utils$create_interaction(
      data, base::rev(x_cols)
    )
    data$.x_nested <- x_nested
    list(
      data = data,
      x_var = ".x_nested",
      x_label = paste(x_cols, collapse = " | ")
    )
  } else {
    list(
      data = data,
      x_var = x_cols,
      x_label = x_cols
    )
  }
}

#' Prepare shape mapping
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


# =============================================================================
# Internal helpers — processing
# =============================================================================

#' Apply outlier detection + trimming to data
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
# Internal helpers — plot construction
# =============================================================================

#' Build the core ggplot with jitter layers for retained / trimmed / outlier
build_plot <- function(data, x_var, y_col, ps,
                       use_shape, color_map,
                       color_legend_title, shape_legend_title) {
  p <- ggplot2$ggplot(
    data,
    ggplot2$aes(x = .data[[x_var]], y = .data[[y_col]])
  )

  is_trimmed <- data[[".is_trimmed"]]
  is_outlier <- data[[".is_outlier"]]
  retained_idx <- which(!is_trimmed & !is_outlier)
  trimmed_idx  <- which(is_trimmed & !is_outlier)
  outlier_idx  <- which(is_outlier)

  # Layer 1: Retained points (colored, optionally shaped)
  if (length(retained_idx) > 0) {
    rd <- data[retained_idx, , drop = FALSE]
    rd$.data_id <- retained_idx

    if (use_shape) {
      aes_map <- ggplot2$aes(
        tooltip = .data[[".tooltip"]],
        data_id = .data[[".data_id"]],
        color   = .data[[".color_group"]],
        fill    = .data[[".color_group"]],
        shape   = .data[[".shape_group"]]
      )
    } else {
      aes_map <- ggplot2$aes(
        tooltip = .data[[".tooltip"]],
        data_id = .data[[".data_id"]],
        color   = .data[[".color_group"]],
        fill    = .data[[".color_group"]]
      )
    }

    p <- p + ggiraph$geom_jitter_interactive(
      data = rd, mapping = aes_map,
      hover_nearest = TRUE,
      position = ggplot2$position_jitter(
        width = ps$spread, height = 0, seed = 42L
      ),
      alpha = ps$alpha, size = ps$size
    )
  }

  # Layer 2: Trimmed points (gray outline, white fill)
  if (length(trimmed_idx) > 0) {
    td <- data[trimmed_idx, , drop = FALSE]
    td$.data_id <- trimmed_idx

    p <- p + ggiraph$geom_jitter_interactive(
      data = td,
      ggplot2$aes(
        tooltip = .data[[".tooltip"]],
        data_id = .data[[".data_id"]]
      ),
      shape = 21, color = "gray40", fill = "white",
      hover_nearest = TRUE,
      position = ggplot2$position_jitter(
        width = ps$spread, height = 0, seed = 42L
      ),
      alpha = ps$alpha, size = ps$size
    )
  }

  # Layer 3: Outlier points (gray X)
  if (length(outlier_idx) > 0) {
    od <- data[outlier_idx, , drop = FALSE]
    od$.data_id <- outlier_idx

    p <- p + ggiraph$geom_jitter_interactive(
      data = od,
      ggplot2$aes(
        tooltip = .data[[".tooltip"]],
        data_id = .data[[".data_id"]]
      ),
      shape = 4, color = "gray40",
      hover_nearest = TRUE,
      position = ggplot2$position_jitter(
        width = ps$spread, height = 0, seed = 42L
      ),
      alpha = ps$alpha, size = ps$size
    )
  }

  # Color scales
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

  # Shape scale
  if (use_shape) {
    n <- length(unique(data$.shape_group))
    fillable <- c(21, 22, 23, 24, 25, 3)
    vals <- fillable[base::seq_len(min(n, length(fillable)))]
    p <- p + ggplot2$scale_shape_manual(
      values = vals, name = shape_legend_title
    )
  }

  p
}


#' Add median crossbar and SD error bars on retained data
add_stat_overlays <- function(p, data, gl, sls) {
  retained_idx <- which(!data[[".is_trimmed"]] & !data[[".is_outlier"]])
  if (length(retained_idx) == 0) return(p)

  rd <- data[retained_idx, , drop = FALSE]

  if (isTRUE(gl$show_median)) {
    p <- p + ggplot2$stat_summary(
      data = rd,
      fun = stats::median,
      geom = "crossbar",
      width = sls$median_width,
      color = "black",
      linewidth = sls$median_thickness
    )
  }

  if (isTRUE(gl$show_sd)) {
    p <- p + ggplot2$stat_summary(
      data = rd,
      fun.data = ggplot2$mean_sdl,
      fun.args = list(mult = 1),
      geom = "errorbar",
      width = sls$sd_width,
      color = "black",
      linewidth = sls$sd_thickness
    )
  }

  p
}


#' Apply theme, grid, and border settings
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


#' Add legendry nested axis guide for multi-column x-axis
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
