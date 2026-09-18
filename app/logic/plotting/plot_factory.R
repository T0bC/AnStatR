box::use(
  ggplot2,
  rhino,
)

box::use(
  app/logic/plotting/boxplot_builder,
  app/logic/plotting/plot_helpers,
  app/logic/plotting/scatter_builder,
  app/logic/plotting/violin_builder,
  app/logic/shared/data_utils,
)

# =============================================================================
# Plot type constants
# =============================================================================

#' Available plot types
#' @export
plot_types <- list(
  SCATTER        = "scatter",
  BOXPLOT        = "boxplot",
  BOXPLOT_POINTS = "boxplot_points",
  VIOLIN         = "violin",
  VIOLIN_POINTS  = "violin_points"
)

#' Get plot type choices for UI dropdown
#' @return Named character vector for selectInput choices
#' @export
get_plot_type_choices <- function() {
  c(
    "Scatter"          = plot_types$SCATTER,
    "Boxplot"          = plot_types$BOXPLOT,
    "Boxplot + Points" = plot_types$BOXPLOT_POINTS,
    "Violin"           = plot_types$VIOLIN,
    "Violin + Points"  = plot_types$VIOLIN_POINTS
  )
}

#' Check if plot type shows points
#' @param plot_type Character string plot type
#' @return Logical
#' @export
shows_points <- function(plot_type) {
  plot_type %in% c(
    plot_types$SCATTER,
    plot_types$BOXPLOT_POINTS,
    plot_types$VIOLIN_POINTS
  )
}

#' Check if plot type shows median/SD overlays
#' @param plot_type Character string plot type
#' @return Logical
#' @export
shows_stat_overlays <- function(plot_type) {
  plot_type %in% c(
    plot_types$SCATTER,
    plot_types$BOXPLOT_POINTS,
    plot_types$VIOLIN_POINTS
  )
}

#' Check if plot type is boxplot variant
#' @param plot_type Character string plot type
#' @return Logical
#' @export
is_boxplot_type <- function(plot_type) {
  plot_type %in% c(plot_types$BOXPLOT, plot_types$BOXPLOT_POINTS)
}

#' Check if plot type is violin variant
#' @param plot_type Character string plot type
#' @return Logical
#' @export
is_violin_type <- function(plot_type) {
  plot_type %in% c(plot_types$VIOLIN, plot_types$VIOLIN_POINTS)
}


# =============================================================================
# Main factory function
# =============================================================================

#' Create a plot using the factory pattern
#'
#' Main entry point for creating plots. Dispatches to appropriate builder
#' based on plot_type.
#'
#' @param plot_type Character string: one of plot_types values
#' @param data Data frame containing the data to plot
#' @param x_cols Character vector of column name(s) for X-axis
#' @param y_col Character string of column name for Y-axis (measurement)
#' @param color_map Named character vector mapping group names to hex colors
#' @param shape_map Named integer vector mapping group names to shape values
#' @param color_cols Character vector of column name(s) for color grouping
#' @param tooltip_cols Character vector of additional column names for tooltip
#' @param point_style List: size, spread (jitter width), alpha, shape_cols
#' @param processing List: trim_percent, outlier_enabled, outlier_method,
#'   outlier_factor, bootstrap_samples
#' @param grid_legend List: legend_position, h_grid, v_grid,
#'   top_right_borders, show_median, show_sd, aspect_ratio
#' @param stat_line_style List: median_thickness, median_width,
#'   sd_thickness, sd_width
#' @param axis_style List: tick_length, line_thickness
#' @param boxplot_style List: box_width, show_outliers, notch
#' @param violin_style List: violin_width, trim, scale
#' @param factor_order Optional named list of custom factor level orderings
#' @return A ggplot2 object (ready for ggiraph::girafe)
#' @export
create_plot <- function(plot_type = "scatter",
                        data,
                        x_cols,
                        y_col,
                        color_map = NULL,
                        shape_map = NULL,
                        color_cols = NULL,
                        tooltip_cols = NULL,
                        point_style = list(),
                        processing = list(),
                        grid_legend = list(),
                        stat_line_style = list(),
                        axis_style = list(),
                        boxplot_style = list(),
                        violin_style = list(),
                        factor_order = NULL,
                        black_points = FALSE) {
  # --- Validate inputs ---
  validation <- plot_helpers$validate_plot_inputs(data, x_cols, y_col)
  if (!is.null(validation)) {
    return(validation)
  }

  rhino$log$info(
    "Plot ({plot_type}): {y_col} by {paste(x_cols, collapse = ' | ')} ",
    "({nrow(data)} rows)"
  )

  # --- Resolve defaults ---
  ps <- plot_helpers$resolve_point_style(point_style)
  proc <- plot_helpers$resolve_processing(processing)
  gl <- plot_helpers$resolve_grid_legend(grid_legend)
  sls <- plot_helpers$resolve_stat_line_style(stat_line_style)
  ax <- plot_helpers$resolve_axis_style(axis_style)
  bp <- plot_helpers$resolve_boxplot_style(boxplot_style)
  vp <- plot_helpers$resolve_violin_style(violin_style)

  # --- Prepare x-axis variable ---
  x_prep <- plot_helpers$prepare_x_axis(data, x_cols, factor_order)
  data <- x_prep$data
  x_var <- x_prep$x_var
  x_label <- x_prep$x_label

  # --- Prepare color grouping ---
  color_prep <- plot_helpers$prepare_color_group(
    data, color_cols, x_cols, factor_order
  )
  data <- color_prep$data
  color_legend_title <- color_prep$color_legend_title

  # --- Prepare shape grouping ---
  #
  # Shapes group by "Shape by" when set, otherwise by the X-axis columns --
  # independently of the colour grouping.  The user's shape_map applies in
  # both cases; "Shape by" additionally puts a shape legend on the plot.
  use_shape <- FALSE
  use_custom_shape <- FALSE
  # Default path renders shape 21 (fill only); keep the color scale only when
  # a non-fillable shape (pch 0-14) is in play
  skip_color_scale <- shows_points(plot_type)
  shape_legend_title <- NULL
  fillable_shapes <- TRUE

  if (shows_points(plot_type)) {
    shape_prep <- plot_helpers$prepare_shape_group(
      data, ps$shape_cols, x_cols, factor_order
    )
    data <- shape_prep$data
    shape_legend_title <- shape_prep$legend_title

    if (!is.null(shape_map) && length(shape_map) > 0) {
      data <- plot_helpers$prepare_custom_shapes(data, shape_map)
      fillable_shapes <- plot_helpers$all_fillable_shapes(
        data$.point_shape
      )
      skip_color_scale <- fillable_shapes
      # "Shape by" renders through the aesthetic so the legend shows up;
      # otherwise the per-row shape vector is enough
      use_shape <- shape_prep$explicit
      use_custom_shape <- !shape_prep$explicit
    } else {
      use_shape <- shape_prep$explicit
      skip_color_scale <- TRUE
    }
  }

  # --- Process data: outliers + trimming ---
  interaction_term <- data_utils$create_interaction(data, x_cols, factor_order)
  data <- plot_helpers$apply_processing(data, y_col, interaction_term, proc)

  # --- Build tooltip ---
  data$.tooltip <- plot_helpers$build_tooltip_text(
    data, x_var, x_label, y_col, tooltip_cols
  )

  # --- Initialize base plot ---
  p <- ggplot2$ggplot(
    data,
    ggplot2$aes(x = .data[[x_var]], y = .data[[y_col]])
  )

  # --- Dispatch to appropriate builder ---
  p <- switch(plot_type,
    "scatter" = scatter_builder$build_scatter_layers(
      p, data, ps, gl, sls, use_shape, use_custom_shape, black_points,
      fillable_shapes
    ),
    "boxplot" = boxplot_builder$build_boxplot_layers(
      p, data, bp, ps, gl
    ),
    "boxplot_points" = boxplot_builder$build_boxplot_points_layers(
      p, data, bp, ps, gl, sls, use_shape, use_custom_shape, black_points,
      fillable_shapes
    ),
    "violin" = violin_builder$build_violin_layers(
      p, data, vp, ps, gl
    ),
    "violin_points" = violin_builder$build_violin_points_layers(
      p, data, vp, ps, gl, sls, use_shape, use_custom_shape, black_points,
      fillable_shapes
    ),
    # Default to scatter
    scatter_builder$build_scatter_layers(
      p, data, ps, gl, sls, use_shape, use_custom_shape, black_points,
      fillable_shapes
    )
  )

  # --- Pin x-axis order (layers may cover disjoint subsets of the x factor) ---
  p <- plot_helpers$apply_x_scale(p, data, x_var)

  # --- Apply color scales ---
  p <- plot_helpers$apply_color_scales(p, color_map, color_legend_title, skip_color_scale)

  # --- Apply shape scale if using shape aesthetic ---
  if (use_shape) {
    p <- plot_helpers$apply_shape_scale(
      p, data, shape_legend_title, shape_map
    )
  }

  # --- Labels ---
  labs_args <- list(x = x_label, y = y_col, fill = color_legend_title)
  if (!skip_color_scale) labs_args$color <- color_legend_title
  p <- p + do.call(ggplot2$labs, labs_args)

  # --- Theme ---
  p <- plot_helpers$apply_theme(p, x_cols, gl, ax)

  # --- Stats legend (separate from color legend) ---
  p <- plot_helpers$add_stats_legend(p, gl, plot_type, use_shape)

  # --- Nested axis ---
  if (length(x_cols) > 1) {
    p <- plot_helpers$add_nested_axis(p)
  }

  p
}
