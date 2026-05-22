box::use(
  ggplot2,
  ggiraph,
)

box::use(
  app/logic/plotting/plot_helpers,
  app/logic/plotting/scatter_builder,
)

# =============================================================================
# Violin-specific layer builders
# =============================================================================

#' Add violin layer
#'
#' @param p ggplot object with base aesthetics
#' @param data Data frame with .color_group column
#' @param vp Resolved violin style parameters
#' @param ps Resolved point style parameters (for alpha)
#' @return ggplot object with violin layer added
#' @export
add_violin_layer <- function(p, data, vp, ps) {
  # Filter to retained data only (exclude trimmed and outliers)
  is_trimmed <- data[[".is_trimmed"]]
  is_outlier <- data[[".is_outlier"]]
  retained_idx <- which(!is_trimmed & !is_outlier)

  if (length(retained_idx) == 0) return(p)

  rd <- data[retained_idx, , drop = FALSE]

  # Violin with color fill by group
  p <- p + ggplot2$geom_violin(
    data = rd,
    ggplot2$aes(
      fill = .data[[".color_group"]]
    ),
    width = vp$violin_width,
    alpha = vp$alpha,
    trim = vp$trim,
    scale = vp$scale,
    color = "black",
    linewidth = 0.5
  )

  p
}


#' Add interactive violin layer with tooltips
#'
#' @param p ggplot object with base aesthetics
#' @param data Data frame with .color_group column
#' @param vp Resolved violin style parameters
#' @param ps Resolved point style parameters (for alpha)
#' @return ggplot object with interactive violin layer added
#' @export
add_violin_layer_interactive <- function(p, data, vp, ps) {
  # Filter to retained data only (exclude trimmed and outliers)
  is_trimmed <- data[[".is_trimmed"]]
  is_outlier <- data[[".is_outlier"]]
  retained_idx <- which(!is_trimmed & !is_outlier)

  if (length(retained_idx) == 0) return(p)

  rd <- data[retained_idx, , drop = FALSE]

  # Interactive violin with color fill by group
  p <- p + ggiraph$geom_violin_interactive(
    data = rd,
    ggplot2$aes(
      fill = .data[[".color_group"]],
      data_id = .data[[".color_group"]]
    ),
    width = vp$violin_width,
    alpha = vp$alpha,
    trim = vp$trim,
    scale = vp$scale,
    color = "black",
    linewidth = 0.5
  )

  p
}


#' Build complete violin layers (violin only, no points)
#'
#' @param p ggplot object with base aesthetics
#' @param data Prepared data frame
#' @param vp Resolved violin style parameters
#' @param ps Resolved point style parameters
#' @param gl Resolved grid/legend parameters (for stat point overlays)
#' @return ggplot object with violin layers
#' @export
build_violin_layers <- function(p, data, vp, ps, gl = list()) {
  p <- add_violin_layer_interactive(p, data, vp, ps)
  p <- scatter_builder$add_stat_point_overlays(p, data, gl)
  p
}


#' Build complete violin + points layers
#'
#' Combines violin with scatter points underneath.
#'
#' @param p ggplot object with base aesthetics
#' @param data Prepared data frame
#' @param vp Resolved violin style parameters
#' @param ps Resolved point style parameters
#' @param gl Resolved grid/legend parameters (for stat point overlays)
#' @param use_shape Whether to use shape aesthetic
#' @param use_custom_shape Whether to use custom shapes
#' @param black_points Whether to force points to be black
#' @return ggplot object with violin and scatter layers
#' @export
build_violin_points_layers <- function(p, data, vp, ps, gl = list(),
                                       use_shape = FALSE,
                                       use_custom_shape = FALSE,
                                       black_points = FALSE) {
  # First add scatter points underneath
  p <- scatter_builder$add_scatter_layers(
    p, data, ps, use_shape, use_custom_shape, black_points
  )

  # Then add violin on top
  p <- add_violin_layer_interactive(p, data, vp, ps)

  # Stat point overlays (median/mean markers) on top
  p <- scatter_builder$add_stat_point_overlays(p, data, gl)

  p
}
