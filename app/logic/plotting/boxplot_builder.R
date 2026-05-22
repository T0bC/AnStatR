box::use(
  ggplot2,
  ggiraph,
)

box::use(
  app/logic/plotting/plot_helpers,
  app/logic/plotting/scatter_builder,
)

# =============================================================================
# Boxplot-specific layer builders
# =============================================================================

#' Add boxplot layer
#'
#' @param p ggplot object with base aesthetics
#' @param data Data frame with .color_group column
#' @param bp Resolved boxplot style parameters
#' @param ps Resolved point style parameters (for alpha)
#' @return ggplot object with boxplot layer added
#' @export
add_boxplot_layer <- function(p, data, bp, ps) {
  # Filter to retained data only (exclude trimmed and outliers)
  is_trimmed <- data[[".is_trimmed"]]
  is_outlier <- data[[".is_outlier"]]
  retained_idx <- which(!is_trimmed & !is_outlier)

  if (length(retained_idx) == 0) return(p)

  rd <- data[retained_idx, , drop = FALSE]

  # Boxplot with color fill by group
  p <- p + ggplot2$geom_boxplot(
    data = rd,
    ggplot2$aes(
      fill = .data[[".color_group"]]
    ),
    width = bp$box_width,
    alpha = ps$alpha,
    outlier.shape = if (bp$show_outliers) 19 else NA,
    notch = bp$notch,
    color = "black",
    linewidth = 0.5
  )

  p
}


#' Add interactive boxplot layer with tooltips
#'
#' @param p ggplot object with base aesthetics
#' @param data Data frame with .color_group and .tooltip columns
#' @param bp Resolved boxplot style parameters
#' @param ps Resolved point style parameters (for alpha)
#' @return ggplot object with interactive boxplot layer added
#' @export
add_boxplot_layer_interactive <- function(p, data, bp, ps) {
  # Filter to retained data only (exclude trimmed and outliers)
  is_trimmed <- data[[".is_trimmed"]]
  is_outlier <- data[[".is_outlier"]]
  retained_idx <- which(!is_trimmed & !is_outlier)

  if (length(retained_idx) == 0) return(p)

  rd <- data[retained_idx, , drop = FALSE]

  # Interactive boxplot with color fill by group
  p <- p + ggiraph$geom_boxplot_interactive(
    data = rd,
    ggplot2$aes(
      fill = .data[[".color_group"]],
      data_id = .data[[".color_group"]]
    ),
    width = bp$box_width,
    alpha = ps$alpha,
    outlier.shape = if (bp$show_outliers) 19 else NA,
    notch = bp$notch,
    color = "black",
    linewidth = 0.5
  )

  p
}


#' Build complete boxplot layers (boxplot only, no points)
#'
#' @param p ggplot object with base aesthetics
#' @param data Prepared data frame
#' @param bp Resolved boxplot style parameters
#' @param ps Resolved point style parameters
#' @return ggplot object with boxplot layers
#' @export
build_boxplot_layers <- function(p, data, bp, ps) {
  add_boxplot_layer_interactive(p, data, bp, ps)
}


#' Build complete boxplot + points layers
#'
#' Combines boxplot with scatter points overlay.
#'
#' @param p ggplot object with base aesthetics
#' @param data Prepared data frame
#' @param bp Resolved boxplot style parameters
#' @param ps Resolved point style parameters
#' @param use_shape Whether to use shape aesthetic
#' @param use_custom_shape Whether to use custom shapes
#' @return ggplot object with boxplot and scatter layers
#' @export
build_boxplot_points_layers <- function(p, data, bp, ps,
                                        use_shape = FALSE,
                                        use_custom_shape = FALSE) {
  # First add boxplot (without outliers since we'll show all points)
  bp_no_outliers <- bp
  bp_no_outliers$show_outliers <- FALSE

  p <- add_boxplot_layer_interactive(p, data, bp_no_outliers, ps)

  # Then add scatter points on top
  p <- scatter_builder$add_scatter_layers(
    p, data, ps, use_shape, use_custom_shape
  )

  p
}
