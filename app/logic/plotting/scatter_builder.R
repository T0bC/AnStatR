box::use(
  ggplot2,
  ggiraph,
)

box::use(
  app/logic/plotting/plot_helpers,
)

# =============================================================================
# Scatter-specific layer builders
# =============================================================================

#' Add scatter point layers (jittered) for retained/trimmed/outlier points
#'
#' @param p ggplot object with base aesthetics
#' @param data Data frame with .is_trimmed, .is_outlier, .tooltip, .color_group
#' @param ps Resolved point style parameters
#' @param use_shape Whether to use shape aesthetic from .shape_group
#' @param use_custom_shape Whether to use custom shapes from .point_shape
#' @return ggplot object with scatter layers added
#' @export
add_scatter_layers <- function(p, data, ps, use_shape = FALSE,
                               use_custom_shape = FALSE) {
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
      # Shape by column mapping (aesthetic)
      aes_map <- ggplot2$aes(
        tooltip = .data[[".tooltip"]],
        data_id = .data[[".data_id"]],
        color   = .data[[".color_group"]],
        fill    = .data[[".color_group"]],
        shape   = .data[[".shape_group"]]
      )
      p <- p + ggiraph$geom_jitter_interactive(
        data = rd, mapping = aes_map,
        hover_nearest = TRUE,
        position = ggplot2$position_jitter(
          width = ps$spread, height = 0, seed = 42L
        ),
        alpha = ps$alpha, size = ps$size
      )
    } else if (use_custom_shape) {
      # Custom shapes per group: pass shape as a vector (not aesthetic)
      aes_map <- ggplot2$aes(
        tooltip = .data[[".tooltip"]],
        data_id = .data[[".data_id"]],
        color   = .data[[".color_group"]],
        fill    = .data[[".color_group"]]
      )
      p <- p + ggiraph$geom_jitter_interactive(
        data = rd, mapping = aes_map,
        shape = rd$.point_shape,
        hover_nearest = TRUE,
        position = ggplot2$position_jitter(
          width = ps$spread, height = 0, seed = 42L
        ),
        alpha = ps$alpha, size = ps$size
      )
    } else {
      # Default: no shape variation
      aes_map <- ggplot2$aes(
        tooltip = .data[[".tooltip"]],
        data_id = .data[[".data_id"]],
        color   = .data[[".color_group"]],
        fill    = .data[[".color_group"]]
      )
      p <- p + ggiraph$geom_jitter_interactive(
        data = rd, mapping = aes_map,
        hover_nearest = TRUE,
        position = ggplot2$position_jitter(
          width = ps$spread, height = 0, seed = 42L
        ),
        alpha = ps$alpha, size = ps$size
      )
    }
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

  p
}


#' Add median crossbar and SD error bars on retained data
#'
#' @param p ggplot object
#' @param data Data frame with .is_trimmed and .is_outlier columns
#' @param gl Resolved grid/legend parameters (for show_median, show_sd)
#' @param sls Resolved stat line style parameters
#' @return ggplot object with stat overlays added
#' @export
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


#' Build complete scatter plot layers
#'
#' Combines scatter points with optional stat overlays.
#'
#' @param p ggplot object with base aesthetics
#' @param data Prepared data frame
#' @param ps Resolved point style parameters
#' @param gl Resolved grid/legend parameters
#' @param sls Resolved stat line style parameters
#' @param use_shape Whether to use shape aesthetic
#' @param use_custom_shape Whether to use custom shapes
#' @return ggplot object with all scatter layers
#' @export
build_scatter_layers <- function(p, data, ps, gl, sls,
                                 use_shape = FALSE,
                                 use_custom_shape = FALSE) {
  # Add scatter points

  p <- add_scatter_layers(p, data, ps, use_shape, use_custom_shape)

  # Add stat overlays (median/SD)
  p <- add_stat_overlays(p, data, gl, sls)

  p
}
