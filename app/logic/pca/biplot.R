box::use(
  ggplot2,
  ggiraph,
  grDevices,
  rhino,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for PCA biplot
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Create a PCA biplot
#'
#' Builds a ggplot biplot with layer toggles for individuals,
#' variables (loadings), or combined view. Returns a ggplot
#' object — the caller wraps it in ggiraph::girafe().
#'
#' @param pca_result PCA result list (the $result field from run_pca)
#' @param dim_x Character, dimension for x-axis (e.g. "Dim.1")
#' @param dim_y Character, dimension for y-axis (e.g. "Dim.2")
#' @param layer Character, one of "individuals", "variables", "combined"
#' @param group_cols Character vector, column name(s) in ind$meta
#'   for grouping. Multiple columns are combined via interaction().
#'   NULL for no grouping.
#' @param show_convex_hull Logical, use convex hull instead of
#'   95% confidence ellipse
#' @param show_group_shapes Logical, whether to draw
#'   ellipses or hulls around groups at all. Default TRUE.
#' @param point_alpha Character or numeric, "Contribution" or fixed value
#' @param point_size Character or numeric, "Contribution" or fixed value
#' @param show_title Logical, whether to show the plot title
#' @return List with $success, $result (ggplot) or $error
#' @export
create_biplot <- function(pca_result, dim_x = "Dim.1",
                          dim_y = "Dim.2",
                          layer = "combined",
                          group_cols = NULL,
                          show_convex_hull = FALSE,
                          show_group_shapes = TRUE,
                          point_alpha = "Contribution",
                          point_size = "Contribution",
                          show_title = TRUE) {
  error_context <- list(
    dim_x = dim_x,
    dim_y = dim_y,
    layer = layer,
    group_cols = paste(group_cols %||% "none", collapse = ", ")
  )

  error_handling$safe_execute(
    expr = {
      validate_biplot_inputs(pca_result, dim_x, dim_y, layer)

      eig <- pca_result$eig
      show_ind <- layer %in% c("individuals", "combined")
      show_var <- layer %in% c("variables", "combined")

      # Build plot data
      ind_data <- if (show_ind) {
        build_ind_plot_data(
          pca_result, dim_x, dim_y,
          group_cols, point_alpha, point_size
        )
      }
      var_data <- if (show_var) {
        build_var_plot_data(pca_result, dim_x, dim_y)
      }

      # Scale variable arrows in combined mode
      if (layer == "combined" && !is.null(ind_data) &&
          !is.null(var_data)) {
        max_ind <- max(
          abs(c(ind_data$x, ind_data$y)), na.rm = TRUE
        )
        max_var <- max(
          abs(c(var_data$xend, var_data$yend)), na.rm = TRUE
        )
        if (max_var > 0) {
          scale_factor <- max_ind / max_var
          var_data$xend <- var_data$xend * scale_factor
          var_data$yend <- var_data$yend * scale_factor
        }
      }

      # Base plot
      x_label <- axis_label_with_variance(dim_x, eig)
      y_label <- axis_label_with_variance(dim_y, eig)

      p <- ggplot2$ggplot() +
        biplot_theme() +
        ggplot2$labs(x = x_label, y = y_label)

      # Title
      if (show_title) {
        title_text <- switch(
          layer,
          individuals = "PCA — Individuals",
          variables = "PCA — Variables (Loadings)",
          combined = "PCA — Biplot"
        )
        p <- p + ggplot2$ggtitle(title_text)
      }

      # Individual layers
      if (show_ind && !is.null(ind_data)) {
        has_group <- "group" %in% names(ind_data)
        has_contrib <- "contrib" %in% names(ind_data)
        map_alpha <- has_contrib &&
          identical(point_alpha, "Contribution")
        map_size <- has_contrib &&
          identical(point_size, "Contribution")

        # Ensure fill column exists for shape 21
        if (!has_group) {
          ind_data$group <- factor("All")
        }

        # Build aes dynamically
        aes_args <- list(
          x = quote(x), y = quote(y),
          tooltip = quote(tooltip),
          data_id = quote(data_id),
          fill = quote(group)
        )
        if (map_alpha) aes_args$alpha <- quote(contrib)
        if (map_size) aes_args$size <- quote(contrib)
        ind_aes <- do.call(ggplot2$aes, aes_args)

        # Fixed aesthetics (when not mapping)
        fixed_aes <- list()
        if (!map_alpha) {
          fixed_aes$alpha <- as.numeric(point_alpha)
        }
        if (!map_size) {
          fixed_aes$size <- as.numeric(point_size)
        }

        p <- p + do.call(
          ggiraph$geom_point_interactive,
          c(
            list(
              data = ind_data,
              mapping = ind_aes,
              shape = 21,
              color = "white",
              stroke = 0.6
            ),
            fixed_aes
          )
        )

        # Hide legend when no real grouping
        if (!has_group) {
          p <- p +
            ggplot2$scale_fill_manual(
              values = "steelblue"
            ) +
            ggplot2$guides(fill = "none")
        } else {
          p <- p + ggplot2$labs(fill = "Group")
        }

        # Contribution scales — use matching breaks so
        # ggplot2 merges alpha + size into one legend
        contrib_breaks <- c(0, 0.25, 0.5, 0.75, 1)
        contrib_labels <- c(
          "Low", "", "Med", "", "High"
        )

        # Build shared override.aes for visible keys
        override <- list(
          color = "grey40", stroke = 0.4
        )
        if (!map_alpha) override$alpha <- 1
        if (!map_size) override$size <- 3

        contrib_guide <- ggplot2$guide_legend(
          title = "Contribution",
          override.aes = override
        )

        if (map_alpha) {
          p <- p + ggplot2$scale_alpha_continuous(
            range = c(0.3, 1),
            breaks = contrib_breaks,
            labels = contrib_labels,
            guide = contrib_guide
          )
        }
        if (map_size) {
          p <- p + ggplot2$scale_size_continuous(
            range = c(1, 6),
            breaks = contrib_breaks,
            labels = contrib_labels,
            guide = contrib_guide
          )
        }

        # Override fill legend keys to be visible too
        if (has_group) {
          p <- p + ggplot2$guides(
            fill = ggplot2$guide_legend(
              override.aes = list(
                color = "grey40",
                stroke = 0.4,
                size = 3,
                alpha = 1
              )
            )
          )
        }

        # Ellipses or convex hulls (only when grouped
        # and show_group_shapes is enabled)
        if (has_group && show_group_shapes) {
          if (show_convex_hull) {
            hull_data <- build_hull_data(ind_data)
            if (!is.null(hull_data) && nrow(hull_data) > 0) {
              p <- p + ggplot2$geom_polygon(
                data = hull_data,
                ggplot2$aes(
                  x = x, y = y,
                  fill = group, group = group
                ),
                alpha = 0.15,
                show.legend = FALSE
              )
            }
          } else {
            # stat_ellipse needs >= 4 points per group
            group_counts <- table(ind_data$group)
            valid_groups <- names(
              group_counts[group_counts >= 4]
            )
            if (length(valid_groups) > 0) {
              ellipse_data <- ind_data[
                ind_data$group %in% valid_groups, ,
                drop = FALSE
              ]
              skipped <- setdiff(
                names(group_counts), valid_groups
              )
              if (length(skipped) > 0) {
                rhino$log$warn(
                  "Biplot: skipping ellipse for groups",
                  " with < 4 points: ",
                  paste(skipped, collapse = ", ")
                )
              }
              p <- p + ggplot2$stat_ellipse(
                data = ellipse_data,
                ggplot2$aes(
                  x = x, y = y, colour = group
                ),
                level = 0.95,
                show.legend = FALSE
              )
              # Sync ellipse colour scale with fill scale
              p <- p + ggplot2$scale_color_discrete(
                guide = "none"
              )
            }
          }
        }
      }

      # Variable layers
      if (show_var && !is.null(var_data)) {
        # Unit circle (only in pure variables mode)
        if (layer == "variables") {
          circle_data <- build_circle_data()
          p <- p + ggplot2$geom_path(
            data = circle_data,
            ggplot2$aes(x = x, y = y),
            color = "grey70",
            linetype = "dashed"
          )
        }

        # Arrows from origin
        p <- p + ggiraph$geom_segment_interactive(
          data = var_data,
          ggplot2$aes(
            x = 0, y = 0,
            xend = xend, yend = yend,
            tooltip = tooltip,
            data_id = data_id
          ),
          arrow = ggplot2$arrow(
            length = ggplot2$unit(0.2, "cm"),
            type = "closed"
          ),
          color = "firebrick",
          linewidth = 0.6
        )

        # Variable labels
        p <- p + ggplot2$geom_text(
          data = var_data,
          ggplot2$aes(
            x = xend * 1.08,
            y = yend * 1.08,
            label = label
          ),
          color = "firebrick",
          size = 3.2,
          fontface = "bold"
        )
      }

      p
    },
    operation_name = "Biplot",
    context = error_context,
    error_parser = biplot_error_parser
  )
}

#' Error parser for biplot-specific errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
biplot_error_parser <- function(error_msg,
                                operation_name = "Biplot") {
  if (grepl(
    "dimension|dim_x|dim_y|not found",
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
  } else if (grepl("layer", error_msg, ignore.case = TRUE)) {
    paste0(
      operation_name,
      ": Invalid layer selection."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Validate biplot inputs
validate_biplot_inputs <- function(pca_result, dim_x, dim_y,
                                   layer) {
  if (is.null(pca_result)) {
    stop("pca_result is NULL")
  }

  valid_layers <- c("individuals", "variables", "combined")
  if (!layer %in% valid_layers) {
    stop(paste(
      "Invalid layer:", layer,
      "— must be one of:",
      paste(valid_layers, collapse = ", ")
    ))
  }

  available_dims <- colnames(pca_result$var$coord)
  if (!dim_x %in% available_dims) {
    stop(paste("Dimension not found:", dim_x))
  }
  if (!dim_y %in% available_dims) {
    stop(paste("Dimension not found:", dim_y))
  }
}

#' Build individual plot data
build_ind_plot_data <- function(pca_result, dim_x, dim_y,
                                group_cols, point_alpha,
                                point_size) {
  coord <- pca_result$ind$coord
  contrib <- pca_result$ind$contrib
  meta <- pca_result$ind$meta

  df <- data.frame(
    x = coord[, dim_x],
    y = coord[, dim_y],
    label = rownames(coord),
    stringsAsFactors = FALSE
  )

  # Contribution values for dim_x (used for mapping)
  contrib_vals <- contrib[, dim_x]
  # Rescale to [0, 1] for mapping
  contrib_range <- range(contrib_vals, na.rm = TRUE)
  if (contrib_range[2] > contrib_range[1]) {
    contrib_scaled <- (contrib_vals - contrib_range[1]) /
      (contrib_range[2] - contrib_range[1])
  } else {
    contrib_scaled <- rep(0.5, length(contrib_vals))
  }

  # Store normalised contribution for alpha/size mapping
  if (identical(point_alpha, "Contribution") ||
      identical(point_size, "Contribution")) {
    df$contrib <- contrib_scaled
  }

  # Group column(s) — use interaction() for multi-level designs
  if (!is.null(group_cols) && length(group_cols) > 0 &&
      !is.null(meta)) {
    valid_cols <- intersect(group_cols, names(meta))
    if (length(valid_cols) == 1) {
      df$group <- as.factor(meta[[valid_cols]])
    } else if (length(valid_cols) > 1) {
      df$group <- interaction(
        meta[, valid_cols, drop = FALSE],
        sep = " / ", drop = TRUE
      )
    }
  }

  # Tooltip
  tooltip_parts <- sprintf(
    "<b>%s</b><br/>%s: %.3f<br/>%s: %.3f<br/>Contrib: %.2f%%",
    df$label, dim_x, df$x, dim_y, df$y, contrib_vals
  )
  if ("group" %in% names(df)) {
    tooltip_parts <- paste0(
      tooltip_parts,
      "<br/>Group: ", as.character(df$group)
    )
  }
  df$tooltip <- tooltip_parts
  df$data_id <- paste0("ind_", seq_len(nrow(df)))

  df
}

#' Build variable plot data
build_var_plot_data <- function(pca_result, dim_x, dim_y) {
  coord <- pca_result$var$coord
  contrib <- pca_result$var$contrib

  df <- data.frame(
    xend = coord[, dim_x],
    yend = coord[, dim_y],
    label = rownames(coord),
    stringsAsFactors = FALSE
  )

  contrib_x <- contrib[, dim_x]
  contrib_y <- contrib[, dim_y]

  df$tooltip <- sprintf(
    paste0(
      "<b>%s</b><br/>",
      "%s: %.3f<br/>%s: %.3f<br/>",
      "Contrib %s: %.2f%%<br/>Contrib %s: %.2f%%"
    ),
    df$label,
    dim_x, df$xend, dim_y, df$yend,
    dim_x, contrib_x, dim_y, contrib_y
  )
  df$data_id <- paste0("var_", seq_len(nrow(df)))

  df
}

#' Build convex hull data for grouped individuals
build_hull_data <- function(ind_data) {
  if (!"group" %in% names(ind_data)) return(NULL)

  groups <- levels(ind_data$group)
  hull_list <- lapply(groups, function(g) {
    sub <- ind_data[ind_data$group == g, , drop = FALSE]
    # Remove non-finite coordinates
    sub <- sub[
      is.finite(sub$x) & is.finite(sub$y), ,
      drop = FALSE
    ]
    if (nrow(sub) < 3) return(NULL)
    hull_idx <- grDevices$chull(sub$x, sub$y)
    # Close the polygon
    hull_idx <- c(hull_idx, hull_idx[1])
    data.frame(
      x = sub$x[hull_idx],
      y = sub$y[hull_idx],
      group = g,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, hull_list)
  if (!is.null(result)) {
    result$group <- as.factor(result$group)
  }
  result
}

#' Build unit circle data
build_circle_data <- function(n = 100) {
  theta <- seq(0, 2 * pi, length.out = n)
  data.frame(
    x = cos(theta),
    y = sin(theta)
  )
}

#' Biplot theme
biplot_theme <- function() {
  ggplot2$theme_minimal() +
    ggplot2$theme(
      plot.title = ggplot2$element_text(
        hjust = 0.5, size = 14, face = "bold"
      ),
      aspect.ratio = 1,
      axis.title = ggplot2$element_text(size = 12),
      axis.text = ggplot2$element_text(size = 10),
      legend.position = "right",
      legend.box = "vertical",
      legend.title = ggplot2$element_text(size = 11),
      legend.text = ggplot2$element_text(size = 10),
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
