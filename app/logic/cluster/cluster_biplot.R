box::use(
  ggiraph,
  ggplot2,
  grDevices,
  rhino,
)

box::use(
  app/logic/cluster/cluster[
    CLUSTER_PALETTE,
    cluster_color,
  ],
  app/logic/error_handling,
  app/logic/pca/biplot[create_biplot],
  app/logic/pca/pca[run_pca],
)

# =============================================================================
# Pure logic functions for Cluster Biplot
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Create a cluster biplot with convex-hull polygons
#'
#' Projects clustered data onto reduced dimensions (currently PCA)
#' and overlays convex-hull polygons per cluster. Points are colored
#' by user-selected metadata groups (via the PCA biplot), while
#' polygons are colored by cluster assignment.
#'
#' @param data Data frame (already cleaned and scaled)
#' @param measure_cols Character vector of measurement column names
#' @param clusters Integer vector of cluster assignments
#' @param meta_cols Character vector of metadata column names
#' @param dim_x Character, dimension for x-axis (e.g. "Dim.1")
#' @param dim_y Character, dimension for y-axis (e.g. "Dim.2")
#' @param group_cols Character vector of metadata columns for
#'   point coloring. NULL for no grouping.
#' @param reduction_method Character, dimensionality reduction
#'   method. Currently only "pca" is implemented.
#' @param show_title Logical, whether to show the plot title
#' @return List with $success, $result (ggplot), $pca_result,
#'   or $error
#' @export
create_cluster_biplot <- function(data, measure_cols,
                                   clusters,
                                   meta_cols = character(0),
                                   dim_x = "Dim.1",
                                   dim_y = "Dim.2",
                                   group_cols = NULL,
                                   show_convex_hull = FALSE,
                                   show_group_shapes = FALSE,
                                   point_alpha = 1,
                                   point_size = 3,
                                   reduction_method = "pca",
                                   show_title = TRUE) {
  error_context <- list(
    dim_x = dim_x,
    dim_y = dim_y,
    reduction_method = reduction_method,
    n_clusters = length(unique(clusters[clusters > 0])),
    group_cols = paste(
      group_cols %||% "none", collapse = ", "
    )
  )

  # Guard: only PCA and raw are implemented
  if (!reduction_method %in% c("pca", "raw")) {
    return(error_handling$simple_error(
      message = paste0(
        "Reduction method '", reduction_method,
        "' is not implemented yet. ",
        "Please select PCA or Raw Data."
      ),
      operation_name = "Cluster Biplot",
      context = error_context
    ))
  }

  error_handling$safe_execute(
    expr = {
      if (reduction_method == "pca") {
        p <- build_pca_biplot(
          data, measure_cols, meta_cols,
          clusters, dim_x, dim_y,
          group_cols, show_convex_hull,
          show_group_shapes,
          point_alpha, point_size, show_title
        )
      } else {
        p <- build_raw_biplot(
          data, measure_cols, meta_cols,
          clusters, dim_x, dim_y,
          group_cols, show_convex_hull,
          show_group_shapes,
          point_alpha, point_size, show_title
        )
      }

      rhino$log$info(
        "Cluster Biplot: complete ",
        "({dim_x} vs {dim_y}, ",
        "method={reduction_method}, ",
        "{length(unique(clusters[clusters > 0]))}",
        " clusters)"
      )

      list(plot = p)
    },
    operation_name = "Cluster Biplot",
    context = error_context,
    error_parser = cluster_biplot_error_parser
  )
}

#' Error parser for cluster biplot errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
cluster_biplot_error_parser <- function(
    error_msg,
    operation_name = "Cluster Biplot") {
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
      ": No data available for projection.",
      " Please run clustering first."
    )
  } else if (grepl(
    "not implemented",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": ", error_msg
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Build the PCA-based biplot with cluster overlays
build_pca_biplot <- function(data, measure_cols,
                              meta_cols, clusters,
                              dim_x, dim_y,
                              group_cols,
                              show_convex_hull,
                              show_group_shapes,
                              point_alpha, point_size,
                              show_title) {
  pca_res <- run_pca(
    data, measure_cols,
    meta_cols = meta_cols
  )
  if (!pca_res$success) {
    stop(pca_res$error$message)
  }
  pca_result <- pca_res$result

  biplot_res <- create_biplot(
    pca_result = pca_result,
    dim_x = dim_x,
    dim_y = dim_y,
    layer = "individuals",
    group_cols = group_cols,
    show_convex_hull = show_convex_hull,
    show_group_shapes = show_group_shapes,
    point_alpha = point_alpha,
    point_size = point_size,
    show_title = show_title
  )
  if (!biplot_res$success) {
    stop(biplot_res$error$message)
  }
  p <- biplot_res$result

  if (show_title) {
    p <- p + ggplot2$ggtitle(
      "Cluster Biplot (PCA Projection)"
    )
  }

  ind_coord <- pca_result$ind$coord
  add_cluster_overlays(
    p, ind_coord, clusters, dim_x, dim_y
  )
}

#' Build the raw-data scatter plot with cluster overlays
build_raw_biplot <- function(data, measure_cols,
                              meta_cols, clusters,
                              dim_x, dim_y,
                              group_cols,
                              show_convex_hull,
                              show_group_shapes,
                              point_alpha, point_size,
                              show_title) {
  # Validate that dim_x and dim_y are actual columns
  if (!dim_x %in% names(data)) {
    stop(paste0(
      "Column '", dim_x,
      "' not found in the data."
    ))
  }
  if (!dim_y %in% names(data)) {
    stop(paste0(
      "Column '", dim_y,
      "' not found in the data."
    ))
  }

  # Build plot data frame
  plot_df <- data.frame(
    x = data[[dim_x]],
    y = data[[dim_y]],
    stringsAsFactors = FALSE
  )

  # Resolve group column for point coloring
  has_group <- !is.null(group_cols) &&
    length(group_cols) > 0
  if (has_group) {
    # Combine multiple group cols into one label
    group_vals <- lapply(group_cols, function(gc) {
      if (gc %in% names(data)) {
        as.character(data[[gc]])
      } else {
        rep("NA", nrow(data))
      }
    })
    if (length(group_vals) == 1) {
      plot_df$group <- group_vals[[1]]
    } else {
      plot_df$group <- do.call(
        paste, c(group_vals, sep = " | ")
      )
    }
    plot_df$group <- as.factor(plot_df$group)
  }

  # Build tooltip
  plot_df$tooltip <- paste0(
    dim_x, ": ",
    round(plot_df$x, 3), "<br>",
    dim_y, ": ",
    round(plot_df$y, 3)
  )
  if (has_group) {
    plot_df$tooltip <- paste0(
      plot_df$tooltip, "<br>Group: ",
      plot_df$group
    )
  }

  # Resolve alpha and size
  alpha_val <- if (is.character(point_alpha) &&
      point_alpha == "Contribution") {
    0.7
  } else {
    as.numeric(point_alpha)
  }
  size_val <- if (is.character(point_size) &&
      point_size == "Contribution") {
    3
  } else {
    as.numeric(point_size)
  }

  # Build ggplot
  if (has_group) {
    p <- ggplot2$ggplot(
      plot_df,
      ggplot2$aes(x = x, y = y)
    ) +
      ggiraph$geom_point_interactive(
        ggplot2$aes(
          fill = group,
          tooltip = tooltip,
          data_id = tooltip
        ),
        shape = 21,
        colour = "grey30",
        alpha = alpha_val,
        size = size_val,
        stroke = 0.3
      ) +
      ggplot2$labs(fill = "Group")

    # Add ellipses or convex hulls for groups
    if (show_group_shapes) {
      if (show_convex_hull) {
        hull_grp <- build_group_hull_data(
          plot_df, "group"
        )
        if (!is.null(hull_grp) &&
            nrow(hull_grp) > 0) {
          p <- p + ggplot2$geom_polygon(
            data = hull_grp,
            ggplot2$aes(
              x = x, y = y,
              fill = group_label
            ),
            alpha = 0.1,
            show.legend = FALSE
          )
        }
      } else {
        # stat_ellipse needs >= 4 points per group
        group_counts <- table(plot_df$group)
        valid_groups <- names(
          group_counts[group_counts >= 4]
        )
        if (length(valid_groups) > 0) {
          ellipse_data <- plot_df[
            plot_df$group %in% valid_groups, ,
            drop = FALSE
          ]
          p <- p + ggplot2$stat_ellipse(
            data = ellipse_data,
            ggplot2$aes(
              x = x, y = y,
              colour = group
            ),
            level = 0.95,
            show.legend = FALSE
          )
        }
      }
    }
  } else {
    p <- ggplot2$ggplot(
      plot_df,
      ggplot2$aes(x = x, y = y)
    ) +
      ggiraph$geom_point_interactive(
        ggplot2$aes(
          tooltip = tooltip,
          data_id = tooltip
        ),
        shape = 21,
        fill = "steelblue",
        colour = "grey30",
        alpha = alpha_val,
        size = size_val,
        stroke = 0.3
      )
  }

  p <- p +
    ggplot2$labs(
      x = dim_x,
      y = dim_y
    ) +
    ggplot2$theme_minimal() +
    ggplot2$theme(
      panel.grid.minor = ggplot2$element_blank()
    )

  if (show_title) {
    p <- p + ggplot2$ggtitle(
      paste0(
        "Cluster Scatter (",
        dim_x, " vs ", dim_y, ")"
      )
    )
  }

  # Build coordinate matrix for cluster overlays
  ind_coord <- as.matrix(
    data.frame(x = plot_df$x, y = plot_df$y)
  )
  colnames(ind_coord) <- c(dim_x, dim_y)

  add_cluster_overlays(
    p, ind_coord, clusters, dim_x, dim_y
  )
}

#' Add cluster polygon overlays and centroid labels
add_cluster_overlays <- function(p, ind_coord,
                                  clusters,
                                  dim_x, dim_y) {
  hull_data <- build_cluster_hull_data(
    ind_coord, clusters, dim_x, dim_y
  )

  if (!is.null(hull_data) && nrow(hull_data) > 0) {
    # Build named color vector from shared palette
    cluster_levels <- levels(hull_data$cluster_label)
    n_cl <- length(cluster_levels)
    cl_colors <- CLUSTER_PALETTE[
      seq_len(min(n_cl, length(CLUSTER_PALETTE)))
    ]
    if (n_cl > length(CLUSTER_PALETTE)) {
      cl_colors <- rep_len(
        CLUSTER_PALETTE, n_cl
      )
    }
    names(cl_colors) <- cluster_levels

    p <- p + ggplot2$geom_polygon(
      data = hull_data,
      ggplot2$aes(
        x = x, y = y,
        colour = cluster_label,
        group = cluster_label
      ),
      fill = NA,
      linewidth = 1.0,
      linetype = "dashed",
      alpha = 0.8,
      show.legend = TRUE
    )

    centroid_data <- build_cluster_centroids(
      ind_coord, clusters, dim_x, dim_y
    )
    if (!is.null(centroid_data) &&
        nrow(centroid_data) > 0) {
      p <- p + ggplot2$geom_label(
        data = centroid_data,
        ggplot2$aes(
          x = x, y = y,
          label = cluster_label,
          colour = cluster_label
        ),
        fill = "white",
        alpha = 0.8,
        size = 3.5,
        fontface = "bold",
        label.padding = ggplot2$unit(
          0.2, "lines"
        ),
        show.legend = FALSE
      )
    }

    p <- p +
      ggplot2$scale_colour_manual(
        values = cl_colors,
        name = "Cluster"
      ) +
      ggplot2$labs(colour = "Cluster")
  }

  p
}

#' Build convex hull data for group coloring
#' (used in raw data mode)
build_group_hull_data <- function(plot_df,
                                   group_col) {
  groups <- unique(plot_df[[group_col]])
  hull_list <- lapply(groups, function(g) {
    sub <- plot_df[plot_df[[group_col]] == g, ]
    sub <- sub[
      is.finite(sub$x) & is.finite(sub$y), ,
      drop = FALSE
    ]
    if (nrow(sub) < 3) return(NULL)
    hull_idx <- grDevices$chull(sub$x, sub$y)
    hull_idx <- c(hull_idx, hull_idx[1])
    data.frame(
      x = sub$x[hull_idx],
      y = sub$y[hull_idx],
      group_label = g,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, hull_list)
  if (!is.null(result)) {
    result$group_label <- as.factor(
      result$group_label
    )
  }
  result
}

#' Build convex hull data for each cluster
#'
#' @param ind_coord Matrix of individual coordinates
#'   from PCA result
#' @param clusters Integer vector of cluster assignments
#' @param dim_x Character, x dimension name
#' @param dim_y Character, y dimension name
#' @return Data frame with x, y, cluster_label columns
#'   or NULL if insufficient data
build_cluster_hull_data <- function(ind_coord, clusters,
                                     dim_x, dim_y) {
  # Only use non-noise points (cluster > 0)
  valid_mask <- clusters > 0
  valid_clusters <- clusters[valid_mask]
  valid_coord <- ind_coord[valid_mask, , drop = FALSE]

  cluster_ids <- sort(unique(valid_clusters))

  hull_list <- lapply(cluster_ids, function(k) {
    idx <- valid_clusters == k
    sub_x <- valid_coord[idx, dim_x]
    sub_y <- valid_coord[idx, dim_y]
    # Remove non-finite coordinates
    finite_mask <- is.finite(sub_x) &
      is.finite(sub_y)
    sub_x <- sub_x[finite_mask]
    sub_y <- sub_y[finite_mask]
    if (length(sub_x) < 3) return(NULL)

    hull_idx <- grDevices$chull(sub_x, sub_y)
    # Close the polygon
    hull_idx <- c(hull_idx, hull_idx[1])
    data.frame(
      x = sub_x[hull_idx],
      y = sub_y[hull_idx],
      cluster_label = paste("Cluster", k),
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, hull_list)
  if (!is.null(result)) {
    result$cluster_label <- as.factor(
      result$cluster_label
    )
  }
  result
}

#' Build cluster centroid positions for labels
#'
#' @param ind_coord Matrix of individual coordinates
#' @param clusters Integer vector of cluster assignments
#' @param dim_x Character, x dimension name
#' @param dim_y Character, y dimension name
#' @return Data frame with x, y, cluster_label columns
build_cluster_centroids <- function(ind_coord, clusters,
                                     dim_x, dim_y) {
  valid_mask <- clusters > 0
  valid_clusters <- clusters[valid_mask]
  valid_coord <- ind_coord[valid_mask, , drop = FALSE]

  cluster_ids <- sort(unique(valid_clusters))

  centroid_list <- lapply(cluster_ids, function(k) {
    idx <- valid_clusters == k
    data.frame(
      x = mean(valid_coord[idx, dim_x]),
      y = mean(valid_coord[idx, dim_y]),
      cluster_label = paste("Cluster", k),
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, centroid_list)
  if (!is.null(result)) {
    result$cluster_label <- as.factor(
      result$cluster_label
    )
  }
  result
}
