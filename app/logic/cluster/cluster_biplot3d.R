box::use(
  plotly,
  rhino,
  scales,
)

box::use(
  app/logic/shared/error_handling,
  app/logic/pca/pca[run_pca],
)

# =============================================================================
# Pure logic functions for 3D Cluster visualization
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Create a 3D cluster plot using plotly
#'
#' Builds an interactive 3D scatter plot of clustered data.
#' Points are colored by cluster assignment. Supports both
#' PCA projection and raw data visualization.
#'
#' @param data Data frame (already cleaned and scaled)
#' @param measure_cols Character vector of measurement column names
#' @param clusters Integer vector of cluster assignments
#' @param meta_cols Character vector of metadata column names
#' @param dim_x Character, dimension for x-axis (e.g. "Dim.1" or column name)
#' @param dim_y Character, dimension for y-axis
#' @param dim_z Character, dimension for z-axis
#' @param group_cols Character vector for point coloring (NULL = use clusters)
#' @param reduction_method Character, "pca" or "raw"
#' @return List with $success, $result (plotly) or $error
#' @export
create_cluster_biplot3d <- function(data,
                                    measure_cols,
                                    clusters,
                                    meta_cols = character(0),
                                    dim_x = "Dim.1",
                                    dim_y = "Dim.2",
                                    dim_z = "Dim.3",
                                    group_cols = NULL,
                                    reduction_method = "pca") {
  error_context <- list(
    dim_x = dim_x,
    dim_y = dim_y,
    dim_z = dim_z,
    reduction_method = reduction_method,
    n_clusters = length(unique(clusters[clusters > 0])),
    n_measure_cols = length(measure_cols)
  )

  # Guard: only PCA and raw are implemented
  if (!reduction_method %in% c("pca", "raw")) {
    return(error_handling$simple_error(
      message = paste0(
        "Reduction method '", reduction_method,
        "' is not supported for 3D plot. "
      ),
      operation_name = "3D Cluster Plot",
      context = error_context
    ))
  }

  error_handling$safe_execute(
    expr = {
      if (reduction_method == "pca") {
        p <- build_pca_3d_plot(
          data, measure_cols, clusters,
          meta_cols, dim_x, dim_y, dim_z,
          group_cols
        )
      } else {
        p <- build_raw_3d_plot(
          data, measure_cols, clusters,
          meta_cols, dim_x, dim_y, dim_z,
          group_cols
        )
      }

      rhino$log$info(
        "3D Cluster Plot: complete ",
        "({dim_x} vs {dim_y} vs {dim_z}, ",
        "method={reduction_method}, ",
        "{length(unique(clusters[clusters > 0]))}",
        " clusters)"
      )

      list(plot = p)
    },
    operation_name = "3D Cluster Plot",
    context = error_context,
    error_parser = cluster_biplot3d_error_parser
  )
}

#' Error parser for 3D cluster plot errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
cluster_biplot3d_error_parser <- function(
    error_msg,
    operation_name = "3D Cluster Plot") {
  if (grepl(
    "at least 3",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Need at least 3 dimensions. "
    )
  } else if (grepl(
    "dimension|dim_|not found",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Invalid dimension selection."
    )
  } else if (grepl(
    "not supported|not implemented",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name, ": ", error_msg
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Build 3D plot with PCA projection
build_pca_3d_plot <- function(data, measure_cols,
                               clusters, meta_cols,
                               dim_x, dim_y, dim_z,
                               group_cols) {
  # Run PCA for projection
  pca_res <- run_pca(
    data, measure_cols,
    meta_cols = meta_cols
  )
  if (!pca_res$success) {
    stop(pca_res$error$message)
  }
  pca_result <- pca_res$result

  # Validate dimensions exist
  available_dims <- colnames(pca_result$ind$coord)
  for (d in c(dim_x, dim_y, dim_z)) {
    if (!d %in% available_dims) {
      stop(paste("Dimension not found:", d))
    }
  }

  # Build coordinate data frame
  clusters_int <- as.integer(as.character(clusters))
  coord <- pca_result$ind$coord
  plot_df <- data.frame(
    x = coord[, dim_x],
    y = coord[, dim_y],
    z = coord[, dim_z],
    cluster = clusters_int,
    stringsAsFactors = FALSE
  )

  # Add metadata for hover info
  meta <- pca_result$ind$meta
  hover_text <- build_3d_hover_text(plot_df, meta, dim_x, dim_y, dim_z)

  # Determine color grouping
  color_by <- resolve_3d_grouping(data, meta, clusters, group_cols)
  plot_df$color_group <- color_by$values
  legend_title <- color_by$title

  # Get eigenvalues for axis labels
  eig <- pca_result$eig
  x_label <- axis_label_3d(dim_x, eig)
  y_label <- axis_label_3d(dim_y, eig)
  z_label <- axis_label_3d(dim_z, eig)

  # Create color palette
  groups <- unique(plot_df$color_group)
  n_groups <- length(groups)
  col_vec <- scales$hue_pal()(n_groups)

  # Build plotly
  fig <- plotly$plot_ly()

  fig <- fig |>
    plotly$add_trace(
      data = plot_df,
      x = ~x,
      y = ~y,
      z = ~z,
      type = "scatter3d",
      mode = "markers",
      opacity = 0.7,
      marker = list(
        size = 6,
        line = list(width = 1, color = "black")
      ),
      color = ~color_group,
      colors = col_vec,
      text = hover_text,
      hoverinfo = "text"
    )

  # Add cluster centroids
  fig <- add_cluster_centroids_3d(fig, plot_df)

  fig <- fig |>
    plotly$layout(
      scene = list(
        xaxis = list(title = x_label),
        yaxis = list(title = y_label),
        zaxis = list(title = z_label)
      ),
      title = "Cluster 3D Plot (PCA Projection)"
    )

  fig
}

#' Build 3D plot with raw data
build_raw_3d_plot <- function(data, measure_cols,
                               clusters, meta_cols,
                               dim_x, dim_y, dim_z,
                               group_cols) {
  # Validate columns exist
  for (col in c(dim_x, dim_y, dim_z)) {
    if (!col %in% names(data)) {
      stop(paste0("Column '", col, "' not found in data."))
    }
  }

  # Build plot data frame
  clusters_int <- as.integer(as.character(clusters))
  plot_df <- data.frame(
    x = data[[dim_x]],
    y = data[[dim_y]],
    z = data[[dim_z]],
    cluster = clusters_int,
    stringsAsFactors = FALSE
  )

  # Add metadata for hover
  meta <- data[, meta_cols, drop = FALSE]
  hover_text <- build_3d_hover_text(plot_df, meta, dim_x, dim_y, dim_z)

  # Determine color grouping
  color_by <- resolve_3d_grouping(data, meta, clusters, group_cols)
  plot_df$color_group <- color_by$values
  legend_title <- color_by$title

  # Create color palette
  groups <- unique(plot_df$color_group)
  n_groups <- length(groups)
  col_vec <- scales$hue_pal()(n_groups)

  # Build plotly
  fig <- plotly$plot_ly()

  fig <- fig |>
    plotly$add_trace(
      data = plot_df,
      x = ~x,
      y = ~y,
      z = ~z,
      type = "scatter3d",
      mode = "markers",
      opacity = 0.7,
      marker = list(
        size = 6,
        line = list(width = 1, color = "black")
      ),
      color = ~color_group,
      colors = col_vec,
      text = hover_text,
      hoverinfo = "text"
    )

  # Add cluster centroids
  fig <- add_cluster_centroids_3d(fig, plot_df)

  fig <- fig |>
    plotly$layout(
      scene = list(
        xaxis = list(title = dim_x),
        yaxis = list(title = dim_y),
        zaxis = list(title = dim_z)
      ),
      title = paste0("Cluster 3D Plot (", dim_x, ", ", dim_y, ", ", dim_z, ")")
    )

  fig
}

#' Resolve grouping for 3D plot coloring
resolve_3d_grouping <- function(data, meta, clusters, group_cols) {
  # Priority: explicit group_cols > CLUSTER > default
  has_group <- !is.null(group_cols) && length(group_cols) > 0
  clusters_int <- as.integer(as.character(clusters))

  if (has_group && "CLUSTER" %in% group_cols) {
    # Use cluster assignments
    return(list(
      values = factor(paste("Cluster", clusters_int)),
      title = "Cluster"
    ))
  } else if (has_group && length(group_cols) > 0) {
    # Use metadata column(s)
    valid_cols <- intersect(group_cols, names(data))
    if (length(valid_cols) == 1) {
      return(list(
        values = as.factor(data[[valid_cols]]),
        title = valid_cols[1]
      ))
    } else if (length(valid_cols) > 1) {
      combined <- interaction(
        data[, valid_cols, drop = FALSE],
        sep = " / ", drop = TRUE
      )
      return(list(values = combined, title = "Group"))
    }
  }

  # Default: color by cluster
  list(
    values = factor(paste("Cluster", clusters_int)),
    title = "Cluster"
  )
}

#' Build hover text for 3D plot
build_3d_hover_text <- function(plot_df, meta,
                                dim_x, dim_y, dim_z) {
  vapply(
    seq_len(nrow(plot_df)),
    function(i) {
      parts <- character(0)

      # Metadata
      if (!is.null(meta) && ncol(meta) > 0) {
        meta_cols <- names(meta)
        # Skip simple row-number metadata
        if (!("Row" %in% meta_cols && ncol(meta) == 1)) {
          for (col in meta_cols) {
            parts <- c(parts, paste0(
              col, ": ",
              as.character(meta[i, col])
            ))
          }
        }
      }

      # Dimension values
      parts <- c(parts, paste0(
        dim_x, ": ",
        sprintf("%.3f", plot_df$x[i])
      ))
      parts <- c(parts, paste0(
        dim_y, ": ",
        sprintf("%.3f", plot_df$y[i])
      ))
      parts <- c(parts, paste0(
        dim_z, ": ",
        sprintf("%.3f", plot_df$z[i])
      ))

      # Cluster
      parts <- c(parts, paste0(
        "Cluster: ", as.character(plot_df$cluster[i])
      ))

      paste(parts, collapse = "<br>")
    },
    character(1)
  )
}

#' Add cluster centroid markers to 3D plot
add_cluster_centroids_3d <- function(fig, plot_df) {
  cluster_ids <- unique(plot_df$cluster[plot_df$cluster > 0])

  centroid_list <- lapply(cluster_ids, function(k) {
    idx <- plot_df$cluster == k
    data.frame(
      x = mean(plot_df$x[idx]),
      y = mean(plot_df$y[idx]),
      z = mean(plot_df$z[idx]),
      cluster_label = paste("Center", k),
      cluster = k,
      stringsAsFactors = FALSE
    )
  })
  centroids <- do.call(rbind, centroid_list)

  if (!is.null(centroids) && nrow(centroids) > 0) {
    fig <- fig |>
      plotly$add_trace(
        data = centroids,
        x = ~x,
        y = ~y,
        z = ~z,
        type = "scatter3d",
        mode = "markers+text",
        marker = list(
          size = 12,
          symbol = "diamond",
          color = "black",
          line = list(width = 2, color = "white")
        ),
        text = ~cluster_label,
        textposition = "top center",
        textfont = list(size = 10, color = "black"),
        showlegend = FALSE,
        hoverinfo = "text",
        hovertext = ~paste0(cluster_label, "<br>",
                           "x: ", sprintf("%.3f", x), "<br>",
                           "y: ", sprintf("%.3f", y), "<br>",
                           "z: ", sprintf("%.3f", z))
      )
  }

  fig
}

#' Build axis label with variance percentage (PCA mode)
axis_label_3d <- function(dim_name, eig) {
  dim_idx <- which(rownames(eig) == dim_name)
  if (length(dim_idx) == 1) {
    var_pct <- eig[dim_idx, "variance.percent"]
    sprintf("%s (%.1f%%)", dim_name, var_pct)
  } else {
    dim_name
  }
}
