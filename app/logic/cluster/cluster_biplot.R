box::use(
  ggplot2,
  grDevices,
  rhino,
)

box::use(
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

  # Guard: only PCA is implemented

  if (reduction_method != "pca") {
    return(error_handling$simple_error(
      message = paste0(
        "Reduction method '", reduction_method,
        "' is not implemented yet. ",
        "Please select PCA."
      ),
      operation_name = "Cluster Biplot",
      context = error_context
    ))
  }

  error_handling$safe_execute(
    expr = {
      # Run PCA on the cluster data
      pca_res <- run_pca(
        data, measure_cols,
        meta_cols = meta_cols
      )
      if (!pca_res$success) {
        stop(pca_res$error$message)
      }
      pca_result <- pca_res$result

      # Build the base biplot (individuals only, no hull)
      biplot_res <- create_biplot(
        pca_result = pca_result,
        dim_x = dim_x,
        dim_y = dim_y,
        layer = "individuals",
        group_cols = group_cols,
        show_convex_hull = FALSE,
        point_alpha = 1,
        point_size = 3,
        show_title = show_title
      )
      if (!biplot_res$success) {
        stop(biplot_res$error$message)
      }
      p <- biplot_res$result

      # Override title for cluster context
      if (show_title) {
        p <- p + ggplot2$ggtitle(
          "Cluster Biplot (PCA Projection)"
        )
      }

      # Build cluster polygon overlay
      ind_coord <- pca_result$ind$coord
      hull_data <- build_cluster_hull_data(
        ind_coord, clusters, dim_x, dim_y
      )

      if (!is.null(hull_data) && nrow(hull_data) > 0) {
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

        # Add cluster labels at hull centroids
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
            label.padding = ggplot2$unit(0.2, "lines"),
            show.legend = FALSE
          )
        }

        p <- p + ggplot2$labs(colour = "Cluster")
      }

      rhino$log$info(
        "Cluster Biplot: complete ",
        "({dim_x} vs {dim_y}, ",
        "{length(unique(clusters[clusters > 0]))} clusters)"
      )

      list(
        plot = p,
        pca_result = pca_result
      )
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
    if (sum(idx) < 3) return(NULL)

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
