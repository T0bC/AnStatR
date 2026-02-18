box::use(
  grDevices,
  heatmaply,
  plotly,
  rhino,
  stats,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for cluster heatmap visualization
# No Shiny dependencies allowed in this file.
# =============================================================================

# Palette matching cluster_results.R cluster_color()
CLUSTER_PALETTE <- c(
  "#0d6efd", "#198754", "#dc3545", "#fd7e14",
  "#6f42c1", "#20c997", "#d63384", "#0dcaf0",
  "#6610f2", "#ffc107"
)

SERIATION_CHOICES <- c(
  "OLO" = "OLO",
  "GW" = "GW",
  "mean" = "mean",
  "none" = "none"
)

#' Create an interactive cluster heatmap using heatmaply
#'
#' Builds a plotly-based heatmap with dendrograms on both
#' axes. Only works for hierarchical clustering results
#' (variant == "hclust").
#'
#' @param cluster_result Result list from run_clustering()
#'   (the $result field, not the wrapper)
#' @param data Data frame used for clustering (scaled)
#' @param measure_cols Character vector of measurement columns
#' @param show_labels Logical, show row labels
#' @param custom_labels Character vector of row labels
#'   matching the data row order. If NULL, row numbers used.
#' @param seriation Character, seriation method for leaf
#'   ordering. One of "OLO", "GW", "mean", "none".
#' @param row_side_colors_df Data frame of metadata columns
#'   to display as colored side bars. NULL for none.
#' @param scale_heatmap Character, scaling for heatmaply.
#'   One of "none", "row", "column".
#' @return List with $success, $result (plotly) or $error
#' @export
create_cluster_heatmap <- function(
    cluster_result,
    data,
    measure_cols,
    show_labels = FALSE,
    custom_labels = NULL,
    seriation = "OLO",
    row_side_colors_df = NULL,
    scale_heatmap = "none") {
  if (is.null(cluster_result)) {
    return(list(
      success = FALSE,
      error = error_handling$simple_error(
        message = "No cluster results available.",
        operation_name = "Cluster Heatmap"
      )
    ))
  }

  if (is.null(data) || !is.data.frame(data)) {
    return(list(
      success = FALSE,
      error = error_handling$simple_error(
        message = "No data available for heatmap.",
        operation_name = "Cluster Heatmap"
      )
    ))
  }

  error_handling$safe_execute(
    expr = {
      variant <- cluster_result$details$variant

      # Use the clustering's own dendrogram when
      # available (hclust), otherwise compute one
      # independently from the data.
      dist_method <- cluster_result$details$metric %||%
        cluster_result$details$db_metric %||%
        "euclidean"
      hclust_method <- cluster_result$details$method %||%
        "ward.D2"
      dist_mat_rows <- stats$dist(
        as.matrix(data[, measure_cols, drop = FALSE]),
        method = dist_method
      )
      hc <- if (variant == "hclust" &&
                !is.null(
                  cluster_result$details$hclust_obj
                )) {
        cluster_result$details$hclust_obj
      } else {
        stats$hclust(dist_mat_rows, method = hclust_method)
      }

      # Build numeric matrix from measurement columns
      num_mat <- as.matrix(
        data[, measure_cols, drop = FALSE]
      )

      # Apply custom row labels
      if (!is.null(custom_labels) &&
          length(custom_labels) == nrow(num_mat)) {
        rownames(num_mat) <- custom_labels
      } else if (!show_labels) {
        rownames(num_mat) <- NULL
      }

      # Resolve seriation method
      seriate_method <- if (
        seriation %in% names(SERIATION_CHOICES)
      ) {
        seriation
      } else {
        "OLO"
      }

      # Build color palette
      n_clusters <- cluster_result$n_clusters
      n_colors <- min(n_clusters, length(CLUSTER_PALETTE))
      side_colors <- CLUSTER_PALETTE[seq_len(n_colors)]

      # Row side colors: always include Cluster assignment
      cluster_vec <- factor(
        paste0("C", cluster_result$clusters),
        levels = paste0(
          "C", seq_len(n_clusters)
        )
      )
      rsc <- data.frame(Cluster = cluster_vec)

      if (!is.null(row_side_colors_df) &&
          ncol(row_side_colors_df) > 0) {
        for (col_name in names(row_side_colors_df)) {
          rsc[[col_name]] <- factor(
            row_side_colors_df[[col_name]]
          )
        }
      }

      # Build hclust for columns (use same method)
      col_dist <- stats$dist(
        t(num_mat), method = dist_method
      )
      col_hc <- stats$hclust(
        col_dist, method = hclust_method
      )

      # heatmaply arguments
      hm_args <- list(
        x = num_mat,
        Rowv = stats$as.dendrogram(hc),
        Colv = stats$as.dendrogram(col_hc),
        scale = scale_heatmap,
        seriate = seriate_method,
        row_side_colors = rsc,
        showticklabels = c(show_labels, TRUE),
        fontsize_row = 8,
        fontsize_col = 10,
        margins = c(80, 80, 40, 40),
        colorbar_len = 0.3,
        plot_method = "plotly"
      )

      # Build the heatmap
      p <- do.call(heatmaply$heatmaply, hm_args)

      # Align row annotation legend with the value
      # colorbar by placing it below the colorbar
      p <- plotly$layout(
        p,
        legend = list(
          orientation = "v",
          yanchor = "top",
          y = 0.5,
          xanchor = "left",
          x = 1.03
        )
      )

      rhino$log$info(
        "Cluster Heatmap: created ",
        "(k={n_clusters}, seriation={seriate_method}, ",
        "labels={show_labels}, ",
        "side_colors={ncol(rsc)})"
      )

      p
    },
    operation_name = "Cluster Heatmap",
    context = list(
      n_clusters = cluster_result$n_clusters,
      n_rows = nrow(data),
      n_measure_cols = length(measure_cols),
      seriation = seriation,
      show_labels = show_labels,
      has_custom_labels = !is.null(custom_labels),
      has_side_colors = !is.null(row_side_colors_df)
    ),
    error_parser = heatmap_error_parser
  )
}

#' Error parser for heatmap errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
heatmap_error_parser <- function(
    error_msg,
    operation_name = "Cluster Heatmap") {
  if (grepl(
    "heatmaply|plotly",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Could not create heatmap. The data or ",
      "clustering result may be incompatible."
    )
  } else if (grepl(
    "dendrogram|hclust|as\\.dendrogram",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Could not build dendrogram from ",
      "clustering result. The hierarchical ",
      "clustering object may be invalid."
    )
  } else if (grepl(
    "color|colour|palette|side",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Error applying row side colors. ",
      "Check that selected metadata columns ",
      "have valid values."
    )
  } else if (grepl(
    "seriat|OLO|GW",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Seriation method failed. Try a ",
      "different ordering option."
    )
  } else if (grepl(
    "dist|distance|matrix",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Distance computation failed. The data ",
      "may contain invalid values."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}
