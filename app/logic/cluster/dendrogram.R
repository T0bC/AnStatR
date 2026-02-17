box::use(
  dendextend,
  ggplot2,
  rhino,
  stats,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for dendrogram visualization
# No Shiny dependencies allowed in this file.
# =============================================================================

# Palette matching cluster_results.R cluster_color()
CLUSTER_PALETTE <- c(
  "#0d6efd", "#198754", "#dc3545", "#fd7e14",
  "#6f42c1", "#20c997", "#d63384", "#0dcaf0",
  "#6610f2", "#ffc107"
)

#' Create a dendrogram plot from clustering results
#'
#' Builds a ggplot dendrogram with colored branches and
#' labels matching the cluster assignments. Only works for
#' hierarchical clustering results (variant == "hclust").
#'
#' @param cluster_result Result list from run_clustering()
#'   (the $result field, not the wrapper)
#' @param horiz Logical, display dendrogram horizontally
#' @param polar Logical, display in polar coordinates
#' @param show_labels Logical, show leaf labels
#' @return List with $success, $result (ggplot) or $error
#' @export
create_dendrogram_plot <- function(cluster_result,
                                   horiz = FALSE,
                                   polar = FALSE,
                                   show_labels = FALSE) {
  if (is.null(cluster_result)) {
    return(list(
      success = FALSE,
      error = error_handling$simple_error(
        message = "No cluster results available.",
        operation_name = "Dendrogram"
      )
    ))
  }

  variant <- cluster_result$details$variant
  if (variant != "hclust") {
    algo_label <- switch(
      variant,
      kmeans = "K-Means",
      pam    = "K-Means (PAM)",
      dbscan = "DBSCAN",
      variant
    )
    return(list(
      success = FALSE,
      error = error_handling$simple_error(
        message = paste0(
          "Dendrogram visualization is not available ",
          "for ", algo_label, " clustering. ",
          "Dendrograms represent the hierarchical ",
          "merge tree, which is only produced by ",
          "hierarchical clustering algorithms."
        ),
        operation_name = "Dendrogram"
      )
    ))
  }

  error_handling$safe_execute(
    expr = {
      hc <- cluster_result$details$hclust_obj
      n_clusters <- cluster_result$n_clusters

      n_colors <- min(n_clusters, length(CLUSTER_PALETTE))
      colors <- CLUSTER_PALETTE[seq_len(n_colors)]

      dend <- stats$as.dendrogram(hc)

      # Highlight branches: gradient from dark (root)
      # to lighter colors toward leaves
      dend <- dendextend$highlight_branches_col(
        dend
      )
      dend <- dendextend$highlight_branches_lwd(
        dend
      )

      # Color branches and labels by cluster
      dend <- dendextend$set(
        dend, "branches_k_color",
        k = n_clusters, value = colors
      )
      dend <- dendextend$set(
        dend, "labels_colors",
        k = n_clusters, value = colors
      )
      dend <- dendextend$set(
        dend, "labels_cex", 0.6
      )

      p <- ggplot2$ggplot(
        dend,
        horiz = horiz,
        labels = show_labels
      ) +
        ggplot2$scale_x_continuous(
          expand = c(-1, -1)
        )

      if (horiz) {
        p <- p + ggplot2$scale_y_reverse(
          expand = c(1, 1)
        )
      }

      if (polar) {
        p <- p + ggplot2$coord_polar(
          theta = "x", start = 0
        )
      }

      p <- p +
        ggplot2$labs(
          title = "Cluster Dendrogram",
          y = "Height"
        ) +
        ggplot2$theme_minimal() +
        ggplot2$theme(
          plot.title = ggplot2$element_text(
            size = 12, face = "bold"
          ),
          panel.grid.minor =
            ggplot2$element_blank()
        )

      rhino$log$info(
        "Dendrogram: plot created ",
        "(k={n_clusters}, horiz={horiz}, ",
        "polar={polar}, labels={show_labels})"
      )

      p
    },
    operation_name = "Dendrogram",
    context = list(
      n_clusters = cluster_result$n_clusters,
      horiz = horiz,
      polar = polar,
      show_labels = show_labels
    ),
    error_parser = dendrogram_error_parser
  )
}

#' Error parser for dendrogram errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
dendrogram_error_parser <- function(
    error_msg,
    operation_name = "Dendrogram") {
  if (grepl(
    "dendrogram|hclust|as\\.dendrogram",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Could not create dendrogram from ",
      "clustering result. The hierarchical ",
      "clustering object may be invalid."
    )
  } else if (grepl(
    "color|colour|palette",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Error applying cluster colors to ",
      "dendrogram branches."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}
