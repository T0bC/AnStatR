box::use(
  cluster,
  ggiraph,
  ggplot2,
  rhino,
  stats,
)

box::use(
  app/logic/cluster/cluster[CLUSTER_PALETTE],
  app/logic/shared/error_handling,
)

# =============================================================================
# Pure logic functions for Cluster Silhouette Plot
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Compute silhouette data from cluster results
#'
#' Computes per-observation silhouette widths. Noise
#' points (cluster == 0, from DBSCAN) are excluded.
#'
#' @param data Numeric matrix or data frame of
#'   measurement columns used for clustering
#' @param clusters Integer vector of cluster assignments
#' @param metric Character, distance metric used for
#'   clustering ("euclidean" or "manhattan")
#' @return List with $success, $result or $error.
#'   $result contains: sil_df (data frame with
#'   observation, cluster, sil_width), sil_avg,
#'   per_cluster_avg, n_noise
#' @export
compute_silhouette_data <- function(data, clusters,
                                     metric = "euclidean") {
  error_context <- list(
    n_obs = length(clusters),
    n_clusters = length(unique(clusters[clusters > 0])),
    metric = metric
  )

  error_handling$safe_execute(
    expr = {
      valid_mask <- clusters > 0
      valid_clusters <- clusters[valid_mask]
      valid_data <- as.matrix(
        data[valid_mask, , drop = FALSE]
      )
      n_noise <- sum(!valid_mask)
      unique_k <- length(unique(valid_clusters))

      if (unique_k < 2) {
        stop(
          "Silhouette requires at least 2 clusters. ",
          "Found ", unique_k, "."
        )
      }
      if (nrow(valid_data) < 2) {
        stop(
          "Silhouette requires at least 2 ",
          "observations."
        )
      }

      dist_mat <- stats$dist(
        valid_data, method = metric
      )
      sil <- cluster$silhouette(
        valid_clusters, dist_mat
      )

      sil_mat <- sil[, , drop = FALSE]
      sil_df <- data.frame(
        observation = seq_len(nrow(sil_mat)),
        cluster = as.integer(sil_mat[, "cluster"]),
        neighbor = as.integer(sil_mat[, "neighbor"]),
        sil_width = as.numeric(sil_mat[, "sil_width"]),
        stringsAsFactors = FALSE
      )

      # Store original row indices (before noise filter)
      original_idx <- which(valid_mask)
      sil_df$original_index <- original_idx

      sil_avg <- mean(sil_df$sil_width)

      per_cluster_avg <- stats$aggregate(
        sil_width ~ cluster,
        data = sil_df,
        FUN = mean
      )
      names(per_cluster_avg) <- c(
        "cluster", "avg_sil_width"
      )

      rhino$log$info(
        "Silhouette: computed for ",
        "{nrow(sil_df)} observations, ",
        "{unique_k} clusters, ",
        "avg={round(sil_avg, 4)}"
      )

      list(
        sil_df = sil_df,
        sil_avg = sil_avg,
        per_cluster_avg = per_cluster_avg,
        n_noise = n_noise
      )
    },
    operation_name = "Silhouette Computation",
    context = error_context,
    error_parser = silhouette_error_parser
  )
}

#' Create an interactive silhouette plot
#'
#' Builds a ggplot with per-observation silhouette
#' bars, sorted by cluster and then by silhouette
#' width within each cluster (fviz_silhouette style).
#' Optionally overlays metadata grouping via bar
#' border colors.
#'
#' @param sil_data Result from compute_silhouette_data()
#'   (the $result field)
#' @param membership_data Data frame with metadata and
#'   Cluster column (or NULL)
#' @param group_cols Character vector of metadata column
#'   names for border coloring (or NULL)
#' @param sort_by Character, "width" to sort bars by
#'   silhouette width within clusters, "cluster" to
#'   keep original observation order within clusters
#' @param show_avg_line Logical, show dashed horizontal
#'   line for average silhouette width
#' @return List with $success, $result (list with $plot)
#'   or $error
#' @export
create_silhouette_plot <- function(sil_data,
                                    membership_data = NULL,
                                    group_cols = NULL,
                                    sort_by = "width",
                                    show_avg_line = TRUE) {
  error_context <- list(
    n_obs = nrow(sil_data$sil_df),
    sort_by = sort_by,
    has_group = !is.null(group_cols) &&
      length(group_cols) > 0
  )

  error_handling$safe_execute(
    expr = {
      sil_df <- sil_data$sil_df
      sil_avg <- sil_data$sil_avg
      n_noise <- sil_data$n_noise

      # Sort: by cluster, then by sil_width within
      if (sort_by == "width") {
        sil_df <- sil_df[order(
          sil_df$cluster,
          -sil_df$sil_width
        ), ]
      } else {
        sil_df <- sil_df[order(
          sil_df$cluster,
          sil_df$observation
        ), ]
      }
      sil_df$x_order <- seq_len(nrow(sil_df))

      # Create cluster factor for coloring
      sil_df$cluster_label <- factor(
        paste("Cluster", sil_df$cluster),
        levels = paste(
          "Cluster",
          sort(unique(sil_df$cluster))
        )
      )

      # Build named color vector
      cluster_ids <- sort(unique(sil_df$cluster))
      n_cl <- length(cluster_ids)
      cl_colors <- CLUSTER_PALETTE[
        seq_len(min(n_cl, length(CLUSTER_PALETTE)))
      ]
      if (n_cl > length(CLUSTER_PALETTE)) {
        cl_colors <- rep_len(CLUSTER_PALETTE, n_cl)
      }
      names(cl_colors) <- paste("Cluster", cluster_ids)

      # Resolve metadata grouping for border color
      has_group <- !is.null(group_cols) &&
        length(group_cols) > 0 &&
        !is.null(membership_data)
      # Filter out "CLUSTER" from group_cols since
      # cluster is already the fill color
      if (has_group) {
        meta_group_cols <- setdiff(
          group_cols, "CLUSTER"
        )
        has_group <- length(meta_group_cols) > 0
      }

      if (has_group) {
        sil_df <- resolve_metadata_groups(
          sil_df, membership_data, meta_group_cols
        )
      }

      # Build tooltip text
      sil_df$tooltip <- build_silhouette_tooltip(
        sil_df, has_group
      )

      # Build ggplot
      p <- build_silhouette_ggplot(
        sil_df, cl_colors, sil_avg, n_noise,
        has_group, show_avg_line
      )

      rhino$log$info(
        "Silhouette Plot: created ",
        "({nrow(sil_df)} bars, ",
        "{n_cl} clusters)"
      )

      list(plot = p)
    },
    operation_name = "Silhouette Plot",
    context = error_context,
    error_parser = silhouette_error_parser
  )
}

#' Error parser for silhouette errors
#'
#' @param error_msg Character, the original error
#'   message
#' @param operation_name Character, name of the
#'   operation
#' @return Character, user-friendly error message
#' @export
silhouette_error_parser <- function(
    error_msg,
    operation_name = "Silhouette") {
  if (grepl(
    "at least 2 clusters",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Need at least 2 clusters to ",
      "compute silhouette widths."
    )
  } else if (grepl(
    "at least 2 observations",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Not enough observations for ",
      "silhouette computation."
    )
  } else if (grepl(
    "NA|NaN|missing",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Data contains missing values. ",
      "Please handle missing data first."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Resolve metadata groups and add to sil_df
resolve_metadata_groups <- function(sil_df,
                                     membership_data,
                                     meta_group_cols) {
  # membership_data rows correspond to original
  # observations; use original_index to map back
  group_vals <- lapply(meta_group_cols, function(gc) {
    if (gc %in% names(membership_data)) {
      vals <- as.character(
        membership_data[[gc]]
      )
      vals[sil_df$original_index]
    } else {
      rep("NA", nrow(sil_df))
    }
  })

  if (length(group_vals) == 1) {
    sil_df$meta_group <- group_vals[[1]]
  } else {
    sil_df$meta_group <- do.call(
      paste, c(group_vals, sep = " | ")
    )
  }
  sil_df$meta_group <- as.factor(sil_df$meta_group)
  sil_df
}

#' Build tooltip HTML for silhouette bars
build_silhouette_tooltip <- function(sil_df,
                                      has_group) {
  tt <- paste0(
    "<b>Cluster ", sil_df$cluster, "</b><br>",
    "Silhouette width: ",
    round(sil_df$sil_width, 4), "<br>",
    "Nearest neighbor cluster: ",
    sil_df$neighbor
  )
  if (has_group && "meta_group" %in% names(sil_df)) {
    tt <- paste0(
      tt, "<br>Group: ", sil_df$meta_group
    )
  }
  tt
}

#' Build the silhouette ggplot object
build_silhouette_ggplot <- function(sil_df, cl_colors,
                                     sil_avg, n_noise,
                                     has_group,
                                     show_avg_line) {
  # Cluster separator positions
  cluster_breaks <- compute_cluster_breaks(sil_df)

  # Per-cluster average labels
  per_cl_avg <- stats$aggregate(
    sil_width ~ cluster_label,
    data = sil_df,
    FUN = mean
  )

  if (has_group) {
    p <- ggplot2$ggplot(
      sil_df,
      ggplot2$aes(
        x = x_order,
        y = sil_width
      )
    ) +
      ggiraph$geom_bar_interactive(
        ggplot2$aes(
          fill = cluster_label,
          colour = meta_group,
          tooltip = tooltip,
          data_id = x_order
        ),
        stat = "identity",
        width = 1,
        linewidth = 0.4
      ) +
      ggplot2$scale_fill_manual(
        values = cl_colors,
        name = "Cluster"
      ) +
      ggplot2$labs(colour = "Group")
  } else {
    p <- ggplot2$ggplot(
      sil_df,
      ggplot2$aes(
        x = x_order,
        y = sil_width
      )
    ) +
      ggiraph$geom_bar_interactive(
        ggplot2$aes(
          fill = cluster_label,
          tooltip = tooltip,
          data_id = x_order
        ),
        stat = "identity",
        width = 1,
        colour = "white",
        linewidth = 0.1
      ) +
      ggplot2$scale_fill_manual(
        values = cl_colors,
        name = "Cluster"
      )
  }

  # Average silhouette line
  if (show_avg_line) {
    p <- p + ggplot2$geom_hline(
      yintercept = sil_avg,
      linetype = "dashed",
      colour = "red",
      linewidth = 0.6
    ) +
      ggplot2$annotate(
        "text",
        x = nrow(sil_df) * 0.98,
        y = sil_avg,
        label = paste0(
          "Avg: ", round(sil_avg, 3)
        ),
        hjust = 1,
        vjust = -0.5,
        size = 3.2,
        colour = "red",
        fontface = "bold"
      )
  }

  # Cluster separator lines
  if (length(cluster_breaks) > 0) {
    p <- p + ggplot2$geom_vline(
      xintercept = cluster_breaks,
      linetype = "dotted",
      colour = "grey50",
      linewidth = 0.4
    )
  }

  # Subtitle with noise info
  subtitle_text <- paste0(
    "Average silhouette width: ",
    round(sil_avg, 4)
  )
  if (n_noise > 0) {
    subtitle_text <- paste0(
      subtitle_text,
      " (", n_noise,
      " noise points excluded)"
    )
  }

  p <- p +
    ggplot2$labs(
      title = "Cluster Silhouette Plot",
      subtitle = subtitle_text,
      x = "Observations (sorted by cluster)",
      y = "Silhouette Width"
    ) +
    ggplot2$ylim(
      min(-0.05, min(sil_df$sil_width) - 0.02),
      1
    ) +
    ggplot2$theme_minimal() +
    ggplot2$theme(
      axis.text.x = ggplot2$element_blank(),
      axis.ticks.x = ggplot2$element_blank(),
      panel.grid.major.x = ggplot2$element_blank(),
      panel.grid.minor = ggplot2$element_blank(),
      plot.title = ggplot2$element_text(
        face = "bold", size = 13
      ),
      plot.subtitle = ggplot2$element_text(
        size = 10, colour = "grey40"
      ),
      legend.position = "bottom"
    )

  p
}

#' Compute x positions where cluster boundaries occur
compute_cluster_breaks <- function(sil_df) {
  clusters_ordered <- sil_df$cluster[
    order(sil_df$x_order)
  ]
  n <- length(clusters_ordered)
  breaks <- which(
    clusters_ordered[-1] != clusters_ordered[-n]
  )
  breaks + 0.5
}
