box::use(
  cluster,
  dbscan,
  rhino,
  stats,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for Cluster
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Validate clustering inputs before computation
#' @param columns Character vector of selected column names
#' @param data Data frame to validate against
#' @return List with $valid (logical) and $error (app_error or NULL)
#' @export
validate_inputs <- function(columns, data) {
  if (is.null(data) || !is.data.frame(data)) {
    rhino$log$warn("Cluster: data not available for validation")
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = "No data available. Please load data first.",
        operation_name = "cluster_validate_inputs"
      )
    ))
  }

  if (is.null(columns) || length(columns) == 0) {
    rhino$log$warn("Cluster: no columns selected")
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = "Please select at least one column.",
        operation_name = "cluster_validate_inputs"
      )
    ))
  }

  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    rhino$log$warn(
      "Cluster: columns not found: {paste(missing, collapse = ', ')}"
    )
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = paste(
          "Columns not found in data:",
          paste(missing, collapse = ", ")
        ),
        operation_name = "cluster_validate_inputs"
      )
    ))
  }

  list(valid = TRUE, error = NULL)
}

#' Run clustering analysis
#'
#' Dispatches to the appropriate algorithm based on user settings.
#' - kmeans + euclidean: stats$kmeans
#' - kmeans + manhattan: cluster$pam (partitioning around medoids)
#' - hierarchical: stats$dist + stats$hclust + stats$cutree
#' - dbscan: dbscan$dbscan (n_clusters is ignored, eps auto-computed)
#'
#' @param data Data frame (already cleaned and scaled)
#' @param columns Character vector of measurement column names
#' @param n_clusters Integer, number of clusters (ignored for DBSCAN)
#' @param algorithm Character, one of "kmeans", "hierarchical", "dbscan"
#' @param metric Character, distance metric: "euclidean" or "manhattan"
#' @param method Character, linkage method for hierarchical clustering
#'   (e.g. "ward", "single", "complete", "average", "mcquitty",
#'   "median", "centroid"). Ignored for other algorithms.
#' @return List with $success, $result or $error
#' @export
run_clustering <- function(data, columns, n_clusters,
                           algorithm = "kmeans",
                           metric = "euclidean",
                           method = "ward") {
  error_context <- list(
    n_obs = nrow(data),
    n_cols = length(columns),
    n_clusters = n_clusters,
    algorithm = algorithm,
    metric = metric,
    method = method
  )

  error_handling$safe_execute(
    expr = {
      num_data <- as.matrix(
        data[, columns, drop = FALSE]
      )
      validate_clustering_inputs(
        num_data, n_clusters, algorithm
      )

      res <- switch(
        algorithm,
        kmeans = run_kmeans(
          num_data, n_clusters, metric
        ),
        hierarchical = run_hierarchical(
          num_data, n_clusters, metric, method
        ),
        dbscan = run_dbscan(num_data, metric),
        stop(paste0(
          "Unknown algorithm: '", algorithm, "'. ",
          "Supported: kmeans, hierarchical, dbscan."
        ))
      )

      actual_k <- length(unique(
        res$clusters[res$clusters > 0]
      ))

      # Compute shared quality metrics for all algorithms
      shared_stats <- compute_cluster_stats(
        num_data, res$clusters, metric
      )
      res$details <- c(res$details, shared_stats)

      rhino$log$info(
        "Cluster: {algorithm} complete ",
        "({length(columns)} cols, ",
        "k={actual_k}, metric={metric})"
      )

      list(
        clusters = res$clusters,
        n_clusters = actual_k,
        algorithm = algorithm,
        metric = metric,
        method = method,
        columns = columns,
        details = res$details
      )
    },
    operation_name = "Cluster Analysis",
    context = error_context,
    error_parser = cluster_error_parser
  )
}

#' Error parser for clustering errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
cluster_error_parser <- function(
    error_msg,
    operation_name = "Cluster Analysis") {
  if (grepl(
    "constant|variance|zero",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Data contains constant columns with zero ",
      "variance. Consider scaling or removing them."
    )
  } else if (grepl(
    "\\bNA\\b|missing|NaN",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Data contains missing values. ",
      "Please handle missing data first."
    )
  } else if (grepl(
    "observations|rows|enough|too few|at least",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Not enough observations for the requested ",
      "number of clusters."
    )
  } else if (grepl(
    "numeric|non-numeric",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": All selected columns must be numeric."
    )
  } else if (grepl(
    "algorithm|unknown",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name, ": ", error_msg
    )
  } else if (grepl(
    "eps|minPts|no clusters|noise",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": DBSCAN could not find meaningful clusters. ",
      "Try different data scaling or algorithm."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}

# =============================================================================
# Internal helpers (not exported)
# =============================================================================

validate_clustering_inputs <- function(num_data,
                                        n_clusters,
                                        algorithm) {
  if (is.null(num_data) || nrow(num_data) == 0) {
    stop("Data is NULL or empty")
  }

  non_numeric <- !all(
    vapply(
      as.data.frame(num_data),
      is.numeric, logical(1)
    )
  )
  if (non_numeric) {
    stop("All selected columns must be numeric")
  }

  if (algorithm != "dbscan") {
    if (
      is.null(n_clusters) ||
      !is.numeric(n_clusters) ||
      n_clusters < 2
    ) {
      stop("Number of clusters must be at least 2")
    }
    if (n_clusters >= nrow(num_data)) {
      stop(
        "Number of clusters (", n_clusters,
        ") must be less than the number of ",
        "observations (", nrow(num_data), ")"
      )
    }
  }

  invisible(TRUE)
}

#' Compute per-cluster variable means
#'
#' @param num_data Numeric matrix or data frame
#' @param clusters Integer vector of cluster assignments
#' @return List with means matrix, overall_mean, cluster_ids,
#'   n_per_cluster
#' @export
compute_cluster_summary <- function(num_data, clusters) {
  valid_mask <- clusters > 0
  valid_clusters <- clusters[valid_mask]
  valid_data <- num_data[valid_mask, , drop = FALSE]
  col_names <- colnames(valid_data)

  cluster_ids <- sort(unique(valid_clusters))
  means_list <- lapply(cluster_ids, function(k) {
    members <- valid_data[
      valid_clusters == k, , drop = FALSE
    ]
    colMeans(members)
  })
  means_mat <- do.call(rbind, means_list)
  rownames(means_mat) <- paste("Cluster", cluster_ids)
  colnames(means_mat) <- col_names

  # Also compute overall mean for comparison
  overall <- colMeans(valid_data)

  list(
    means = means_mat,
    overall_mean = overall,
    cluster_ids = cluster_ids,
    n_per_cluster = vapply(
      cluster_ids,
      function(k) sum(valid_clusters == k),
      integer(1)
    )
  )
}

compute_cluster_stats <- function(num_data, clusters,
                                   metric) {
  # Only use non-noise points for silhouette
  valid_mask <- clusters > 0
  valid_clusters <- clusters[valid_mask]
  valid_data <- num_data[valid_mask, , drop = FALSE]
  n <- nrow(valid_data)
  unique_k <- length(unique(valid_clusters))

  # Silhouette (needs >= 2 clusters and >= 2 points)
  sil_avg <- NA_real_
  if (unique_k >= 2 && n >= 2) {
    dist_mat <- stats$dist(valid_data, method = metric)
    sil <- cluster$silhouette(valid_clusters, dist_mat)
    sil_avg <- mean(sil[, "sil_width"])
  }

  # BSS / TSS and within-SS from data + assignments
  grand_center <- colMeans(valid_data)
  totss <- sum(
    sweep(valid_data, 2, grand_center)^2
  )
  withinss <- 0
  cluster_sizes <- integer(0)
  for (k in sort(unique(valid_clusters))) {
    members <- valid_data[
      valid_clusters == k, , drop = FALSE
    ]
    center_k <- colMeans(members)
    withinss <- withinss + sum(
      sweep(members, 2, center_k)^2
    )
    cluster_sizes <- c(
      cluster_sizes,
      stats$setNames(nrow(members), k)
    )
  }
  betweenss <- totss - withinss
  bss_tss <- if (totss > 0) {
    betweenss / totss
  } else {
    NA_real_
  }

  list(
    silhouette_avg = sil_avg,
    tot_withinss = withinss,
    betweenss = betweenss,
    totss = totss,
    bss_tss = bss_tss,
    size = as.integer(cluster_sizes)
  )
}

run_kmeans <- function(num_data, n_clusters, metric) {
  if (metric == "manhattan") {
    # PAM (partitioning around medoids) supports manhattan
    pam_res <- cluster$pam(
      num_data,
      k = n_clusters,
      metric = "manhattan",
      nstart = 10
    )
    list(
      clusters = pam_res$clustering,
      details = list(
        variant = "pam",
        medoids = pam_res$medoids,
        objective = pam_res$objective
      )
    )
  } else {
    km_res <- stats$kmeans(
      num_data,
      centers = n_clusters,
      nstart = 25
    )
    list(
      clusters = km_res$cluster,
      details = list(
        variant = "kmeans",
        centers = km_res$centers
      )
    )
  }
}

run_hierarchical <- function(num_data, n_clusters,
                              metric, method) {
  # Ward's method requires squared euclidean distances
  hclust_method <- if (method == "ward") {
    "ward.D2"
  } else {
    method
  }

  dist_matrix <- stats$dist(num_data, method = metric)
  hc <- stats$hclust(dist_matrix, method = hclust_method)
  clusters <- stats$cutree(hc, k = n_clusters)

  list(
    clusters = clusters,
    details = list(
      variant = "hclust",
      hclust_obj = hc,
      method = hclust_method,
      metric = metric,
      height = hc$height,
      merge = hc$merge
    )
  )
}

run_dbscan <- function(num_data, metric) {
  dist_matrix <- stats$dist(num_data, method = metric)

  # Auto-compute eps using k-nearest neighbor distances
  # k = minPts - 1 (convention: minPts = ncol + 1)
  min_pts <- ncol(num_data) + 1
  min_pts <- max(min_pts, 3)

  knn_dists <- dbscan$kNNdist(
    dist_matrix, k = min_pts - 1
  )
  # Use the "knee" of the sorted kNN distance curve
  sorted_dists <- sort(knn_dists)
  eps <- estimate_dbscan_eps(sorted_dists)

  db_res <- dbscan$dbscan(
    dist_matrix, eps = eps, minPts = min_pts
  )

  clusters <- db_res$cluster
  # Label noise points (cluster 0) as their own group
  # so downstream code can handle them
  n_found <- length(unique(clusters[clusters > 0]))

  if (n_found == 0) {
    stop(
      "DBSCAN found no clusters with auto-computed ",
      "eps=", round(eps, 4),
      " and minPts=", min_pts,
      ". All points classified as noise."
    )
  }

  rhino$log$info(
    "DBSCAN: eps={round(eps, 4)}, ",
    "minPts={min_pts}, ",
    "clusters={n_found}, ",
    "noise={sum(clusters == 0)}"
  )

  list(
    clusters = clusters,
    details = list(
      variant = "dbscan",
      eps = eps,
      min_pts = min_pts,
      n_noise = sum(clusters == 0),
      n_clusters_found = n_found
    )
  )
}

estimate_dbscan_eps <- function(sorted_dists) {
  n <- length(sorted_dists)
  if (n < 3) return(stats$median(sorted_dists))

  # Detect knee via maximum second derivative
  # (same approach as elbow detection)
  first_diff <- diff(sorted_dists)
  second_diff <- diff(first_diff)

  if (length(second_diff) > 0) {
    knee_idx <- which.max(second_diff) + 1
    knee_idx <- max(1, min(knee_idx, n))
    sorted_dists[knee_idx]
  } else {
    stats$median(sorted_dists)
  }
}
