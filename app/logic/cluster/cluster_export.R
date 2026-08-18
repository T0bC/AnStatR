box::use(
  rhino,
)

box::use(
  app/logic/shared/settings[app_version],
)

# =============================================================================
# Pure logic functions for Cluster result export
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Create a standardized RDS bundle for Cluster export
#'
#' Builds the named list that the prediction module
#' expects when loading a Cluster .rds file. Only
#' K-Means/PAM clusters fit on raw measurement data are
#' supported (see run_kmeans()) — the caller is
#' responsible for gating on data_source == "raw" and
#' algorithm == "kmeans" before calling this.
#'
#' @param cluster_result Cluster result list from
#'   run_clustering() (the $result field, not the wrapper)
#' @param raw_data Data frame, original data before
#'   any transforms
#' @param used_data Data frame, data actually passed
#'   to the clustering algorithm (after transform + NA
#'   removal + scaling)
#' @param numeric_cols Character vector of measurement
#'   column names
#' @param meta_cols Character vector of metadata column
#'   names
#' @param transform_params List of per-column transform
#'   param lists (from transform_skewed), or empty list
#' @param scale_params List with center/scale vectors
#'   (from base::scale()-style centering), or NULL
#' @param settings List with algorithm, metric,
#'   n_clusters, etc.
#' @return Named list (the bundle)
#' @export
create_cluster_bundle <- function(cluster_result, raw_data,
                                  used_data, numeric_cols,
                                  meta_cols = character(0),
                                  transform_params = list(),
                                  scale_params = NULL,
                                  settings = list()) {
  variant <- cluster_result$details$variant

  bundle <- list(
    analysis_type = "cluster",
    variant = variant,
    model = cluster_result$details$fitted_model,
    raw_data = raw_data,
    used_data = used_data,
    group_col = NULL,
    numeric_cols = numeric_cols,
    meta_cols = meta_cols,
    transform_params = transform_params,
    scale_params = scale_params,
    cluster_metric = cluster_result$metric,
    n_clusters = cluster_result$n_clusters,
    cluster_labels = as.integer(cluster_result$clusters),
    settings = settings,
    data_source = "raw",
    app_version = app_version,
    created = Sys.time()
  )

  rhino$log$info(
    "Cluster bundle: created ({variant}, ",
    "{length(numeric_cols)} vars,",
    " {nrow(used_data)} obs)"
  )

  bundle
}
