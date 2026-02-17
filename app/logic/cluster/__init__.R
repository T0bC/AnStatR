#' @export
box::use(
  app/logic/cluster/cluster[
    validate_inputs,
    run_clustering,
    cluster_error_parser,
  ],
  app/logic/cluster/hopkins[compute_hopkins],
  app/logic/cluster/optimal_clusters[
    compute_optimal_clusters,
    create_optimal_clusters_ggplot,
  ],
)
