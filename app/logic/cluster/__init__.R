#' @export
box::use(
  app/logic/cluster/cluster[
    cluster_error_parser,
    compute_cluster_summary,
    run_clustering,
    validate_inputs,
  ],
  app/logic/cluster/heatmap[
    create_cluster_heatmap,
  ],
  app/logic/cluster/hopkins[compute_hopkins],
  app/logic/cluster/optimal_clusters[
    compute_optimal_clusters,
    create_optimal_clusters_ggplot,
  ],
)
