#' @export
box::use(
  app/logic/cluster/cluster[validate_inputs, run_clustering],
  app/logic/cluster/distance_matrix[compute_distance_matrix],
)
