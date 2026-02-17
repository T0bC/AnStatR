box::use(
  rhino,
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

#' Run clustering analysis wrapped in safe_execute
#' @param data Data frame
#' @param columns Character vector of column names
#' @param n_clusters Integer number of clusters
#' @param algorithm Character clustering algorithm name
#' @return List with $success, $result or $error
#' @export
run_clustering <- function(data, columns, n_clusters, algorithm) {
  error_handling$safe_execute(
    expr = {
      subset <- data[, columns, drop = FALSE]
      # ... clustering computation will go here ...
      rhino$log$info(
        "Cluster: analysis complete ({length(columns)} columns, {n_clusters} clusters, {algorithm} algorithm)"
      )
      list(
        data = subset,
        n_clusters = n_clusters,
        algorithm = algorithm,
        clusters = rep(1:n_clusters, length.out = nrow(subset))
      )
    },
    operation_name = "cluster_analysis"
  )
}
