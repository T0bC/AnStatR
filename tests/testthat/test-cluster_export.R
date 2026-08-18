box::use(
  app/logic/cluster/cluster[run_clustering],
  app/logic/cluster/cluster_export[create_cluster_bundle],
  app/logic/prediction/bundle_io[validate_bundle],
)

# =============================================================================
# Tests for Cluster bundle export
# =============================================================================

make_cluster_data <- function(n = 30, seed = 42) {
  set.seed(seed)
  data.frame(
    a = c(rnorm(n, 0, 0.5), rnorm(n, 5, 0.5)),
    b = c(rnorm(n, 0, 0.5), rnorm(n, 5, 0.5))
  )
}

test_that("create_cluster_bundle produces a bundle that passes validate_bundle (kmeans)", {
  data <- make_cluster_data()
  clustering <- run_clustering(
    data, c("a", "b"), 2,
    algorithm = "kmeans", metric = "euclidean"
  )
  expect_true(clustering$success)

  bundle <- create_cluster_bundle(
    cluster_result = clustering$result,
    raw_data = data,
    used_data = data,
    numeric_cols = c("a", "b"),
    meta_cols = character(0),
    transform_params = list(),
    scale_params = NULL,
    settings = list(
      algorithm = "kmeans",
      metric = "euclidean",
      n_clusters = 2
    )
  )

  expect_equal(bundle$analysis_type, "cluster")
  expect_equal(bundle$variant, "kmeans")
  expect_true(!is.null(bundle$model))
  expect_true(inherits(bundle$model, "kmeans"))
  expect_equal(bundle$cluster_metric, "euclidean")
  expect_length(bundle$cluster_labels, nrow(data))

  validation <- validate_bundle(bundle)
  expect_true(validation$valid)
})

test_that("create_cluster_bundle produces a bundle that passes validate_bundle (pam)", {
  data <- make_cluster_data()
  clustering <- run_clustering(
    data, c("a", "b"), 2,
    algorithm = "kmeans", metric = "manhattan"
  )
  expect_true(clustering$success)

  bundle <- create_cluster_bundle(
    cluster_result = clustering$result,
    raw_data = data,
    used_data = data,
    numeric_cols = c("a", "b"),
    meta_cols = character(0),
    transform_params = list(),
    scale_params = NULL,
    settings = list(
      algorithm = "kmeans",
      metric = "manhattan",
      n_clusters = 2
    )
  )

  expect_equal(bundle$variant, "pam")
  expect_true(inherits(bundle$model, "pam"))
  expect_equal(bundle$cluster_metric, "manhattan")

  validation <- validate_bundle(bundle)
  expect_true(validation$valid)
})
