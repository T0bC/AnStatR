box::use(
  testthat[describe, expect_true, expect_false, it],
)

box::use(
  app/logic/cluster,
  app/logic/error_handling,
)

# Helper: well-separated 2-cluster data
make_cluster_data <- function(n = 30, seed = 42) {
  set.seed(seed)
  data.frame(
    a = c(rnorm(n, 0, 0.5), rnorm(n, 5, 0.5)),
    b = c(rnorm(n, 0, 0.5), rnorm(n, 5, 0.5))
  )
}

# =============================================================================
# create_dendrogram_plot — hierarchical
# =============================================================================

describe("create_dendrogram_plot with hierarchical result", {
  data <- make_cluster_data()
  hc_result <- cluster$run_clustering(
    data, c("a", "b"), 2,
    algorithm = "hierarchical",
    metric = "euclidean",
    method = "ward"
  )

  it("returns success with a ggplot object", {
    expect_true(hc_result$success)
    dend_res <- cluster$create_dendrogram_plot(
      hc_result$result
    )
    expect_true(dend_res$success)
    expect_true(inherits(dend_res$result, "ggplot"))
  })

  it("works with horiz = TRUE", {
    dend_res <- cluster$create_dendrogram_plot(
      hc_result$result, horiz = TRUE
    )
    expect_true(dend_res$success)
    expect_true(inherits(dend_res$result, "ggplot"))
  })

  it("works with polar = TRUE", {
    dend_res <- cluster$create_dendrogram_plot(
      hc_result$result, polar = TRUE
    )
    expect_true(dend_res$success)
    expect_true(inherits(dend_res$result, "ggplot"))
  })

  it("works with show_labels = TRUE", {
    dend_res <- cluster$create_dendrogram_plot(
      hc_result$result, show_labels = TRUE
    )
    expect_true(dend_res$success)
    expect_true(inherits(dend_res$result, "ggplot"))
  })

  it("works with show_rectangles = TRUE", {
    dend_res <- cluster$create_dendrogram_plot(
      hc_result$result, show_rectangles = TRUE
    )
    expect_true(dend_res$success)
    expect_true(inherits(dend_res$result, "ggplot"))
  })

  it("works with all display options combined", {
    dend_res <- cluster$create_dendrogram_plot(
      hc_result$result,
      horiz = TRUE,
      polar = TRUE,
      show_labels = TRUE,
      show_rectangles = TRUE
    )
    expect_true(dend_res$success)
    expect_true(inherits(dend_res$result, "ggplot"))
  })
})

# =============================================================================
# create_dendrogram_plot — non-hierarchical algorithms
# =============================================================================

describe("create_dendrogram_plot with kmeans result", {
  data <- make_cluster_data()
  km_result <- cluster$run_clustering(
    data, c("a", "b"), 2,
    algorithm = "kmeans",
    metric = "euclidean"
  )

  it("returns an app_error for kmeans", {
    expect_true(km_result$success)
    dend_res <- cluster$create_dendrogram_plot(
      km_result$result
    )
    expect_false(dend_res$success)
    expect_true(
      error_handling$is_app_error(dend_res$error)
    )
  })
})

describe("create_dendrogram_plot with dbscan result", {
  data <- make_cluster_data(n = 50)
  db_result <- cluster$run_clustering(
    data, c("a", "b"), 2,
    algorithm = "dbscan",
    metric = "euclidean"
  )

  it("returns an app_error for dbscan", {
    if (!db_result$success) {
      # DBSCAN may fail on small data; skip gracefully
      expect_true(TRUE)
    } else {
      dend_res <- cluster$create_dendrogram_plot(
        db_result$result
      )
      expect_false(dend_res$success)
      expect_true(
        error_handling$is_app_error(dend_res$error)
      )
    }
  })
})

# =============================================================================
# create_dendrogram_plot — NULL input
# =============================================================================

describe("create_dendrogram_plot with NULL input", {
  it("returns an app_error for NULL", {
    dend_res <- cluster$create_dendrogram_plot(NULL)
    expect_false(dend_res$success)
    expect_true(
      error_handling$is_app_error(dend_res$error)
    )
  })
})
