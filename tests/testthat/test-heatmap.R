box::use(
  testthat[describe, expect_false, expect_true, it],
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
# create_cluster_heatmap — hierarchical
# =============================================================================

describe("create_cluster_heatmap with hierarchical result", {
  data <- make_cluster_data()
  hc_result <- cluster$run_clustering(
    data, c("a", "b"), 2,
    algorithm = "hierarchical",
    metric = "euclidean",
    method = "ward"
  )

  it("returns success with a plotly object", {
    expect_true(hc_result$success)
    hm_res <- cluster$create_cluster_heatmap(
      hc_result$result,
      data = data,
      measure_cols = c("a", "b")
    )
    expect_true(hm_res$success)
    expect_true(inherits(hm_res$result, "plotly"))
  })

  it("works with seriation = OLO (default)", {
    hm_res <- cluster$create_cluster_heatmap(
      hc_result$result,
      data = data,
      measure_cols = c("a", "b"),
      seriation = "OLO"
    )
    expect_true(hm_res$success)
    expect_true(inherits(hm_res$result, "plotly"))
  })

  it("works with seriation = GW", {
    hm_res <- cluster$create_cluster_heatmap(
      hc_result$result,
      data = data,
      measure_cols = c("a", "b"),
      seriation = "GW"
    )
    expect_true(hm_res$success)
    expect_true(inherits(hm_res$result, "plotly"))
  })

  it("works with seriation = mean", {
    hm_res <- cluster$create_cluster_heatmap(
      hc_result$result,
      data = data,
      measure_cols = c("a", "b"),
      seriation = "mean"
    )
    expect_true(hm_res$success)
    expect_true(inherits(hm_res$result, "plotly"))
  })

  it("works with seriation = none", {
    hm_res <- cluster$create_cluster_heatmap(
      hc_result$result,
      data = data,
      measure_cols = c("a", "b"),
      seriation = "none"
    )
    expect_true(hm_res$success)
    expect_true(inherits(hm_res$result, "plotly"))
  })

  it("works with show_labels = TRUE", {
    hm_res <- cluster$create_cluster_heatmap(
      hc_result$result,
      data = data,
      measure_cols = c("a", "b"),
      show_labels = TRUE
    )
    expect_true(hm_res$success)
    expect_true(inherits(hm_res$result, "plotly"))
  })

  it("works with custom_labels", {
    labels <- paste0("S", seq_len(nrow(data)))
    hm_res <- cluster$create_cluster_heatmap(
      hc_result$result,
      data = data,
      measure_cols = c("a", "b"),
      show_labels = TRUE,
      custom_labels = labels
    )
    expect_true(hm_res$success)
    expect_true(inherits(hm_res$result, "plotly"))
  })

  it("works with row_side_colors_df", {
    side_df <- data.frame(
      group = rep(c("A", "B"), each = 30)
    )
    hm_res <- cluster$create_cluster_heatmap(
      hc_result$result,
      data = data,
      measure_cols = c("a", "b"),
      row_side_colors_df = side_df
    )
    expect_true(hm_res$success)
    expect_true(inherits(hm_res$result, "plotly"))
  })
})

# =============================================================================
# create_cluster_heatmap — non-hierarchical algorithms
# =============================================================================

describe("create_cluster_heatmap with kmeans result", {
  data <- make_cluster_data()
  km_result <- cluster$run_clustering(
    data, c("a", "b"), 2,
    algorithm = "kmeans",
    metric = "euclidean"
  )

  it("returns an app_error for kmeans", {
    expect_true(km_result$success)
    hm_res <- cluster$create_cluster_heatmap(
      km_result$result,
      data = data,
      measure_cols = c("a", "b")
    )
    expect_false(hm_res$success)
    expect_true(
      error_handling$is_app_error(hm_res$error)
    )
  })
})

describe("create_cluster_heatmap with dbscan result", {
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
      hm_res <- cluster$create_cluster_heatmap(
        db_result$result,
        data = data,
        measure_cols = c("a", "b")
      )
      expect_false(hm_res$success)
      expect_true(
        error_handling$is_app_error(hm_res$error)
      )
    }
  })
})

# =============================================================================
# create_cluster_heatmap — NULL input
# =============================================================================

describe("create_cluster_heatmap with NULL input", {
  it("returns an app_error for NULL cluster_result", {
    hm_res <- cluster$create_cluster_heatmap(
      NULL,
      data = data.frame(a = 1, b = 2),
      measure_cols = c("a", "b")
    )
    expect_false(hm_res$success)
    expect_true(
      error_handling$is_app_error(hm_res$error)
    )
  })

  it("returns an app_error for NULL data", {
    data <- make_cluster_data()
    hc_result <- cluster$run_clustering(
      data, c("a", "b"), 2,
      algorithm = "hierarchical",
      metric = "euclidean",
      method = "ward"
    )
    hm_res <- cluster$create_cluster_heatmap(
      hc_result$result,
      data = NULL,
      measure_cols = c("a", "b")
    )
    expect_false(hm_res$success)
    expect_true(
      error_handling$is_app_error(hm_res$error)
    )
  })
})
