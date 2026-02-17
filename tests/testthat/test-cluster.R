box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/cluster,
  app/logic/cluster/cluster[cluster_error_parser],
)

impl <- attr(
  environment(cluster$run_clustering),
  "namespace"
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
# validate_inputs
# =============================================================================

describe("validate_inputs", {
  it("returns valid = TRUE for valid columns", {
    data <- data.frame(a = 1:3, b = 4:6, c = 7:9)
    result <- cluster$validate_inputs(c("a", "b"), data)
    expect_true(result$valid)
  })

  it("returns valid = FALSE when no columns selected", {
    data <- data.frame(a = 1:3)
    result <- cluster$validate_inputs(NULL, data)
    expect_true(!result$valid)
  })

  it("returns valid = FALSE for missing columns", {
    data <- data.frame(a = 1:3)
    result <- cluster$validate_inputs(c("a", "z"), data)
    expect_true(!result$valid)
  })
})

# =============================================================================
# run_clustering — K-Means (euclidean via stats$kmeans)
# =============================================================================

describe("run_clustering kmeans euclidean", {
  it("returns success with correct structure", {
    data <- make_cluster_data()
    result <- cluster$run_clustering(
      data, c("a", "b"), 2,
      algorithm = "kmeans", metric = "euclidean"
    )
    expect_true(result$success)
    r <- result$result
    expect_equal(length(r$clusters), nrow(data))
    expect_equal(r$n_clusters, 2)
    expect_equal(r$algorithm, "kmeans")
    expect_equal(r$metric, "euclidean")
    expect_equal(r$details$variant, "kmeans")
    expect_true(!is.null(r$details$centers))
  })

  it("assigns all rows to a cluster", {
    data <- make_cluster_data()
    result <- cluster$run_clustering(
      data, c("a", "b"), 3,
      algorithm = "kmeans", metric = "euclidean"
    )
    expect_true(result$success)
    expect_true(all(result$result$clusters >= 1))
  })
})

# =============================================================================
# run_clustering — K-Means + manhattan (via cluster$pam)
# =============================================================================

describe("run_clustering kmeans manhattan (PAM)", {
  it("returns success using PAM variant", {
    data <- make_cluster_data()
    result <- cluster$run_clustering(
      data, c("a", "b"), 2,
      algorithm = "kmeans", metric = "manhattan"
    )
    expect_true(result$success)
    r <- result$result
    expect_equal(r$details$variant, "pam")
    expect_equal(r$metric, "manhattan")
    expect_true(!is.null(r$details$medoids))
  })
})

# =============================================================================
# run_clustering — Hierarchical
# =============================================================================

describe("run_clustering hierarchical", {
  it("returns success with ward method", {
    data <- make_cluster_data()
    result <- cluster$run_clustering(
      data, c("a", "b"), 2,
      algorithm = "hierarchical",
      metric = "euclidean", method = "ward"
    )
    expect_true(result$success)
    r <- result$result
    expect_equal(r$algorithm, "hierarchical")
    expect_equal(r$details$variant, "hclust")
    expect_equal(r$details$method, "ward.D2")
    expect_equal(length(r$clusters), nrow(data))
  })

  it("works with single linkage and manhattan", {
    data <- make_cluster_data()
    result <- cluster$run_clustering(
      data, c("a", "b"), 2,
      algorithm = "hierarchical",
      metric = "manhattan", method = "single"
    )
    expect_true(result$success)
    expect_equal(
      result$result$details$method, "single"
    )
    expect_equal(
      result$result$details$metric, "manhattan"
    )
  })

  it("works with complete linkage", {
    data <- make_cluster_data()
    result <- cluster$run_clustering(
      data, c("a", "b"), 2,
      algorithm = "hierarchical",
      metric = "euclidean", method = "complete"
    )
    expect_true(result$success)
  })

  it("works with average linkage", {
    data <- make_cluster_data()
    result <- cluster$run_clustering(
      data, c("a", "b"), 2,
      algorithm = "hierarchical",
      metric = "euclidean", method = "average"
    )
    expect_true(result$success)
  })
})

# =============================================================================
# run_clustering — DBSCAN
# =============================================================================

describe("run_clustering dbscan", {
  it("returns success on well-separated data", {
    data <- make_cluster_data(n = 50)
    result <- cluster$run_clustering(
      data, c("a", "b"), n_clusters = 2,
      algorithm = "dbscan", metric = "euclidean"
    )
    expect_true(result$success)
    r <- result$result
    expect_equal(r$algorithm, "dbscan")
    expect_equal(r$details$variant, "dbscan")
    expect_true(r$details$n_clusters_found >= 1)
    expect_true(!is.null(r$details$eps))
  })

  it("ignores n_clusters parameter", {
    data <- make_cluster_data(n = 50)
    r1 <- cluster$run_clustering(
      data, c("a", "b"), n_clusters = 2,
      algorithm = "dbscan"
    )
    r2 <- cluster$run_clustering(
      data, c("a", "b"), n_clusters = 5,
      algorithm = "dbscan"
    )
    expect_true(r1$success)
    expect_true(r2$success)
    # Same data should produce same clusters
    expect_equal(
      r1$result$details$eps,
      r2$result$details$eps
    )
  })
})

# =============================================================================
# Shared quality metrics (all algorithms)
# =============================================================================

describe("shared quality metrics", {
  it("kmeans returns silhouette, bss_tss, withinss", {
    data <- make_cluster_data()
    r <- cluster$run_clustering(
      data, c("a", "b"), 2,
      algorithm = "kmeans", metric = "euclidean"
    )
    expect_true(r$success)
    d <- r$result$details
    expect_true(!is.na(d$silhouette_avg))
    expect_true(d$silhouette_avg >= -1 && d$silhouette_avg <= 1)
    expect_true(!is.na(d$bss_tss))
    expect_true(d$bss_tss >= 0 && d$bss_tss <= 1)
    expect_true(!is.null(d$tot_withinss))
    expect_true(!is.null(d$size))
  })

  it("hierarchical returns silhouette, bss_tss, withinss", {
    data <- make_cluster_data()
    r <- cluster$run_clustering(
      data, c("a", "b"), 2,
      algorithm = "hierarchical",
      metric = "euclidean", method = "ward"
    )
    expect_true(r$success)
    d <- r$result$details
    expect_true(!is.na(d$silhouette_avg))
    expect_true(!is.na(d$bss_tss))
    expect_true(!is.null(d$tot_withinss))
    expect_true(!is.null(d$size))
  })

  it("pam returns silhouette, bss_tss, withinss", {
    data <- make_cluster_data()
    r <- cluster$run_clustering(
      data, c("a", "b"), 2,
      algorithm = "kmeans", metric = "manhattan"
    )
    expect_true(r$success)
    d <- r$result$details
    expect_true(!is.na(d$silhouette_avg))
    expect_true(!is.na(d$bss_tss))
    expect_true(!is.null(d$tot_withinss))
  })

  it("dbscan returns silhouette, bss_tss, withinss", {
    data <- make_cluster_data(n = 50)
    r <- cluster$run_clustering(
      data, c("a", "b"), 2,
      algorithm = "dbscan", metric = "euclidean"
    )
    expect_true(r$success)
    d <- r$result$details
    # silhouette may be NA if only 1 cluster found
    expect_true(!is.null(d$silhouette_avg))
    expect_true(!is.null(d$bss_tss))
    expect_true(!is.null(d$tot_withinss))
  })

  it("bss + withinss equals totss", {
    data <- make_cluster_data()
    r <- cluster$run_clustering(
      data, c("a", "b"), 2,
      algorithm = "kmeans", metric = "euclidean"
    )
    d <- r$result$details
    expect_equal(
      d$betweenss + d$tot_withinss,
      d$totss,
      tolerance = 1e-6
    )
  })
})

# =============================================================================
# Cluster summary and result structure (all algorithms)
# =============================================================================

describe("cluster_summary and result structure", {
  it("kmeans returns cluster_summary with means", {
    data <- make_cluster_data()
    r <- cluster$run_clustering(
      data, c("a", "b"), 2,
      algorithm = "kmeans", metric = "euclidean"
    )
    expect_true(r$success)
    cs <- r$result$cluster_summary
    expect_true(!is.null(cs))
    expect_equal(nrow(cs$means), 2)
    expect_equal(ncol(cs$means), 2)
    expect_equal(length(cs$cluster_ids), 2)
    expect_equal(length(cs$n_per_cluster), 2)
    expect_equal(
      sum(cs$n_per_cluster), nrow(data)
    )
    expect_true(!is.null(cs$overall_mean))
  })

  it("hierarchical returns cluster_summary", {
    data <- make_cluster_data()
    r <- cluster$run_clustering(
      data, c("a", "b"), 2,
      algorithm = "hierarchical",
      metric = "euclidean", method = "ward"
    )
    cs <- r$result$cluster_summary
    expect_true(!is.null(cs))
    expect_equal(nrow(cs$means), 2)
  })

  it("columns field is returned", {
    data <- make_cluster_data()
    r <- cluster$run_clustering(
      data, c("a", "b"), 2,
      algorithm = "kmeans", metric = "euclidean"
    )
    expect_equal(r$result$columns, c("a", "b"))
  })

  it("result does not contain raw data", {
    data <- make_cluster_data()
    r <- cluster$run_clustering(
      data, c("a", "b"), 2,
      algorithm = "kmeans", metric = "euclidean"
    )
    expect_null(r$result$data)
    expect_null(r$result$membership_data)
  })
})

# =============================================================================
# run_clustering — Error cases
# =============================================================================

describe("run_clustering errors", {
  it("returns error for invalid columns", {
    data <- data.frame(a = 1:10)
    result <- cluster$run_clustering(
      data, c("nonexistent"), 2, "kmeans"
    )
    expect_true(!result$success)
  })

  it("returns error when n_clusters >= nrow", {
    data <- data.frame(a = 1:3, b = 4:6)
    result <- cluster$run_clustering(
      data, c("a", "b"), 5, "kmeans"
    )
    expect_true(!result$success)
  })

  it("returns error for unknown algorithm", {
    data <- make_cluster_data()
    result <- cluster$run_clustering(
      data, c("a", "b"), 2, "unknown_algo"
    )
    expect_true(!result$success)
  })

  it("returns error for n_clusters < 2", {
    data <- make_cluster_data()
    result <- cluster$run_clustering(
      data, c("a", "b"), 1, "kmeans"
    )
    expect_true(!result$success)
  })
})

# =============================================================================
# cluster_error_parser
# =============================================================================

describe("cluster_error_parser", {
  it("parses variance errors", {
    msg <- cluster_error_parser(
      "zero variance in column", "Test"
    )
    expect_true(grepl("variance", msg, ignore.case = TRUE))
  })

  it("parses NA errors", {
    msg <- cluster_error_parser(
      "data contains NA values", "Test"
    )
    expect_true(grepl("missing", msg, ignore.case = TRUE))
  })

  it("parses observation count errors", {
    msg <- cluster_error_parser(
      "not enough observations", "Test"
    )
    expect_true(grepl("observations", msg, ignore.case = TRUE))
  })

  it("parses DBSCAN noise errors", {
    msg <- cluster_error_parser(
      "no clusters found, all noise", "Test"
    )
    expect_true(grepl("DBSCAN", msg))
  })

  it("falls back for unknown errors", {
    msg <- cluster_error_parser(
      "something unexpected", "Test"
    )
    expect_true(grepl("failed", msg))
  })
})
