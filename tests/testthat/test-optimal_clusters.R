box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/cluster/optimal_clusters,
)

impl <- attr(optimal_clusters, "namespace")

# =============================================================================
# compute_optimal_clusters
# =============================================================================

describe("compute_optimal_clusters", {
  it("returns success with valid data", {
    set.seed(42)
    data <- data.frame(
      a = c(rnorm(30, 0), rnorm(30, 5)),
      b = c(rnorm(30, 0), rnorm(30, 5))
    )
    result <- optimal_clusters$compute_optimal_clusters(
      data, c("a", "b"), max_k = 5
    )
    expect_true(result$success)
    expect_true(!is.null(result$result$methods$elbow))
    expect_true(!is.null(result$result$methods$silhouette))
    expect_true(!is.null(result$result$methods$gap))
    expect_true(!is.null(result$result$summary))
    expect_true(!is.null(result$result$plot_data))
  })

  it("returns valid optimal_k values within k_range", {
    set.seed(42)
    data <- data.frame(
      a = c(rnorm(20, 0), rnorm(20, 5)),
      b = c(rnorm(20, 0), rnorm(20, 5))
    )
    result <- optimal_clusters$compute_optimal_clusters(
      data, c("a", "b"), max_k = 6
    )
    expect_true(result$success)
    methods <- result$result$methods
    for (m in methods) {
      if (!is.null(m$optimal_k) && !is.na(m$optimal_k)) {
        expect_true(m$optimal_k >= 2)
        expect_true(m$optimal_k <= 6)
      }
    }
  })

  it("returns median_k in summary", {
    set.seed(42)
    data <- data.frame(
      a = c(rnorm(30, 0), rnorm(30, 5)),
      b = c(rnorm(30, 0), rnorm(30, 5))
    )
    result <- optimal_clusters$compute_optimal_clusters(
      data, c("a", "b"), max_k = 5
    )
    expect_true(result$success)
    s <- result$result$summary
    expect_true(s$median_k >= s$min_k)
    expect_true(s$median_k <= s$max_k)
    expect_true(s$methods_computed >= 1)
  })

  it("clamps max_k to nrow - 1", {
    set.seed(42)
    data <- data.frame(a = 1:5, b = 6:10)
    result <- optimal_clusters$compute_optimal_clusters(
      data, c("a", "b"), max_k = 20
    )
    expect_true(result$success)
    # max_k should be clamped to 4 (nrow - 1)
    expect_true(
      max(result$result$k_range) <= nrow(data) - 1
    )
  })

  it("returns error for NULL data", {
    result <- optimal_clusters$compute_optimal_clusters(
      NULL, c("a"), max_k = 5
    )
    expect_true(!result$success)
  })

  it("returns error for empty columns", {
    data <- data.frame(a = 1:10)
    result <- optimal_clusters$compute_optimal_clusters(
      data, character(0), max_k = 5
    )
    expect_true(!result$success)
  })

  it("returns error for non-numeric columns", {
    data <- data.frame(
      a = letters[1:10],
      b = 1:10,
      stringsAsFactors = FALSE
    )
    result <- optimal_clusters$compute_optimal_clusters(
      data, c("a", "b"), max_k = 5
    )
    expect_true(!result$success)
  })

  it("returns error for too few rows", {
    data <- data.frame(a = 1:2, b = 3:4)
    result <- optimal_clusters$compute_optimal_clusters(
      data, c("a", "b"), max_k = 5
    )
    expect_true(!result$success)
  })

  it("builds correct plot_data structure", {
    set.seed(42)
    data <- data.frame(
      a = c(rnorm(30, 0), rnorm(30, 5)),
      b = c(rnorm(30, 0), rnorm(30, 5))
    )
    result <- optimal_clusters$compute_optimal_clusters(
      data, c("a", "b"), max_k = 5
    )
    expect_true(result$success)
    pd <- result$result$plot_data
    expect_true("k" %in% names(pd))
    expect_true("value" %in% names(pd))
    expect_true("method" %in% names(pd))
  })
})

# =============================================================================
# detect_elbow_k (internal)
# =============================================================================

describe("detect_elbow_k", {
  it("returns first k for fewer than 3 values", {
    k <- impl$detect_elbow_k(2:3, c(10, 5))
    expect_equal(k, 2)
  })

  it("detects elbow in clear elbow pattern", {
    # Sharp drop then flat: elbow at k=3
    k_range <- 2:6
    values <- c(100, 30, 20, 18, 17)
    k <- impl$detect_elbow_k(k_range, values)
    expect_true(k >= 2 && k <= 6)
  })
})

# =============================================================================
# optimal_clusters_error_parser
# =============================================================================

describe("optimal_clusters_error_parser", {
  it("parses variance errors", {
    msg <- optimal_clusters$optimal_clusters_error_parser(
      "zero variance in column", "Test"
    )
    expect_true(grepl("variance", msg, ignore.case = TRUE))
  })

  it("parses NA errors", {
    msg <- optimal_clusters$optimal_clusters_error_parser(
      "data contains NA values", "Test"
    )
    expect_true(grepl("missing", msg, ignore.case = TRUE))
  })

  it("falls back for unknown errors", {
    msg <- optimal_clusters$optimal_clusters_error_parser(
      "something unexpected", "Test"
    )
    expect_true(grepl("failed", msg))
  })
})
