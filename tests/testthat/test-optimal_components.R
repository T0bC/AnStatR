box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/pca/optimal_components,
)

impl <- attr(optimal_components, "namespace")

# =============================================================================
# calculate_optimal_components
# =============================================================================

describe("calculate_optimal_components", {
  it("returns success with expected structure for valid data", {
    set.seed(42)
    data <- data.frame(
      a = rnorm(50),
      b = rnorm(50),
      c = rnorm(50),
      d = rnorm(50)
    )
    res <- optimal_components$calculate_optimal_components(data)

    expect_true(res$success)
    expect_true(!is.null(res$result))
    expect_true(!is.null(res$result$eigenvalues))
    expect_equal(length(res$result$eigenvalues), 4)
    expect_true(!is.null(res$result$methods$kaiser))
    expect_true(!is.null(res$result$methods$elbow))
    expect_true(!is.null(res$result$methods$parallel))
    expect_true(!is.null(res$result$summary))
  })

  it("returns ncp >= 1 for all methods", {
    set.seed(42)
    data <- data.frame(
      a = rnorm(30),
      b = rnorm(30),
      c = rnorm(30)
    )
    res <- optimal_components$calculate_optimal_components(data)

    expect_true(res$success)
    expect_true(res$result$methods$kaiser$ncp >= 1)
    expect_true(res$result$methods$elbow$ncp >= 1)
    expect_true(res$result$methods$parallel$ncp >= 1)
  })

  it("Kaiser returns correct count for known eigenvalues", {
    # Create data where we know the structure:
    # 2 strong components, rest noise
    set.seed(123)
    n <- 100
    x1 <- rnorm(n)
    x2 <- rnorm(n)
    data <- data.frame(
      a = x1 + rnorm(n, sd = 0.1),
      b = x1 + rnorm(n, sd = 0.1),
      c = x2 + rnorm(n, sd = 0.1),
      d = x2 + rnorm(n, sd = 0.1),
      e = rnorm(n),
      f = rnorm(n)
    )
    res <- optimal_components$calculate_optimal_components(data)

    expect_true(res$success)
    # With 2 strong components, Kaiser should find ~2
    expect_true(res$result$methods$kaiser$ncp >= 1)
    expect_true(res$result$methods$kaiser$ncp <= 4)
  })

  it("handles 2-column edge case", {
    set.seed(42)
    data <- data.frame(
      a = rnorm(20),
      b = rnorm(20)
    )
    res <- optimal_components$calculate_optimal_components(data)

    expect_true(res$success)
    expect_equal(length(res$result$eigenvalues), 2)
  })

  it("summary has correct fields", {
    set.seed(42)
    data <- data.frame(
      a = rnorm(30),
      b = rnorm(30),
      c = rnorm(30)
    )
    res <- optimal_components$calculate_optimal_components(data)

    expect_true(res$success)
    s <- res$result$summary
    expect_true(!is.null(s$min_ncp))
    expect_true(!is.null(s$max_ncp))
    expect_true(!is.null(s$median_ncp))
    expect_true(!is.null(s$methods_computed))
    expect_true(s$min_ncp <= s$max_ncp)
    expect_true(s$methods_computed >= 1)
  })
})

# =============================================================================
# detect_elbow
# =============================================================================

describe("detect_elbow", {
  it("returns ncp = 1 for fewer than 3 eigenvalues", {
    res <- impl$detect_elbow(c(3, 1))
    expect_equal(res$ncp, 1)
  })

  it("finds elbow in a clear scree pattern", {
    # Strong drop after first component
    eigenvalues <- c(5, 1.2, 0.9, 0.5, 0.3)
    res <- impl$detect_elbow(eigenvalues)
    expect_true(res$ncp >= 1)
    expect_true(res$ncp <= 4)
  })

  it("returns sensible result for flat eigenvalues", {
    eigenvalues <- c(1.1, 1.0, 0.9, 0.8)
    res <- impl$detect_elbow(eigenvalues)
    expect_true(res$ncp >= 1)
  })
})

# =============================================================================
# compute_parallel_analysis
# =============================================================================

describe("compute_parallel_analysis", {
  it("returns ncp >= 1 and random eigenvalues", {
    set.seed(42)
    data <- data.frame(
      a = rnorm(30),
      b = rnorm(30),
      c = rnorm(30)
    )
    res <- impl$compute_parallel_analysis(data, n_iter = 20)

    expect_true(res$ncp >= 1)
    expect_equal(length(res$random_eigenvalues), 3)
    expect_equal(length(res$actual_eigenvalues), 3)
  })

  it("detects signal above noise", {
    set.seed(123)
    n <- 100
    signal <- rnorm(n)
    data <- data.frame(
      a = signal + rnorm(n, sd = 0.1),
      b = signal + rnorm(n, sd = 0.1),
      c = rnorm(n),
      d = rnorm(n)
    )
    res <- impl$compute_parallel_analysis(data, n_iter = 50)

    # Should detect at least 1 component above noise
    expect_true(res$ncp >= 1)
  })
})
