box::use(
  testthat[describe, expect_equal, expect_false, expect_true, it],
)

box::use(
  app/logic/pca/pca[run_pca],
  app/logic/pca/pca_stats,
)

# =============================================================================
# Fixtures
# =============================================================================

# Small unit-norm loadings (3 variables x 2 components) and matching
# scores (4 samples x 2 components), hand-constructed so expected
# values can be computed directly.
loadings_fixture <- matrix(
  c(
    0.6, 0.8, 0.0,
    0.0, 0.6, 0.8
  ),
  nrow = 3, ncol = 2,
  dimnames = list(c("v1", "v2", "v3"), c("Dim.1", "Dim.2"))
)

scores_fixture <- matrix(
  c(
    1, -1, 2, -2,
    0, 3, -3, 1
  ),
  nrow = 4, ncol = 2,
  dimnames = list(NULL, c("Dim.1", "Dim.2"))
)

# =============================================================================
# compute_var_coord
# =============================================================================

describe("compute_var_coord", {
  it("equals loadings scaled by per-component sdev derived from scores", {
    n <- nrow(scores_fixture)
    sdev <- sqrt(colSums(scores_fixture^2) / (n - 1))
    expected <- sweep(loadings_fixture, 2, sdev, FUN = "*")

    result <- pca_stats$compute_var_coord(loadings_fixture, scores_fixture)

    expect_equal(result, expected)
  })
})

# =============================================================================
# compute_var_contrib
# =============================================================================

describe("compute_var_contrib", {
  it("matches loadings^2 * 100", {
    expected <- loadings_fixture^2 * 100
    result <- pca_stats$compute_var_contrib(loadings_fixture)
    expect_equal(result, expected)
  })

  it("sums to 100 per component for unit-norm loadings", {
    result <- pca_stats$compute_var_contrib(loadings_fixture)
    expect_equal(unname(colSums(result)), c(100, 100))
  })
})

# =============================================================================
# compute_var_cos2
# =============================================================================

describe("compute_var_cos2", {
  it("equals var_coord squared", {
    var_coord <- pca_stats$compute_var_coord(
      loadings_fixture, scores_fixture
    )
    expected <- var_coord^2
    result <- pca_stats$compute_var_cos2(var_coord)
    expect_equal(result, expected)
  })
})

# =============================================================================
# compute_ind_contrib
# =============================================================================

describe("compute_ind_contrib", {
  it("matches scores^2 / colSums(scores^2) * 100", {
    expected <- sweep(
      scores_fixture^2, 2, colSums(scores_fixture^2), FUN = "/"
    ) * 100
    result <- pca_stats$compute_ind_contrib(scores_fixture)
    expect_equal(result, expected)
  })

  it("sums to 100 per component", {
    result <- pca_stats$compute_ind_contrib(scores_fixture)
    expect_equal(unname(colSums(result)), c(100, 100))
  })
})

# =============================================================================
# compute_ind_cos2
# =============================================================================

describe("compute_ind_cos2", {
  it("matches scores_display^2 / rowSums(scores^2)", {
    total_dist2 <- rowSums(scores_fixture^2)
    expected <- sweep(scores_fixture^2, 1, total_dist2, FUN = "/")
    result <- pca_stats$compute_ind_cos2(scores_fixture, scores_fixture)
    expect_equal(result, expected)
  })

  it("does not produce NaN/Inf when a row has all-zero scores", {
    scores_with_zero_row <- rbind(scores_fixture, c(0, 0))
    rownames(scores_with_zero_row) <- NULL

    result <- pca_stats$compute_ind_cos2(
      scores_with_zero_row, scores_with_zero_row
    )

    last_row <- result[nrow(result), ]
    expect_true(all(is.finite(last_row)))
    expect_equal(unname(last_row), c(0, 0))
  })
})

# =============================================================================
# Cross-check against a real mixOmics::pca() fit
# =============================================================================

describe("pca_stats against a real run_pca() fit", {
  set.seed(42)
  test_data <- data.frame(
    a = rnorm(20, mean = 10, sd = 2),
    b = rnorm(20, mean = 5, sd = 1),
    c = rnorm(20, mean = 0, sd = 3),
    d = rnorm(20, mean = 20, sd = 5)
  )
  res <- run_pca(test_data, c("a", "b", "c", "d"))

  it("run_pca succeeds on the fixture data", {
    expect_true(res$success)
  })

  it("compute_var_contrib sums to 100 per component", {
    contrib <- pca_stats$compute_var_contrib(res$result$loadings)
    expect_equal(unname(colSums(contrib)), rep(100, ncol(contrib)))
  })

  it("compute_ind_contrib sums to 100 per component", {
    contrib <- pca_stats$compute_ind_contrib(res$result$scores)
    expect_equal(unname(colSums(contrib)), rep(100, ncol(contrib)))
  })

  it("compute_var_coord/compute_var_cos2 chain without NaN/Inf", {
    var_coord <- pca_stats$compute_var_coord(
      res$result$loadings, res$result$scores
    )
    var_cos2 <- pca_stats$compute_var_cos2(var_coord)
    expect_true(all(is.finite(var_coord)))
    expect_true(all(is.finite(var_cos2)))
  })

  it("compute_ind_cos2 stays finite and non-negative", {
    ind_cos2 <- pca_stats$compute_ind_cos2(
      res$result$scores, res$result$scores
    )
    expect_true(all(is.finite(ind_cos2)))
    expect_true(all(ind_cos2 >= 0))
  })
})
