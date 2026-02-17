box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/cluster,
)

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
# run_clustering
# =============================================================================

describe("run_clustering", {
  it("returns success for valid input", {
    data <- data.frame(a = 1:3, b = 4:6)
    result <- cluster$run_clustering(data, c("a", "b"), 3, "kmeans")
    expect_true(result$success)
  })

  it("returns error for invalid columns", {
    data <- data.frame(a = 1:3)
    result <- cluster$run_clustering(data, c("nonexistent"), 3, "kmeans")
    expect_true(!result$success)
  })

  it("returns correct cluster structure", {
    data <- data.frame(a = 1:5, b = 6:10)
    result <- cluster$run_clustering(data, c("a", "b"), 2, "kmeans")
    expect_true(result$success)
    expect_equal(length(result$result$clusters), nrow(data))
    expect_equal(result$result$n_clusters, 2)
    expect_equal(result$result$algorithm, "kmeans")
  })
})
