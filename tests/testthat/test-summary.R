box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/summary,
)

# =============================================================================
# validate_inputs
# =============================================================================

describe("validate_inputs", {
  it("returns valid = TRUE for valid columns", {
    data <- data.frame(a = 1:3, b = 4:6, c = 7:9)
    result <- summary$validate_inputs(c("a", "b"), data)
    expect_true(result$valid)
  })

  it("returns valid = FALSE when no columns selected", {
    data <- data.frame(a = 1:3)
    result <- summary$validate_inputs(NULL, data)
    expect_true(!result$valid)
  })

  it("returns valid = FALSE for missing columns", {
    data <- data.frame(a = 1:3)
    result <- summary$validate_inputs(c("a", "z"), data)
    expect_true(!result$valid)
  })
})

# =============================================================================
# run_analysis
# =============================================================================

describe("run_analysis", {
  it("returns success for valid input", {
    data <- data.frame(a = 1:3, b = 4:6)
    result <- summary$run_analysis(data, c("a", "b"))
    expect_true(result$success)
  })

  it("returns error for invalid columns", {
    data <- data.frame(a = 1:3)
    result <- summary$run_analysis(data, c("nonexistent"))
    expect_true(!result$success)
  })
})
