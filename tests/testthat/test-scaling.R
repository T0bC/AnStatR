box::use(
  testthat[describe, expect_equal, expect_false, expect_true,
           it],
)

box::use(
  app/logic/pca/scaling,
)

# =============================================================================
# scale_data
# =============================================================================

describe("scale_data", {
  it("returns success with scaled data", {
    data <- data.frame(
      meta = c("a", "b", "c", "d"),
      x = c(10, 20, 30, 40),
      y = c(100, 200, 300, 400)
    )
    result <- scaling$scale_data(data, c("x", "y"))
    expect_true(result$success)
    expect_equal(ncol(result$result), 3)
    expect_equal(
      round(mean(result$result$x), 10), 0
    )
    expect_equal(
      round(sd(result$result$x), 10), 1
    )
  })

  it("preserves metadata columns unchanged", {
    data <- data.frame(
      meta = c("a", "b", "c"),
      x = c(10, 20, 30)
    )
    result <- scaling$scale_data(data, "x")
    expect_true(result$success)
    expect_equal(result$result$meta, c("a", "b", "c"))
  })

  it("preserves row count", {
    data <- data.frame(x = 1:10, y = 11:20)
    result <- scaling$scale_data(data, c("x", "y"))
    expect_true(result$success)
    expect_equal(nrow(result$result), 10)
  })

  it("returns error for constant columns (zero variance)", {
    data <- data.frame(
      x = c(5, 5, 5),
      y = c(1, 2, 3)
    )
    result <- scaling$scale_data(data, c("x", "y"))
    expect_false(result$success)
    expect_true(result$error$is_error)
    expect_true(grepl("zero variance", result$error$message,
                       ignore.case = TRUE))
  })

  it("centers only when scale = FALSE", {
    data <- data.frame(x = c(10, 20, 30))
    result <- scaling$scale_data(
      data, "x", center = TRUE, scale = FALSE
    )
    expect_true(result$success)
    expect_equal(
      round(mean(result$result$x), 10), 0
    )
    expect_true(sd(result$result$x) != 1)
  })

  it("scales only when center = FALSE", {
    data <- data.frame(x = c(10, 20, 30))
    result <- scaling$scale_data(
      data, "x", center = FALSE, scale = TRUE
    )
    expect_true(result$success)
    expect_true(mean(result$result$x) != 0)
  })
})

# =============================================================================
# scaling_error_parser
# =============================================================================

describe("scaling_error_parser", {
  it("parses zero variance errors", {
    msg <- scaling$scaling_error_parser(
      "zero variance columns found"
    )
    expect_true(grepl("zero variance", msg,
                       ignore.case = TRUE))
  })

  it("returns generic message for unknown errors", {
    msg <- scaling$scaling_error_parser("something broke")
    expect_true(grepl("something broke", msg))
  })
})
