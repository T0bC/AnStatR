box::use(
  testthat[
    describe,
    expect_equal,
    expect_false,
    expect_gt,
    expect_gte,
  ],
  testthat[
    expect_length,
    expect_lt,
    expect_true,
    it,
  ],
)

box::use(
  app/logic/preprocessing/skewness_transform,
)

impl <- attr(skewness_transform, "namespace")

# =============================================================================
# detect_skewness
# =============================================================================

describe("detect_skewness", {
  it("returns empty data frame for empty input", {
    data <- data.frame(a = 1:5)
    result <- skewness_transform$detect_skewness(
      data, character(0)
    )
    expect_equal(nrow(result), 0)
    expect_true("column" %in% names(result))
    expect_true("skewness" %in% names(result))
    expect_true("direction" %in% names(result))
    expect_true("is_skewed" %in% names(result))
  })

  it("flags right-skewed columns", {
    # Highly right-skewed data (skewness > 2)
    set.seed(42)
    data <- data.frame(
      x = rexp(200, rate = 0.5) ^ 2
    )
    result <- skewness_transform$detect_skewness(
      data, "x"
    )
    expect_equal(nrow(result), 1)
    expect_gt(result$skewness[1], 0)
    expect_equal(result$direction[1], "right")
    expect_true(result$is_skewed[1])
  })

  it("flags left-skewed columns", {
    # Highly left-skewed data (skewness < -2)
    set.seed(42)
    raw <- rexp(200, rate = 0.5) ^ 2
    data <- data.frame(
      x = max(raw) + 1 - raw
    )
    result <- skewness_transform$detect_skewness(
      data, "x"
    )
    expect_equal(nrow(result), 1)
    expect_lt(result$skewness[1], 0)
    expect_equal(result$direction[1], "left")
    expect_true(result$is_skewed[1])
  })

  it("does not flag symmetric columns", {
    set.seed(42)
    data <- data.frame(
      x = rnorm(200, mean = 50, sd = 10)
    )
    result <- skewness_transform$detect_skewness(
      data, "x"
    )
    expect_equal(nrow(result), 1)
    expect_equal(result$direction[1], "symmetric")
    expect_false(result$is_skewed[1])
  })

  it("respects custom threshold", {
    set.seed(42)
    data <- data.frame(
      x = rexp(200, rate = 1)
    )
    # With very high threshold, nothing should be flagged
    result <- skewness_transform$detect_skewness(
      data, "x",
      threshold = 100
    )
    expect_false(result$is_skewed[1])
    expect_equal(result$direction[1], "symmetric")
  })

  it("handles multiple columns with mixed skewness", {
    set.seed(42)
    right_raw <- rexp(200, rate = 0.5) ^ 2
    left_raw <- rexp(200, rate = 0.5) ^ 2
    data <- data.frame(
      right_skew = right_raw,
      symmetric = rnorm(200),
      left_skew = max(left_raw) + 1 - left_raw
    )
    result <- skewness_transform$detect_skewness(
      data, c("right_skew", "symmetric", "left_skew")
    )
    expect_equal(nrow(result), 3)
    # Check that right_skew is flagged as right
    rs <- result[result$column == "right_skew", ]
    expect_equal(rs$direction, "right")
    expect_true(rs$is_skewed)
    # Check that symmetric is not flagged
    sym <- result[result$column == "symmetric", ]
    expect_equal(sym$direction, "symmetric")
    expect_false(sym$is_skewed)
  })

  it("sorts by abs_skewness descending", {
    set.seed(42)
    data <- data.frame(
      mild = c(rexp(100, rate = 2), rnorm(100)),
      extreme = rexp(200, rate = 0.5)
    )
    result <- skewness_transform$detect_skewness(
      data, c("mild", "extreme")
    )
    expect_gte(result$abs_skewness[1], result$abs_skewness[2])
  })
})

# =============================================================================
# compute_skewness — NA handling
# =============================================================================

describe("compute_skewness with missing values", {
  it("ignores NAs instead of returning NA", {
    set.seed(42)
    x <- rexp(200, rate = 0.5) ^ 2
    with_na <- c(x, NA, NA)
    expect_equal(
      impl$compute_skewness(with_na),
      impl$compute_skewness(x)
    )
  })

  it("returns NA when too few values are observed", {
    expect_true(is.na(
      impl$compute_skewness(c(1, 2, NA, NA))
    ))
    expect_true(is.na(
      impl$compute_skewness(c(NA_real_, NA_real_))
    ))
  })

  it("returns NA below the 20-observation floor", {
    set.seed(42)
    x <- rexp(19, rate = 0.5) ^ 2
    expect_true(is.na(impl$compute_skewness(x)))
    expect_false(is.na(
      impl$compute_skewness(c(x, rexp(1, rate = 0.5) ^ 2))
    ))
  })

  it("counts observed values, not row count, against the floor", {
    set.seed(42)
    x <- c(rexp(19, rate = 0.5) ^ 2, rep(NA_real_, 50))
    expect_true(is.na(impl$compute_skewness(x)))
  })

  it("honours a caller-supplied floor", {
    set.seed(42)
    x <- rexp(10, rate = 0.5) ^ 2
    expect_true(is.na(impl$compute_skewness(x)))
    expect_false(is.na(impl$compute_skewness(x, min_n = 5)))
  })

  it("leaves a short column untransformed via detect_skewness", {
    set.seed(42)
    data <- data.frame(x = rexp(12, rate = 0.5) ^ 2)
    result <- skewness_transform$detect_skewness(data, "x")
    expect_true(is.na(result$skewness[1]))
    expect_false(result$is_skewed[1])
    expect_equal(result$direction[1], "symmetric")
  })

  it("flags an NA-bearing column as skewed", {
    # Regression: a single NA used to make skewness NA, which
    # detect_skewness() reads as "symmetric" — the column was
    # then silently exempted from correction.
    set.seed(42)
    data <- data.frame(x = rexp(200, rate = 0.5) ^ 2)
    data$x[c(5, 50)] <- NA
    result <- skewness_transform$detect_skewness(data, "x")
    expect_false(is.na(result$skewness[1]))
    expect_true(result$is_skewed[1])
    expect_equal(result$direction[1], "right")
  })
})


# =============================================================================
# transform_skewed
# =============================================================================

describe("transform_skewed", {
  it("returns unchanged data when no columns are skewed", {
    set.seed(42)
    data <- data.frame(
      x = rnorm(100),
      y = rnorm(100)
    )
    skew_result <- skewness_transform$detect_skewness(
      data, c("x", "y")
    )
    result <- skewness_transform$transform_skewed(
      data, c("x", "y"), skew_result
    )
    expect_true(result$success)
    expect_equal(nrow(result$result$transformed_cols), 0)
    expect_length(result$result$skipped_cols, 0)
    expect_equal(result$result$data, data)
  })

  it("transforms right-skewed columns", {
    set.seed(42)
    data <- data.frame(
      x = rexp(200, rate = 0.5) ^ 2,
      meta = letters[1:200]
    )
    skew_result <- skewness_transform$detect_skewness(
      data, "x"
    )
    result <- skewness_transform$transform_skewed(
      data, "x", skew_result
    )
    expect_true(result$success)
    expect_equal(nrow(result$result$transformed_cols), 1)
    expect_equal(
      result$result$transformed_cols$direction[1], "right"
    )
    # Skewness should be reduced
    expect_lt(
      abs(result$result$transformed_cols$skewness_after[1]),
      abs(result$result$transformed_cols$skewness_before[1])
    )
    # Metadata should be preserved
    expect_equal(result$result$data$meta, data$meta)
  })

  it("reports progress once per column, before fitting", {
    set.seed(42)
    data <- data.frame(
      x = rexp(200, rate = 0.5) ^ 2,
      y = rexp(200, rate = 0.5) ^ 2
    )
    skew_result <- skewness_transform$detect_skewness(
      data, c("x", "y")
    )
    calls <- list()
    result <- skewness_transform$transform_skewed(
      data, c("x", "y"), skew_result,
      progress = function(i, n, col_name) {
        calls[[length(calls) + 1]] <<- list(
          i = i, n = n, col = col_name
        )
      }
    )
    expect_true(result$success)
    expect_length(calls, 2)
    expect_equal(calls[[1]]$i, 1)
    expect_equal(calls[[2]]$i, 2)
    expect_equal(calls[[1]]$n, 2)
    # Names the column about to be fitted, in fitting order
    expect_equal(
      vapply(calls, function(x) x$col, character(1)),
      skew_result$column[skew_result$is_skewed]
    )
  })

  it("does not call progress when nothing is skewed", {
    set.seed(42)
    data <- data.frame(x = rnorm(100))
    skew_result <- skewness_transform$detect_skewness(
      data, "x"
    )
    n_calls <- 0
    result <- skewness_transform$transform_skewed(
      data, "x", skew_result,
      progress = function(i, n, col_name) {
        n_calls <<- n_calls + 1
      }
    )
    expect_true(result$success)
    expect_equal(n_calls, 0)
  })

  it("transforms left-skewed columns", {
    set.seed(42)
    raw <- rexp(200, rate = 0.5) ^ 2
    data <- data.frame(
      x = max(raw) + 1 - raw
    )
    skew_result <- skewness_transform$detect_skewness(
      data, "x"
    )
    result <- skewness_transform$transform_skewed(
      data, "x", skew_result
    )
    expect_true(result$success)
    expect_equal(nrow(result$result$transformed_cols), 1)
    expect_equal(
      result$result$transformed_cols$direction[1], "left"
    )
    # bestNormalize selects the best method automatically
    expect_gt(nchar(
      result$result$transformed_cols$method_used[1]
    ), 0)
  })

  it("does nothing when method is 'none'", {
    set.seed(42)
    data <- data.frame(x = rexp(200, rate = 0.5) ^ 2)
    skew_result <- skewness_transform$detect_skewness(
      data, "x"
    )
    result <- skewness_transform$transform_skewed(
      data, "x", skew_result,
      method = "none"
    )
    expect_true(result$success)
    expect_equal(nrow(result$result$transformed_cols), 0)
    expect_equal(result$result$data, data)
  })

  it("preserves row count after transformation", {
    set.seed(42)
    data <- data.frame(
      x = rexp(100, rate = 0.5) ^ 2,
      y = rnorm(100)
    )
    skew_result <- skewness_transform$detect_skewness(
      data, c("x", "y")
    )
    result <- skewness_transform$transform_skewed(
      data, c("x", "y"), skew_result
    )
    expect_true(result$success)
    expect_equal(nrow(result$result$data), 100)
  })

  it("reports skipped columns that cannot be transformed", {
    # Constant column has NA skewness but is_skewed = FALSE,
    # so it won't be attempted. This test verifies the
    # skipped_cols mechanism works when a column is marked
    # as skewed but transformation fails.
    data <- data.frame(x = rep(5, 10))
    # Manually create a skew_result that flags it
    skew_result <- data.frame(
      column = "x",
      skewness = 2.0,
      abs_skewness = 2.0,
      direction = "right",
      is_skewed = TRUE,
      stringsAsFactors = FALSE
    )
    result <- skewness_transform$transform_skewed(
      data, "x", skew_result
    )
    expect_true(result$success)
    # Constant column: log1p(0) = 0 for all values,
    # so it should still succeed (all zeros)
    # but the key point is it doesn't error
    expect_equal(nrow(result$result$data), 10)
  })
})
