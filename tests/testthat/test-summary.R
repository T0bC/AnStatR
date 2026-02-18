box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/summary,
)

impl <- attr(summary, "namespace")

# =============================================================================
# Test data helpers
# =============================================================================

make_test_data <- function() {
  data.frame(
    SPECIES = rep(c("A", "B"), each = 10),
    SITE    = rep(c("X", "Y"), times = 10),
    Asfc    = c(rnorm(10, 5, 1), rnorm(10, 8, 2)),
    epLsar  = c(rnorm(10, 0.01, 0.002), rnorm(10, 0.02, 0.003)),
    stringsAsFactors = FALSE
  )
}

make_flagged_data <- function() {
  df <- data.frame(
    SPECIES = rep("A", 10),
    Asfc    = 1:10,
    Asfc_outlier = c(rep(FALSE, 8), TRUE, TRUE),
    Asfc_trimmed = c(rep(FALSE, 7), TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  df
}

# =============================================================================
# validate_inputs
# =============================================================================

describe("validate_inputs", {
  it("returns valid = TRUE for valid grouping columns", {
    data <- make_test_data()
    result <- summary$validate_inputs(c("SPECIES"), data)
    expect_true(result$valid)
  })

  it("returns valid = FALSE when no columns selected", {
    data <- make_test_data()
    result <- summary$validate_inputs(NULL, data)
    expect_true(!result$valid)
  })

  it("returns valid = FALSE for missing columns", {
    data <- make_test_data()
    result <- summary$validate_inputs(c("SPECIES", "MISSING"), data)
    expect_true(!result$valid)
  })
})

# =============================================================================
# get_filtered_values
# =============================================================================

describe("get_filtered_values", {
  it("returns all values when no flag columns exist", {
    data <- data.frame(x = c(1, 2, 3, NA))
    result <- summary$get_filtered_values(data$x, data, "x")
    expect_equal(result, c(1, 2, 3))
  })

  it("excludes outliers and trimmed values", {
    df <- make_flagged_data()
    result <- summary$get_filtered_values(
      df$Asfc, df, "Asfc"
    )
    # rows 8 (trimmed), 9 (outlier), 10 (outlier) excluded
    expect_equal(result, 1:7)
  })
})

# =============================================================================
# compute_base_stats
# =============================================================================

describe("compute_base_stats", {
  it("computes correct stats for normal input", {
    vals <- c(2, 4, 6, 8, 10)
    result <- impl$compute_base_stats(vals)
    expect_equal(result$n, 5)
    expect_equal(result$mean, 6)
    expect_equal(result$median, 6)
    expect_true(!is.na(result$sd))
    expect_true(!is.na(result$sem))
    expect_true(!is.na(result$cv))
  })

  it("returns NA for single value", {
    result <- impl$compute_base_stats(c(5))
    expect_equal(result$n, 1)
    expect_equal(result$mean, 5)
    expect_true(is.na(result$sd))
    expect_true(is.na(result$sem))
  })

  it("returns NA for empty input", {
    result <- impl$compute_base_stats(numeric(0))
    expect_equal(result$n, 0)
    expect_true(is.na(result$mean))
  })
})

# =============================================================================
# compute_shapiro
# =============================================================================

describe("compute_shapiro", {
  it("returns test results for valid data", {
    set.seed(42)
    result <- impl$compute_shapiro(rnorm(50))
    expect_true(!is.na(result$shapiro_p))
    expect_true(!is.na(result$shapiro_W))
    expect_true(result$normal %in% c("yes", "no"))
  })

  it("returns NA for too few values", {
    result <- impl$compute_shapiro(c(1, 2))
    expect_true(is.na(result$shapiro_p))
  })

  it("returns 'identical values' for constant data", {
    result <- impl$compute_shapiro(rep(5, 10))
    expect_equal(result$normal, "identical values")
  })
})

# =============================================================================
# summarize_data
# =============================================================================

describe("summarize_data", {
  it("produces one row per group per measurement", {
    data <- make_test_data()
    result <- summary$summarize_data(
      data, "SPECIES", c("Asfc", "epLsar")
    )
    # 2 species x 2 measurements = 4 rows
    expect_equal(nrow(result), 4)
    expect_true("Measurement" %in% names(result))
    expect_true("n" %in% names(result))
  })

  it("includes shapiro columns when requested", {
    data <- make_test_data()
    result <- summary$summarize_data(
      data, "SPECIES", c("Asfc"), shapiro_test = TRUE
    )
    expect_true("shapiro_p" %in% names(result))
    expect_true("normal" %in% names(result))
  })

  it("respects outlier/trimmed flags", {
    df <- make_flagged_data()
    result <- summary$summarize_data(
      df, "SPECIES", c("Asfc")
    )
    # 7 retained values (1:7)
    expect_equal(result$n, 7)
    expect_equal(result$n_outliers, 2)
    expect_equal(result$n_trimmed, 1)
  })
})

# =============================================================================
# split_by_measurement
# =============================================================================

describe("split_by_measurement", {
  it("splits into per-measurement list and removes Measurement col", {
    data <- make_test_data()
    summary_df <- summary$summarize_data(
      data, "SPECIES", c("Asfc", "epLsar")
    )
    result <- summary$split_by_measurement(summary_df)
    expect_equal(length(result), 2)
    expect_true(is.null(result[[1]]$df$Measurement))
    expect_true(result[[1]]$col %in% c("Asfc", "epLsar"))
  })

  it("drops n_outliers/n_trimmed if all zeros", {
    data <- make_test_data()
    summary_df <- summary$summarize_data(
      data, "SPECIES", c("Asfc")
    )
    result <- summary$split_by_measurement(summary_df)
    # No flags in test data → columns should be dropped
    expect_true(is.null(result[[1]]$df$n_outliers))
    expect_true(is.null(result[[1]]$df$n_trimmed))
  })
})

# =============================================================================
# run_summary
# =============================================================================

describe("run_summary", {
  it("returns success with valid inputs", {
    data <- make_test_data()
    result <- summary$run_summary(data, "SPECIES")
    expect_true(result$success)
    expect_true(length(result$result) > 0)
  })

  it("returns error for invalid grouping columns", {
    data <- make_test_data()
    result <- summary$run_summary(data, "NONEXISTENT")
    expect_true(!result$success)
  })

  it("returns error when no measurement columns exist", {
    data <- data.frame(SPECIES = c("A", "B"), SITE = c("X", "Y"))
    result <- summary$run_summary(data, "SPECIES")
    expect_true(!result$success)
  })
})
