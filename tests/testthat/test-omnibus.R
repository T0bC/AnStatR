box::use(
  testthat[describe, expect_equal, expect_true, expect_false, it],
)

box::use(
  app/logic/statistics/omnibus,
  app/logic/error_handling,
)

# =============================================================================
# safe_factor_conversion
# =============================================================================

describe("safe_factor_conversion", {
  it("converts valid columns to factors", {
    df <- data.frame(
      group = c("A", "A", "B", "B"),
      value = 1:4,
      stringsAsFactors = FALSE
    )
    result <- omnibus$safe_factor_conversion(df, "group")
    expect_true(result$success)
    expect_true(is.factor(result$data$group))
    expect_true(is.null(result$error))
  })

  it("returns error for missing column", {
    df <- data.frame(group = c("A", "B"), value = 1:2)
    result <- omnibus$safe_factor_conversion(df, "nonexistent")
    expect_false(result$success)
    expect_true(error_handling$is_app_error(result$error))
  })

  it("returns error for all-NA column", {
    df <- data.frame(
      group = c(NA_character_, NA_character_),
      value = 1:2
    )
    result <- omnibus$safe_factor_conversion(df, "group")
    expect_false(result$success)
    expect_true(error_handling$is_app_error(result$error))
  })

  it("returns error for column with fewer than 2 unique values", {
    df <- data.frame(
      group = c("A", "A", "A"),
      value = 1:3,
      stringsAsFactors = FALSE
    )
    result <- omnibus$safe_factor_conversion(df, "group")
    expect_false(result$success)
    expect_true(error_handling$is_app_error(result$error))
  })

  it("returns error for column with only empty strings", {
    df <- data.frame(
      group = c("", " ", "  "),
      value = 1:3,
      stringsAsFactors = FALSE
    )
    result <- omnibus$safe_factor_conversion(df, "group")
    expect_false(result$success)
    expect_true(error_handling$is_app_error(result$error))
  })

  it("converts multiple columns", {
    df <- data.frame(
      g1 = c("A", "A", "B", "B"),
      g2 = c("X", "Y", "X", "Y"),
      value = 1:4,
      stringsAsFactors = FALSE
    )
    result <- omnibus$safe_factor_conversion(df, c("g1", "g2"))
    expect_true(result$success)
    expect_true(is.factor(result$data$g1))
    expect_true(is.factor(result$data$g2))
  })
})

# =============================================================================
# calculate_smallest_group
# =============================================================================

describe("calculate_smallest_group", {
  it("returns smallest group size for single factor", {
    df <- data.frame(
      group = c("A", "A", "A", "B", "B"),
      value = 1:5
    )
    result <- omnibus$calculate_smallest_group(df, "group")
    expect_equal(result, 2)
  })

  it("returns smallest group size for two factors", {
    df <- data.frame(
      g1 = c("A", "A", "A", "B", "B", "B"),
      g2 = c("X", "X", "Y", "X", "Y", "Y"),
      value = 1:6
    )
    result <- omnibus$calculate_smallest_group(df, c("g1", "g2"))
    expect_equal(result, 1)
  })
})

# =============================================================================
# setup_bootstrap_params
# =============================================================================

describe("setup_bootstrap_params", {
  it("returns 1 iteration when bootstrap is off", {
    df <- data.frame(
      group = c("A", "A", "B", "B"),
      value = 1:4
    )
    result <- omnibus$setup_bootstrap_params(
      df, "group",
      use_bootstrap = FALSE,
      boot_samples = 599,
      boot_sample_size = NULL
    )
    expect_equal(result$n_iterations, 1)
    expect_true(is.null(result$sample_size))
  })

  it("returns correct params when bootstrap is on", {
    df <- data.frame(
      group = c("A", "A", "A", "B", "B"),
      value = 1:5
    )
    result <- omnibus$setup_bootstrap_params(
      df, "group",
      use_bootstrap = TRUE,
      boot_samples = 100,
      boot_sample_size = NULL
    )
    expect_equal(result$n_iterations, 100)
    expect_equal(result$sample_size, 2)
  })

  it("caps sample_size to smallest group", {
    df <- data.frame(
      group = c("A", "A", "B", "B", "B"),
      value = 1:5
    )
    result <- omnibus$setup_bootstrap_params(
      df, "group",
      use_bootstrap = TRUE,
      boot_samples = 50,
      boot_sample_size = 999
    )
    expect_equal(result$sample_size, 2)
  })
})

# =============================================================================
# format_bootstrap_results
# =============================================================================

describe("format_bootstrap_results", {
  it("formats valid bootstrap results", {
    boot_df <- data.frame(
      F_stat = c(3.1, 3.5, 2.9, 3.3),
      p_value = c(0.04, 0.03, 0.05, 0.04)
    )
    result <- omnibus$format_bootstrap_results(boot_df)
    expect_true(is.data.frame(result))
    expect_equal(names(result), c("F_stat", "p_value"))
    expect_true(grepl("\\[", result$F_stat))
  })

  it("returns app_error for empty input", {
    result <- omnibus$format_bootstrap_results(
      data.frame()
    )
    expect_true(error_handling$is_app_error(result))
  })

  it("returns app_error for all-NA columns", {
    boot_df <- data.frame(
      F_stat = c(NA_real_, NA_real_),
      p_value = c(NA_real_, NA_real_)
    )
    result <- omnibus$format_bootstrap_results(boot_df)
    expect_true(error_handling$is_app_error(result))
  })
})
