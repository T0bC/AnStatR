box::use(
  testthat[describe, expect_equal, expect_null, expect_true, it],
)

box::use(
  app/logic/plotting/assumption_checks,
  app/logic/preprocessing/normalize,
)

# =============================================================================
# Test data helpers
# =============================================================================

make_nonnormal_data <- function() {
  set.seed(42)
  data.frame(
    SPECIES = rep(c("A", "B", "C"), each = 50),
    value = c(rexp(50, 0.5), rexp(50, 0.3), rexp(50, 0.1)),
    value_outlier = FALSE,
    value_trimmed = FALSE,
    stringsAsFactors = FALSE
  )
}

make_normal_data <- function() {
  set.seed(42)
  data.frame(
    SPECIES = rep(c("A", "B"), each = 50),
    value = c(rnorm(50, 10, 2), rnorm(50, 12, 2)),
    value_outlier = FALSE,
    value_trimmed = FALSE,
    stringsAsFactors = FALSE
  )
}

make_normality_results_nonnormal <- function(df) {
  grp <- factor(df$SPECIES)
  list(
    value = assumption_checks$check_normality(df, "value", grp)
  )
}

make_normality_results_normal <- function(df) {
  grp <- factor(df$SPECIES)
  list(
    value = assumption_checks$check_normality(df, "value", grp)
  )
}

# =============================================================================
# normalize_columns
# =============================================================================

describe("normalize_columns", {
  it("adds _normalized column for non-normal data", {
    df <- make_nonnormal_data()
    norm_results <- make_normality_results_nonnormal(df)

    result <- normalize$normalize_columns(
      data = df,
      measure_cols = "value",
      normality_results = norm_results,
      threshold = 0.5
    )

    expect_true("value_normalized" %in% names(result$data))
    expect_true(nrow(result$transform_info) > 0)
    expect_equal(result$transform_info$column[1], "value")
  })

  it("does not add _normalized column for normal data", {
    df <- make_normal_data()
    norm_results <- make_normality_results_normal(df)

    result <- normalize$normalize_columns(
      data = df,
      measure_cols = "value",
      normality_results = norm_results,
      threshold = 0.5
    )

    expect_true(!"value_normalized" %in% names(result$data))
    expect_equal(nrow(result$transform_info), 0)
  })

  it("sets NA for outlier-flagged rows", {
    df <- make_nonnormal_data()
    df$value_outlier[c(1, 2, 3)] <- TRUE
    norm_results <- make_normality_results_nonnormal(df)

    result <- normalize$normalize_columns(
      data = df,
      measure_cols = "value",
      normality_results = norm_results,
      threshold = 0.5
    )

    if ("value_normalized" %in% names(result$data)) {
      expect_true(all(is.na(result$data$value_normalized[c(1, 2, 3)])))
      # Non-outlier, non-NA values should have actual transformed values
      clean_idx <- which(!df$value_outlier & !is.na(df$value))
      expect_true(all(!is.na(result$data$value_normalized[clean_idx])))
    }
  })

  it("preserves original raw values", {
    df <- make_nonnormal_data()
    original_values <- df$value
    norm_results <- make_normality_results_nonnormal(df)

    result <- normalize$normalize_columns(
      data = df,
      measure_cols = "value",
      normality_results = norm_results,
      threshold = 0.5
    )

    expect_equal(result$data$value, original_values)
  })

  it("handles empty normality results gracefully", {
    df <- make_normal_data()

    result <- normalize$normalize_columns(
      data = df,
      measure_cols = "value",
      normality_results = list(),
      threshold = 0.5
    )

    expect_true(!"value_normalized" %in% names(result$data))
    expect_equal(nrow(result$transform_info), 0)
  })
})

# =============================================================================
# get_transform_label
# =============================================================================

describe("get_transform_label", {
  it("returns method name for transformed column", {
    info <- data.frame(
      column = "value",
      method = "orderNorm",
      n_transformed = 150L,
      stringsAsFactors = FALSE
    )
    label <- normalize$get_transform_label(info, "value")
    expect_equal(label, "orderNorm")
  })

  it("returns NULL for non-transformed column", {
    info <- data.frame(
      column = "value",
      method = "orderNorm",
      n_transformed = 150L,
      stringsAsFactors = FALSE
    )
    label <- normalize$get_transform_label(info, "other_col")
    expect_null(label)
  })

  it("returns NULL for empty transform_info", {
    label <- normalize$get_transform_label(NULL, "value")
    expect_null(label)
  })
})
