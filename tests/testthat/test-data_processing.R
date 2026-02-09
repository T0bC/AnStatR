box::use(
  testthat[describe, expect_equal, expect_error, expect_true,
           expect_false, expect_length, it],
)

box::use(
  app/logic/plotting/data_processing,
)

# =============================================================================
# Helper data
# =============================================================================

make_grouped_df <- function() {
  data.frame(
    group = rep(c("A", "B"), each = 10),
    value = c(
      1, 2, 3, 4, 5, 6, 7, 8, 9, 100,
      10, 20, 30, 40, 50, 60, 70, 80, 90, 1000
    ),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# detect_outliers
# =============================================================================

describe("detect_outliers", {
  df <- make_grouped_df()
  grp <- factor(df$group)

  it("detects outliers with IQR method", {
    result <- data_processing$detect_outliers(
      df, "value", grp, method = "IQR", factor = 1.5
    )
    expect_length(result, 20)
    expect_true(is.logical(result))
    # 100 should be an outlier in group A
    expect_true(result[10])
    # 1000 should be an outlier in group B
    expect_true(result[20])
  })

  it("detects outliers with zscore method", {
    result <- data_processing$detect_outliers(
      df, "value", grp, method = "zscore", factor = 2.0
    )
    expect_true(is.logical(result))
    expect_true(result[10])
  })

  it("detects outliers with modified_zscore method", {
    result <- data_processing$detect_outliers(
      df, "value", grp, method = "modified_zscore", factor = 3.5
    )
    expect_true(is.logical(result))
    expect_true(result[10])
  })

  it("returns all FALSE for too few data points", {
    small_df <- data.frame(value = c(1, 2))
    grp_small <- factor(c("A", "A"))
    result <- data_processing$detect_outliers(
      small_df, "value", grp_small, method = "IQR"
    )
    expect_true(all(!result))
  })

  it("handles NaN and Inf gracefully", {
    df_special <- data.frame(
      value = c(1, 2, 3, 4, 5, NaN, Inf, -Inf, 8, 9)
    )
    grp_one <- factor(rep("A", 10))
    result <- data_processing$detect_outliers(
      df_special, "value", grp_one, method = "IQR"
    )
    expect_length(result, 10)
    expect_true(is.logical(result))
  })

  it("errors on invalid method", {
    expect_error(
      data_processing$detect_outliers(
        df, "value", grp, method = "invalid"
      ),
      "Invalid method"
    )
  })

  it("errors on missing column", {
    expect_error(
      data_processing$detect_outliers(
        df, "nonexistent", grp, method = "IQR"
      ),
      "not found"
    )
  })

  it("detects outliers with kde method", {
    result <- data_processing$detect_outliers(
      df, "value", grp, method = "kde", factor = 0.1
    )
    expect_true(is.logical(result))
    expect_length(result, 20)
  })
})

# =============================================================================
# mark_trimmed
# =============================================================================

describe("mark_trimmed", {
  it("returns all FALSE when trim_percent is 0", {
    values <- 1:10
    grp <- factor(rep("A", 10))
    result <- data_processing$mark_trimmed(values, grp, 0)
    expect_true(all(!result))
  })

  it("trims correct number from each end", {
    values <- 1:10
    grp <- factor(rep("A", 10))
    # 20% of 10 = 2 from each end
    result <- data_processing$mark_trimmed(values, grp, 20)
    expect_equal(sum(result), 4)
    # Lowest 2 (1, 2) and highest 2 (9, 10) should be trimmed
    expect_true(result[1])
    expect_true(result[2])
    expect_true(result[9])
    expect_true(result[10])
    expect_false(result[5])
  })

  it("trims within each group independently", {
    values <- c(1, 2, 3, 4, 5, 10, 20, 30, 40, 50)
    grp <- factor(c(rep("A", 5), rep("B", 5)))
    # 20% of 5 = 1 from each end per group
    result <- data_processing$mark_trimmed(values, grp, 20)
    expect_equal(sum(result), 4)
  })

  it("handles single-element groups", {
    values <- c(1)
    grp <- factor("A")
    result <- data_processing$mark_trimmed(values, grp, 20)
    expect_equal(sum(result), 0)
  })

  it("caps at 50%", {
    values <- 1:10
    grp <- factor(rep("A", 10))
    result <- data_processing$mark_trimmed(values, grp, 100)
    # 50% of 10 = 5 from each end = all 10 trimmed
    expect_equal(sum(result), 10)
  })
})

# =============================================================================
# process_data
# =============================================================================

describe("process_data", {
  df <- data.frame(
    species = rep(c("cat", "dog"), each = 10),
    measurement_A = c(
      1, 2, 3, 4, 5, 6, 7, 8, 9, 100,
      10, 20, 30, 40, 50, 60, 70, 80, 90, 1000
    ),
    measurement_B = c(
      10, 20, 30, 40, 50, 60, 70, 80, 90, 500,
      5, 15, 25, 35, 45, 55, 65, 75, 85, 800
    ),
    stringsAsFactors = FALSE
  )

  it("returns data unchanged with no measure cols", {
    result <- data_processing$process_data(
      df, character(0), "species"
    )
    expect_equal(ncol(result), ncol(df))
  })

  it("adds _outlier and _trimmed columns per measure", {
    result <- data_processing$process_data(
      df, c("measurement_A", "measurement_B"), "species",
      trim_percent = 0,
      outlier_options = list(enabled = FALSE)
    )
    expect_true("measurement_A_outlier" %in% names(result))
    expect_true("measurement_A_trimmed" %in% names(result))
    expect_true("measurement_B_outlier" %in% names(result))
    expect_true("measurement_B_trimmed" %in% names(result))
  })

  it("flags are all FALSE when processing disabled", {
    result <- data_processing$process_data(
      df, "measurement_A", "species",
      trim_percent = 0,
      outlier_options = list(enabled = FALSE)
    )
    expect_true(all(!result$measurement_A_outlier))
    expect_true(all(!result$measurement_A_trimmed))
  })

  it("detects outliers when enabled", {
    result <- data_processing$process_data(
      df, "measurement_A", "species",
      trim_percent = 0,
      outlier_options = list(
        enabled = TRUE, method = "IQR", factor = 1.5
      )
    )
    # 100 (row 10) and 1000 (row 20) should be outliers
    expect_true(result$measurement_A_outlier[10])
    expect_true(result$measurement_A_outlier[20])
    # Middle values should not be outliers
    expect_false(result$measurement_A_outlier[5])
  })

  it("applies trimming only to non-outlier rows", {
    result <- data_processing$process_data(
      df, "measurement_A", "species",
      trim_percent = 20,
      outlier_options = list(
        enabled = TRUE, method = "IQR", factor = 1.5
      )
    )
    # Outlier rows should NOT be marked as trimmed
    outlier_rows <- which(result$measurement_A_outlier)
    expect_true(all(!result$measurement_A_trimmed[outlier_rows]))
  })

  it("applies trimming without outlier detection", {
    result <- data_processing$process_data(
      df, "measurement_A", "species",
      trim_percent = 20,
      outlier_options = list(enabled = FALSE)
    )
    expect_true(any(result$measurement_A_trimmed))
    expect_true(all(!result$measurement_A_outlier))
  })

  it("handles missing x_cols gracefully (single group)", {
    result <- data_processing$process_data(
      df, "measurement_A", character(0),
      trim_percent = 0,
      outlier_options = list(
        enabled = TRUE, method = "IQR", factor = 1.5
      )
    )
    expect_true("measurement_A_outlier" %in% names(result))
  })

  it("skips measure cols not in data", {
    result <- data_processing$process_data(
      df, c("measurement_A", "nonexistent"), "species",
      outlier_options = list(enabled = FALSE)
    )
    expect_true("measurement_A_outlier" %in% names(result))
    expect_false("nonexistent_outlier" %in% names(result))
  })
})
