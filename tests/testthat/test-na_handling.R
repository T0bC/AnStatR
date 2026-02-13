box::use(
  testthat[describe, expect_equal, expect_false, expect_true,
           it],
)

box::use(
  app/logic/pca/na_handling,
)

# =============================================================================
# analyse_na
# =============================================================================

describe("analyse_na", {
  it("returns empty data frame when no NAs present", {
    data <- data.frame(a = 1:5, b = 6:10)
    result <- na_handling$analyse_na(data, c("a", "b"))
    expect_equal(nrow(result), 0)
    expect_true("column" %in% names(result))
    expect_true("na_count" %in% names(result))
    expect_true("na_percent" %in% names(result))
  })

  it("detects NAs in measurement columns", {
    data <- data.frame(
      a = c(1, NA, 3, NA, 5),
      b = c(1, 2, 3, 4, 5)
    )
    result <- na_handling$analyse_na(data, c("a", "b"))
    expect_equal(nrow(result), 1)
    expect_equal(result$column, "a")
    expect_equal(result$na_count, 2L)
    expect_equal(result$na_percent, 40.0)
  })

  it("sorts by na_count descending", {
    data <- data.frame(
      a = c(1, NA, 3, 4, 5),
      b = c(NA, NA, NA, 4, 5),
      c = c(1, 2, 3, 4, 5)
    )
    result <- na_handling$analyse_na(data, c("a", "b", "c"))
    expect_equal(nrow(result), 2)
    expect_equal(result$column[1], "b")
    expect_equal(result$column[2], "a")
  })

  it("only analyses selected measurement columns", {
    data <- data.frame(
      meta = c("x", NA, "z"),
      measure = c(1, NA, 3)
    )
    result <- na_handling$analyse_na(data, "measure")
    expect_equal(nrow(result), 1)
    expect_equal(result$column, "measure")
  })
})

# =============================================================================
# clean_na_rows
# =============================================================================

describe("clean_na_rows", {
  it("returns unchanged data when no NAs", {
    data <- data.frame(a = 1:5, b = 6:10)
    result <- na_handling$clean_na_rows(data, c("a", "b"))
    expect_equal(result$rows_before, 5)
    expect_equal(result$rows_after, 5)
    expect_equal(result$rows_removed, 0)
    expect_equal(nrow(result$data), 5)
    expect_equal(nrow(result$na_summary), 0)
  })

  it("removes rows with NAs in measurement columns", {
    data <- data.frame(
      meta = c("x", "y", "z", "w"),
      a = c(1, NA, 3, 4),
      b = c(1, 2, NA, 4)
    )
    result <- na_handling$clean_na_rows(
      data, c("a", "b")
    )
    expect_equal(result$rows_before, 4)
    expect_equal(result$rows_removed, 2)
    expect_equal(result$rows_after, 2)
    expect_equal(nrow(result$data), 2)
  })

  it("preserves all columns including metadata", {
    data <- data.frame(
      meta = c("x", "y", "z"),
      a = c(1, NA, 3)
    )
    result <- na_handling$clean_na_rows(data, "a")
    expect_true("meta" %in% names(result$data))
    expect_true("a" %in% names(result$data))
    expect_equal(ncol(result$data), 2)
  })

  it("ignores NAs in non-measurement columns", {
    data <- data.frame(
      meta = c(NA, "y", "z"),
      a = c(1, 2, 3)
    )
    result <- na_handling$clean_na_rows(data, "a")
    expect_equal(result$rows_removed, 0)
    expect_equal(nrow(result$data), 3)
    expect_true(is.na(result$data$meta[1]))
  })

  it("returns na_summary with column details", {
    data <- data.frame(
      a = c(1, NA, 3, NA, 5),
      b = c(NA, 2, 3, 4, 5)
    )
    result <- na_handling$clean_na_rows(
      data, c("a", "b")
    )
    expect_true(nrow(result$na_summary) > 0)
    expect_true("a" %in% result$na_summary$column)
    expect_true("b" %in% result$na_summary$column)
  })

  it("handles all rows removed", {
    data <- data.frame(
      a = c(NA, NA),
      b = c(1, NA)
    )
    result <- na_handling$clean_na_rows(
      data, c("a", "b")
    )
    expect_equal(result$rows_after, 0)
    expect_equal(result$rows_removed, 2)
    expect_equal(nrow(result$data), 0)
  })

  it("returns empty meta_na_summary when no meta_cols given", {
    data <- data.frame(a = 1:5, b = 6:10)
    result <- na_handling$clean_na_rows(data, c("a", "b"))
    expect_equal(nrow(result$meta_na_summary), 0)
  })

  it("reports NAs in descriptive columns", {
    data <- data.frame(
      sex = c("M", NA, "F", NA, "M"),
      site = c(NA, "A", "B", "A", "B"),
      a = c(1, 2, 3, 4, 5)
    )
    result <- na_handling$clean_na_rows(
      data, "a", meta_cols = c("sex", "site")
    )
    expect_equal(result$rows_removed, 0)
    expect_equal(nrow(result$meta_na_summary), 2)
    expect_true("sex" %in% result$meta_na_summary$column)
    expect_true("site" %in% result$meta_na_summary$column)
  })

  it("does not remove rows for descriptive column NAs", {
    data <- data.frame(
      sex = c(NA, NA, NA),
      a = c(1, 2, 3)
    )
    result <- na_handling$clean_na_rows(
      data, "a", meta_cols = "sex"
    )
    expect_equal(result$rows_removed, 0)
    expect_equal(nrow(result$data), 3)
    expect_equal(nrow(result$meta_na_summary), 1)
    expect_equal(result$meta_na_summary$na_count, 3L)
  })
})
