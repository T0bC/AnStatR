box::use(
  testthat[describe, expect_equal, expect_false, expect_true, it],
)

box::use(
  app/logic/load_data/data_overview,
)

# A small fixture mirroring the shapes seen in real uploads: a duplicate
# name differing only by case, a constant column, a column that is mostly
# missing, and two measurement columns with scattered gaps.
sample_data <- function() {
  data.frame(
    SPECIES = c("A", "A", "B", "B", "C", "C"),
    SITE = c("N", "N", "N", "N", "N", "N"),
    FDI = c(1, 2, NA, NA, 5, 6),
    fdi = c(1, 2, NA, NA, 5, 6),
    LOT = c(NA, NA, NA, NA, 1, 1),
    Sq = c(0.1, NA, 0.3, 0.4, 0.5, 0.6),
    Sa = c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6),
    stringsAsFactors = FALSE
  )
}

describe("compute_overview_stats", {
  it("counts rows, columns and missing cells", {
    stats <- data_overview$compute_overview_stats(sample_data())
    expect_equal(stats$n_rows, 6)
    expect_equal(stats$n_cols, 7)
    expect_equal(stats$n_cells, 42)
    expect_equal(stats$n_missing_cells, 9)
  })

  it("reports the percentage of missing cells", {
    stats <- data_overview$compute_overview_stats(sample_data())
    expect_equal(stats$pct_missing_cells, round(100 * 9 / 42, 1))
  })

  it("counts rows with no gaps at all", {
    stats <- data_overview$compute_overview_stats(sample_data())
    expect_equal(stats$n_complete_rows, 2)
    expect_equal(stats$pct_complete_rows, round(100 * 2 / 6, 1))
  })

  it("splits columns using the app-wide naming convention", {
    stats <- data_overview$compute_overview_stats(sample_data())
    # SPECIES, SITE, FDI, LOT are uppercase-only; Sq, Sa, fdi are not.
    expect_equal(stats$n_descriptive, 4)
    expect_equal(stats$n_measurement, 3)
    expect_equal(stats$n_ambiguous, 0)
  })

  it("handles a zero-row data frame", {
    stats <- data_overview$compute_overview_stats(
      data.frame(A = numeric(0))
    )
    expect_equal(stats$n_rows, 0)
    expect_equal(stats$pct_missing_cells, 0)
    expect_equal(stats$pct_complete_rows, 0)
  })

  it("handles an empty data frame", {
    stats <- data_overview$compute_overview_stats(data.frame())
    expect_equal(stats$n_rows, 0)
    expect_equal(stats$n_cols, 0)
  })
})

describe("detect_quality_flags", {
  flag_types <- function(result) {
    vapply(result$flags, function(f) f$type, character(1))
  }
  flag_cols <- function(result, type) {
    idx <- which(flag_types(result) == type)
    if (length(idx) == 0) return(character(0))
    result$flags[[idx[1]]]$columns
  }

  it("flags column names that differ only by capitalisation", {
    result <- data_overview$detect_quality_flags(sample_data())
    expect_true(result$has_issues)
    # Sorted with a fixed collation so the test does not depend on the
    # machine locale, where "fdi" may order before or after "FDI".
    expect_equal(
      sort(flag_cols(result, "duplicate_names"), method = "radix"),
      c("FDI", "fdi")
    )
  })

  it("flags columns with a single value throughout", {
    result <- data_overview$detect_quality_flags(sample_data())
    expect_equal(flag_cols(result, "constant_cols"), "SITE")
  })

  it("flags columns at or above the missing threshold", {
    result <- data_overview$detect_quality_flags(sample_data())
    expect_equal(flag_cols(result, "high_missing"), "LOT")
  })

  it("does not repeat a mostly-missing column as constant", {
    # LOT is both >50% missing and single-valued; it should be
    # reported once, as mostly missing.
    result <- data_overview$detect_quality_flags(sample_data())
    expect_false("LOT" %in% flag_cols(result, "constant_cols"))
  })

  it("flags an entirely empty column", {
    df <- data.frame(A = c(1, 2), B = c(NA, NA))
    result <- data_overview$detect_quality_flags(df)
    expect_equal(flag_cols(result, "empty_cols"), "B")
  })

  it("reports no issues for clean data", {
    df <- data.frame(SPECIES = c("A", "B"), Sq = c(1.1, 2.2))
    result <- data_overview$detect_quality_flags(df)
    expect_false(result$has_issues)
    expect_equal(length(result$flags), 0)
  })

  it("handles an empty data frame", {
    result <- data_overview$detect_quality_flags(data.frame())
    expect_false(result$has_issues)
  })

  it("respects a custom missing threshold", {
    result <- data_overview$detect_quality_flags(
      sample_data(),
      high_missing_threshold = 0.2
    )
    expect_true("FDI" %in% flag_cols(result, "high_missing"))
  })
})

describe("missing_by_column", {
  it("excludes columns that have no missing values", {
    result <- data_overview$missing_by_column(sample_data())
    expect_false("Sa" %in% result$data$column)
    expect_false("SPECIES" %in% result$data$column)
  })

  it("sorts affected columns by missing count, descending", {
    result <- data_overview$missing_by_column(sample_data())
    expect_equal(result$data$column[1], "LOT")
    expect_true(all(diff(result$data$n_missing) <= 0))
  })

  it("reports how many columns are complete", {
    result <- data_overview$missing_by_column(sample_data())
    expect_equal(result$n_total_cols, 7)
    expect_equal(result$n_complete_cols, 3)
    expect_equal(nrow(result$data), 4)
  })

  it("returns no rows for data without missing values", {
    df <- data.frame(A = c(1, 2), B = c(3, 4))
    result <- data_overview$missing_by_column(df)
    expect_equal(nrow(result$data), 0)
    expect_equal(result$n_complete_cols, 2)
  })

  it("handles an empty data frame", {
    result <- data_overview$missing_by_column(data.frame())
    expect_equal(nrow(result$data), 0)
  })
})

describe("missing_patterns", {
  it("groups rows that share an identical NA signature", {
    result <- data_overview$missing_patterns(sample_data())
    # Rows 3 and 4 are missing FDI, fdi and LOT together.
    top <- result$data[result$data$n_rows == 2 &
                         result$data$n_cols_missing == 3, ]
    expect_equal(nrow(top), 1)
  })

  it("labels the fully complete rows", {
    result <- data_overview$missing_patterns(sample_data())
    expect_true("Complete rows" %in% result$data$label)
  })

  it("limits output to top_n patterns and reports the remainder", {
    df <- data.frame(
      A = c(NA, 1, 1, 1),
      B = c(1, NA, 1, 1),
      C = c(1, 1, NA, 1)
    )
    result <- data_overview$missing_patterns(df, top_n = 2)
    expect_equal(nrow(result$data), 2)
    expect_equal(result$n_patterns, 4)
    expect_equal(result$n_hidden, 2)
  })

  it("marks scattered missingness where every pattern is unique", {
    df <- data.frame(
      A = c(NA, 1, 1),
      B = c(1, NA, 1),
      C = c(1, 1, NA)
    )
    result <- data_overview$missing_patterns(df)
    expect_true(result$all_singleton)
  })

  it("returns nothing for data without missing values", {
    df <- data.frame(A = c(1, 2), B = c(3, 4))
    result <- data_overview$missing_patterns(df)
    expect_equal(result$n_patterns, 0)
    expect_equal(nrow(result$data), 0)
  })

  it("handles a zero-row data frame", {
    result <- data_overview$missing_patterns(data.frame(A = numeric(0)))
    expect_equal(result$n_patterns, 0)
  })
})

describe("missing_raster_data", {
  it("covers only the NA-bearing columns", {
    result <- data_overview$missing_raster_data(sample_data())
    expect_equal(result$n_na_cols, 4)
    expect_equal(nrow(result$data), 6 * 4)
  })

  it("orders rows so identical patterns are contiguous", {
    result <- data_overview$missing_raster_data(sample_data())
    lot <- result$data[result$data$column == "LOT", ]
    lot <- lot[order(lot$row_id), ]
    # Once the missing LOT rows start they must not be interrupted.
    runs <- rle(lot$is_missing)
    expect_equal(sum(runs$values), 1)
  })

  it("downsamples above max_rows while keeping the column count", {
    df <- data.frame(A = c(rep(NA, 50), 1:50), B = 1:100)
    result <- data_overview$missing_raster_data(df, max_rows = 20)
    expect_true(result$downsampled)
    expect_equal(result$n_rows_total, 100)
    expect_true(result$n_rows_shown <= 20)
    expect_equal(nrow(result$data), result$n_rows_shown * result$n_na_cols)
  })

  it("does not downsample below max_rows", {
    result <- data_overview$missing_raster_data(
      sample_data(),
      max_rows = 1000
    )
    expect_false(result$downsampled)
    expect_equal(result$n_rows_shown, 6)
  })

  it("returns nothing for data without missing values", {
    df <- data.frame(A = c(1, 2), B = c(3, 4))
    result <- data_overview$missing_raster_data(df)
    expect_equal(nrow(result$data), 0)
    expect_equal(result$n_na_cols, 0)
  })

  it("handles a zero-row data frame", {
    result <- data_overview$missing_raster_data(data.frame(A = numeric(0)))
    expect_equal(nrow(result$data), 0)
  })
})

describe("na_bearing_cols", {
  it("returns only columns containing at least one NA", {
    expect_equal(
      data_overview$na_bearing_cols(sample_data()),
      c("FDI", "fdi", "LOT", "Sq")
    )
  })

  it("returns an empty vector for an empty data frame", {
    expect_equal(data_overview$na_bearing_cols(data.frame()), character(0))
  })
})
