box::use(
  testthat[describe, expect_equal, expect_false, expect_null,
           expect_true, it],
)

box::use(
  app/logic/column_utils,
  app/logic/error_handling,
  app/logic/median/compute,
  app/logic/median/quality_analysis,
  app/logic/median/quality_filter,
)

# =============================================================================
# column_utils: get_descriptive_cols / get_measurement_cols
# =============================================================================

describe("get_descriptive_cols", {
  it("identifies uppercase-only columns as descriptive", {
    df <- data.frame(
      GENUS = "A", SPECIES = "B", Sq = 1, Sa = 2
    )
    result <- column_utils$get_descriptive_cols(df)
    expect_equal(result, c("GENUS", "SPECIES"))
  })

  it("handles columns with underscores (no digits)", {
    df <- data.frame(
      SPEC_ID = "x", IND_AGE_Y = 1, epLsar = 0.5
    )
    result <- column_utils$get_descriptive_cols(df)
    expect_equal(result, c("SPEC_ID", "IND_AGE_Y"))
  })

  it("excludes uppercase columns with digits", {
    df <- data.frame(
      GENUS = "A", SAMPLE_1 = "x", S10z = 1
    )
    result <- column_utils$get_descriptive_cols(df)
    expect_equal(result, "GENUS")
  })

  it("returns empty vector when no descriptive cols", {
    df <- data.frame(x = 1, y = 2)
    result <- column_utils$get_descriptive_cols(df)
    expect_equal(result, character(0))
  })
})

describe("get_measurement_cols", {
  it("identifies mixed-case columns as measurement", {
    df <- data.frame(
      GENUS = "A", Sq = 1, Sa = 2, epLsar = 0.5
    )
    result <- column_utils$get_measurement_cols(df)
    expect_equal(result, c("Sq", "Sa", "epLsar"))
  })

  it("does not include uppercase-with-digits columns", {
    df <- data.frame(
      GENUS = "A", SAMPLE_1 = "x", Sq = 1
    )
    result <- column_utils$get_measurement_cols(df)
    expect_equal(result, "Sq")
  })

  it("returns empty vector when all cols are uppercase", {
    df <- data.frame(GENUS = "A", SPECIES = "B")
    result <- column_utils$get_measurement_cols(df)
    expect_equal(result, character(0))
  })
})

describe("validate_column_naming", {
  it("reports valid when no ambiguous columns", {
    df <- data.frame(GENUS = "A", Sq = 1)
    result <- column_utils$validate_column_naming(df)
    expect_true(result$valid)
    expect_equal(result$descriptive_cols, "GENUS")
    expect_equal(result$measurement_cols, "Sq")
    expect_equal(result$ambiguous_cols, character(0))
  })

  it("detects ambiguous columns with digits", {
    df <- data.frame(
      GENUS = "A", SAMPLE_1 = "x", Sq = 1
    )
    result <- column_utils$validate_column_naming(df)
    expect_false(result$valid)
    expect_equal(result$ambiguous_cols, "SAMPLE_1")
  })
})

# =============================================================================
# column_utils: analyze_quality_column
# =============================================================================

describe("analyze_quality_column", {
  it("returns type 'none' for NULL column", {
    df <- data.frame(A = 1)
    result <- quality_analysis$analyze_quality_column(df, NULL)
    expect_equal(result$type, "none")
  })

  it("returns type 'none' for 'None' column", {
    df <- data.frame(A = 1)
    result <- quality_analysis$analyze_quality_column(df, "None")
    expect_equal(result$type, "none")
  })

  it("detects categorical integer quality grades", {
    df <- data.frame(Q = c(1, 2, 3, 4, 1, 2))
    result <- quality_analysis$analyze_quality_column(df, "Q")
    expect_equal(result$type, "categorical")
    expect_equal(result$n_unique, 4)
  })

  it("detects percentage_decimal type (0-1)", {
    df <- data.frame(Q = seq(0.1, 0.9, by = 0.01))
    result <- quality_analysis$analyze_quality_column(df, "Q")
    expect_equal(result$type, "percentage_decimal")
  })

  it("detects percentage_100 type (0-100)", {
    df <- data.frame(Q = seq(10, 95, by = 0.5))
    result <- quality_analysis$analyze_quality_column(df, "Q")
    expect_equal(result$type, "percentage_100")
  })

  it("detects non-numeric categorical values", {
    df <- data.frame(
      Q = c("good", "bad", "ok", "good"),
      stringsAsFactors = FALSE
    )
    result <- quality_analysis$analyze_quality_column(df, "Q")
    expect_equal(result$type, "categorical")
    expect_equal(result$n_unique, 3)
  })
})

# =============================================================================
# quality_filter: apply_quality_filter
# =============================================================================

describe("apply_quality_filter", {
  it("returns data unchanged when filter is disabled", {
    df <- data.frame(A = 1:3, B = 4:6)
    settings <- list(
      enabled = FALSE, column = NULL, type = "none"
    )
    result <- quality_filter$apply_quality_filter(
      df, settings, NULL
    )
    expect_equal(nrow(result$data), 3)
    expect_true(grepl("No quality filtering", result$message))
  })

  it("returns data unchanged when column not in data", {
    df <- data.frame(A = 1:3)
    settings <- list(
      enabled = TRUE, column = "MISSING", type = "categorical"
    )
    result <- quality_filter$apply_quality_filter(
      df, settings, NULL
    )
    expect_equal(nrow(result$data), 3)
  })
})

# =============================================================================
# quality_filter: filter_categorical
# =============================================================================

describe("filter_categorical", {
  it("removes bad values without grouping", {
    df <- data.frame(
      Q = c("good", "bad", "good", "bad"),
      val = 1:4,
      stringsAsFactors = FALSE
    )
    result <- quality_filter$filter_categorical(
      df, "Q", "bad", NULL
    )
    expect_equal(nrow(result$data), 2)
    expect_equal(result$data$val, c(1, 3))
  })

  it("keeps all-bad groups intact with grouping", {
    df <- data.frame(
      GRP = c("A", "A", "B", "B"),
      Q = c("good", "bad", "bad", "bad"),
      val = 1:4,
      stringsAsFactors = FALSE
    )
    result <- quality_filter$filter_categorical(
      df, "Q", "bad", "GRP"
    )
    # Group A: has good -> remove bad -> keep row 1
    # Group B: all bad -> keep both rows 3,4
    expect_equal(nrow(result$data), 3)
  })
})

# =============================================================================
# quality_filter: filter_numeric
# =============================================================================

describe("filter_numeric", {
  it("removes below-threshold without grouping", {
    df <- data.frame(Q = c(0.9, 0.3, 0.8, 0.2), val = 1:4)
    result <- quality_filter$filter_numeric(
      df, "Q", 0.5, NULL
    )
    expect_equal(nrow(result$data), 2)
    expect_equal(result$data$val, c(1, 3))
  })

  it("keeps all-bad groups intact with grouping", {
    df <- data.frame(
      GRP = c("A", "A", "B", "B"),
      Q = c(0.9, 0.3, 0.2, 0.1),
      val = 1:4
    )
    result <- quality_filter$filter_numeric(
      df, "Q", 0.5, "GRP"
    )
    # Group A: has good (0.9) -> remove 0.3 -> keep row 1
    # Group B: all bad -> keep both rows 3,4
    expect_equal(nrow(result$data), 3)
  })
})

# =============================================================================
# compute: compute_medians
# =============================================================================

describe("compute_medians", {
  it("returns NULL result when no measurement columns", {
    df <- data.frame(GENUS = c("A", "B"), SPECIES = c("x", "y"))
    result <- compute$compute_medians(df, "GENUS")
    expect_true(result$success)
    expect_null(result$result)
  })

  it("returns data as-is without grouping", {
    df <- data.frame(
      GENUS = c("A", "A"), Sq = c(1.0, 2.0), Sa = c(3.0, 4.0)
    )
    result <- compute$compute_medians(df, NULL)
    expect_true(result$success)
    expect_equal(nrow(result$result), 2)
  })

  it("calculates medians with grouping", {
    df <- data.frame(
      GENUS = c("A", "A", "B", "B"),
      Sq = c(1.0, 3.0, 5.0, 7.0),
      Sa = c(10.0, 20.0, 30.0, 40.0)
    )
    result <- compute$compute_medians(df, "GENUS")
    expect_true(result$success)
    expect_equal(nrow(result$result), 2)
    # Median of A: Sq=2, Sa=15; B: Sq=6, Sa=35
    row_a <- result$result[result$result$GENUS == "A", ]
    expect_equal(row_a$Sq, 2.0)
    expect_equal(row_a$Sa, 15.0)
  })

  it("removes descriptive cols that vary within groups", {
    df <- data.frame(
      GENUS = c("A", "A", "B", "B"),
      SITE = c("x", "y", "z", "z"),
      Sq = c(1.0, 3.0, 5.0, 7.0)
    )
    result <- compute$compute_medians(df, "GENUS")
    expect_true(result$success)
    # SITE varies within group A -> should be removed
    expect_true("SITE" %in% result$removed_cols)
    expect_false("SITE" %in% names(result$result))
  })

  it("keeps constant descriptive cols in output", {
    df <- data.frame(
      GENUS = c("A", "A", "B", "B"),
      COUNTRY = c("ZAF", "ZAF", "USA", "USA"),
      Sq = c(1.0, 3.0, 5.0, 7.0)
    )
    result <- compute$compute_medians(df, "GENUS")
    expect_true(result$success)
    expect_true("COUNTRY" %in% names(result$result))
    expect_equal(result$removed_cols, character(0))
  })

  it("excludes quality column from output", {
    df <- data.frame(
      GENUS = c("A", "A"), QUALITY = c(1, 2),
      Sq = c(1.0, 3.0)
    )
    result <- compute$compute_medians(
      df, "GENUS", quality_col = "QUALITY"
    )
    expect_true(result$success)
    expect_false("QUALITY" %in% names(result$result))
  })

  it("rounds measurement columns to 4 decimals", {
    df <- data.frame(
      GENUS = c("A", "A"),
      Sq = c(1.123456789, 2.987654321)
    )
    result <- compute$compute_medians(df, "GENUS")
    expect_true(result$success)
    # Median = (1.123456789 + 2.987654321) / 2 = 2.055555555
    expect_equal(result$result$Sq, 2.0556)
  })
})
