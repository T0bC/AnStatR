box::use(
  testthat[describe, expect_equal, expect_true, expect_false, it],
)

box::use(
  app/logic/shared/error_handling,
  app/logic/statistics/parametric_posthoc,
)

# =============================================================================
# Helper: create test data
# =============================================================================

make_oneway_data <- function(n_per_group = 20, n_groups = 3) {
  set.seed(42)
  groups <- rep(
    paste0("G", seq_len(n_groups)), each = n_per_group
  )
  values <- rnorm(
    n_per_group * n_groups,
    mean = rep(1:n_groups, each = n_per_group)
  )
  data.frame(
    group = groups,
    measure = values,
    stringsAsFactors = FALSE
  )
}

make_twoway_data <- function(n_per_cell = 10) {
  set.seed(42)
  grid <- expand.grid(
    f1 = c("A", "B"),
    f2 = c("X", "Y"),
    stringsAsFactors = FALSE
  )
  df <- grid[rep(seq_len(nrow(grid)), each = n_per_cell), ]
  df$measure <- rnorm(nrow(df)) +
    ifelse(df$f1 == "B", 1, 0) +
    ifelse(df$f2 == "Y", 0.5, 0)
  rownames(df) <- NULL
  df
}

make_threeway_data <- function(n_per_cell = 5) {
  set.seed(42)
  grid <- expand.grid(
    f1 = c("A", "B"),
    f2 = c("X", "Y"),
    f3 = c("L", "M"),
    stringsAsFactors = FALSE
  )
  df <- grid[rep(seq_len(nrow(grid)), each = n_per_cell), ]
  df$measure <- rnorm(nrow(df)) +
    ifelse(df$f1 == "B", 1, 0) +
    ifelse(df$f2 == "Y", 0.5, 0) +
    ifelse(df$f3 == "M", 0.3, 0)
  rownames(df) <- NULL
  df
}

# =============================================================================
# perform_tukey_hsd — 1-way happy path
# =============================================================================

describe("perform_tukey_hsd 1-way", {
  it("returns data.frame with expected columns", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- parametric_posthoc$perform_tukey_hsd(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_true(is.data.frame(result))
    expect_true("Interaction" %in% names(result))
    expect_true("Tukey.diff" %in% names(result))
    expect_true("Tukey.ci.lower" %in% names(result))
    expect_true("Tukey.ci.upper" %in% names(result))
    expect_true("Tukey.p.value" %in% names(result))
  })

  it("returns correct number of comparisons for 3 groups", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- parametric_posthoc$perform_tukey_hsd(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    # C(3,2) = 3 comparisons
    expect_equal(nrow(result), 3)
  })

  it("returns raw p-values without p.adjusted column", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- parametric_posthoc$perform_tukey_hsd(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_false("Tukey.p.adjusted" %in% names(result))
  })
})

# =============================================================================
# perform_tukey_hsd — 2-way happy path
# =============================================================================

describe("perform_tukey_hsd 2-way", {
  it("returns data.frame with combined groups", {
    df <- make_twoway_data(n_per_cell = 10)
    result <- parametric_posthoc$perform_tukey_hsd(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure"
    )
    expect_true(is.data.frame(result))
    # 4 combined groups -> C(4,2) = 6 comparisons
    expect_equal(nrow(result), 6)
    expect_true("Interaction" %in% names(result))
    expect_true("Tukey.diff" %in% names(result))
  })
})

# =============================================================================
# perform_tukey_hsd — 3-way happy path
# =============================================================================

describe("perform_tukey_hsd 3-way", {
  it("returns data.frame with combined groups", {
    df <- make_threeway_data(n_per_cell = 5)
    result <- parametric_posthoc$perform_tukey_hsd(
      df = df,
      x_axis = c("f1", "f2", "f3"),
      measure_col = "measure"
    )
    expect_true(is.data.frame(result))
    # 8 combined groups -> C(8,2) = 28 comparisons
    expect_equal(nrow(result), 28)
  })
})

# =============================================================================
# perform_tukey_hsd — validation
# =============================================================================

describe("perform_tukey_hsd validation", {
  it("returns app_error when < 2 groups", {
    df <- data.frame(
      group = rep("A", 10),
      measure = rnorm(10),
      stringsAsFactors = FALSE
    )
    result <- parametric_posthoc$perform_tukey_hsd(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_true(error_handling$is_app_error(result))
  })
})

# =============================================================================
# perform_cohens_d — 1-way happy path
# =============================================================================

describe("perform_cohens_d 1-way", {
  it("returns data.frame with expected columns", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- parametric_posthoc$perform_cohens_d(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_true(is.data.frame(result))
    expect_true("Interaction" %in% names(result))
    expect_true("Cohen.d" %in% names(result))
    expect_true("Cohen.ci.lower" %in% names(result))
    expect_true("Cohen.ci.upper" %in% names(result))
    expect_true("Cohen.p.value" %in% names(result))
  })

  it("returns correct number of comparisons for 3 groups", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- parametric_posthoc$perform_cohens_d(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    # C(3,2) = 3 comparisons
    expect_equal(nrow(result), 3)
  })

  it("returns raw p-values without p.adjusted column", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- parametric_posthoc$perform_cohens_d(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_false("Cohen.p.adjusted" %in% names(result))
  })
})

# =============================================================================
# perform_cohens_d — 2-way happy path
# =============================================================================

describe("perform_cohens_d 2-way", {
  it("returns data.frame with combined groups", {
    df <- make_twoway_data(n_per_cell = 10)
    result <- parametric_posthoc$perform_cohens_d(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure"
    )
    expect_true(is.data.frame(result))
    # 4 combined groups -> C(4,2) = 6 comparisons
    expect_equal(nrow(result), 6)
  })
})

# =============================================================================
# perform_cohens_d — validation
# =============================================================================

describe("perform_cohens_d validation", {
  it("returns app_error when < 2 groups", {
    df <- data.frame(
      group = rep("A", 10),
      measure = rnorm(10),
      stringsAsFactors = FALSE
    )
    result <- parametric_posthoc$perform_cohens_d(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_true(error_handling$is_app_error(result))
  })
})

# =============================================================================
# perform_combined_parametric_posthoc — 1-way happy path
# =============================================================================

describe("perform_combined_parametric_posthoc 1-way", {
  it("returns merged table with both Tukey and Cohen columns", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- parametric_posthoc$perform_combined_parametric_posthoc(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      p_adjust_method = "bonferroni"
    )
    expect_true(is.data.frame(result))
    expect_true("Interaction" %in% names(result))
    expect_true("Tukey.diff" %in% names(result))
    expect_true("Cohen.d" %in% names(result))
    expect_true("Tukey.p.adjusted" %in% names(result))
    expect_true("Cohen.p.adjusted" %in% names(result))
  })

  it("has p.adjusted different from raw when bonferroni", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- parametric_posthoc$perform_combined_parametric_posthoc(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      p_adjust_method = "bonferroni"
    )
    expect_true(is.data.frame(result))
    if (nrow(result) > 1) {
      raw <- result$Tukey.p.value
      adj <- result$Tukey.p.adjusted
      # At least one adjusted value should differ from raw
      # (unless all are already 1.0)
      expect_true(
        any(adj != raw) || all(adj == 1)
      )
    }
  })
})

# =============================================================================
# perform_combined_parametric_posthoc — 2-way merge
# =============================================================================

describe("perform_combined_parametric_posthoc 2-way", {
  it("successfully merges Tukey and Cohen for 2-way design", {
    df <- make_twoway_data(n_per_cell = 10)
    result <- parametric_posthoc$perform_combined_parametric_posthoc(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure",
      p_adjust_method = "bonferroni"
    )
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true("Tukey.diff" %in% names(result))
    expect_true("Cohen.d" %in% names(result))
  })
})

# =============================================================================
# perform_combined_parametric_posthoc — 3-way merge
# =============================================================================

describe("perform_combined_parametric_posthoc 3-way", {
  it("successfully merges Tukey and Cohen for 3-way design", {
    df <- make_threeway_data(n_per_cell = 5)
    result <- parametric_posthoc$perform_combined_parametric_posthoc(
      df = df,
      x_axis = c("f1", "f2", "f3"),
      measure_col = "measure",
      p_adjust_method = "bonferroni"
    )
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true("Tukey.diff" %in% names(result))
    expect_true("Cohen.d" %in% names(result))
  })
})

# =============================================================================
# perform_combined_parametric_posthoc — filter_valid with 2-way
# =============================================================================

describe("perform_combined_parametric_posthoc filter_valid", {
  it("reduces row count with filter_valid for 2-way", {
    df <- make_twoway_data(n_per_cell = 10)
    result_all <- parametric_posthoc$perform_combined_parametric_posthoc(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure",
      filter_valid = FALSE
    )
    result_filtered <- parametric_posthoc$perform_combined_parametric_posthoc(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure",
      filter_valid = TRUE
    )
    if (is.data.frame(result_all) &&
        is.data.frame(result_filtered)) {
      expect_true(
        nrow(result_filtered) <= nrow(result_all)
      )
    }
  })
})

# =============================================================================
# perform_combined_parametric_posthoc — error propagation
# =============================================================================

describe("perform_combined_parametric_posthoc error propagation", {
  it("returns app_error when < 2 groups", {
    df <- data.frame(
      group = rep("A", 10),
      measure = rnorm(10),
      stringsAsFactors = FALSE
    )
    result <- parametric_posthoc$perform_combined_parametric_posthoc(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_true(error_handling$is_app_error(result))
  })
})

# =============================================================================
# Helper: create repeated measures test data
# =============================================================================

make_rm_twoway_data <- function(n_subjects = 10) {
  set.seed(42)
  grid <- expand.grid(
    ID = paste0("S", seq_len(n_subjects)),
    COMPOSITE = c("A", "B"),
    TIME = c("T1", "T2"),
    stringsAsFactors = FALSE
  )
  grid$measure <- rnorm(nrow(grid)) +
    ifelse(grid$COMPOSITE == "B", 1, 0) +
    ifelse(grid$TIME == "T2", 0.5, 0)
  grid
}

# =============================================================================
# perform_rm_parametric_posthoc — 2-way RM happy path
# =============================================================================

describe("perform_rm_parametric_posthoc 2-way RM", {
  it("returns data.frame with Tukey and Cohen columns matching non-RM structure", {
    df <- make_rm_twoway_data(n_subjects = 10)
    result <- parametric_posthoc$perform_rm_parametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      p_adjust_method = "bonferroni"
    )
    expect_true(is.data.frame(result))
    expect_true("Interaction" %in% names(result))
    expect_false("Type" %in% names(result))
    expect_true("Tukey.diff" %in% names(result))
    expect_true("Tukey.ci.lower" %in% names(result))
    expect_true("Tukey.ci.upper" %in% names(result))
    expect_true("Tukey.p.value" %in% names(result))
    expect_true("Tukey.p.adjusted" %in% names(result))
    expect_true("Cohen.d" %in% names(result))
    expect_true("Cohen.ci.lower" %in% names(result))
    expect_true("Cohen.ci.upper" %in% names(result))
    expect_true("Cohen.p.value" %in% names(result))
    expect_true("Cohen.p.adjusted" %in% names(result))
    expect_true(nrow(result) > 0)
  })

  it("contains both paired and unpaired comparisons", {
    df <- make_rm_twoway_data(n_subjects = 10)
    result <- parametric_posthoc$perform_rm_parametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      p_adjust_method = "bonferroni"
    )
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    # Should have comparisons like A.T1 vs A.T2 (paired) and A.T1 vs B.T1 (unpaired)
    interactions <- result$Interaction
    expect_true(length(interactions) > 1)
  })

  it("applies p-adjustment across all comparisons", {
    df <- make_rm_twoway_data(n_subjects = 10)
    result <- parametric_posthoc$perform_rm_parametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      p_adjust_method = "bonferroni"
    )
    expect_true(is.data.frame(result))
    # p.adjusted should be >= p.value for all rows
    expect_true(all(result$Tukey.p.adjusted >= result$Tukey.p.value - 1e-10))
  })
})

# =============================================================================
# perform_combined_parametric_posthoc — RM path via is_rm flag
# =============================================================================

describe("perform_combined_parametric_posthoc RM path", {
  it("routes to RM posthoc when is_rm=TRUE", {
    df <- make_rm_twoway_data(n_subjects = 10)
    result <- parametric_posthoc$perform_combined_parametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      p_adjust_method = "bonferroni",
      is_rm = TRUE,
      id_col = "ID",
      within_col = "TIME"
    )
    expect_true(is.data.frame(result))
    expect_false("Type" %in% names(result))
    expect_true("Tukey.diff" %in% names(result))
    expect_true("Cohen.d" %in% names(result))
    expect_true(nrow(result) > 0)
  })
})

# =============================================================================
# DEBUG: Inspect column structures for RM refactoring
# =============================================================================

describe("DEBUG: Column structure inspection", {
  it("prints unpaired vs RM comparison", {
    df <- make_rm_twoway_data(n_subjects = 10)

    # Standard unpaired result with filter_valid=TRUE (no RM)
    unpaired_result <- parametric_posthoc$perform_combined_parametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      p_adjust_method = "none",
      filter_valid = TRUE,
      is_rm = FALSE
    )

    # RM result (hybrid approach)
    rm_result <- parametric_posthoc$perform_rm_parametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      p_adjust_method = "none"
    )

    cat("\n\n========== UNPAIRED (filter_valid=TRUE) ==========\n")
    print(unpaired_result[, c("Interaction", "Tukey.diff", "Tukey.p.value", "Cohen.d")])

    cat("\n========== RM (HYBRID) ==========\n")
    print(rm_result[, c("Interaction", "Tukey.diff", "Tukey.p.value", "Cohen.d")])

    cat("\n========== COMPARISON ==========\n")
    cat("Both should have 4 rows (valid comparisons only)\n")
    cat("Paired rows (A.T1 vs A.T2, B.T1 vs B.T2) should differ\n")
    cat("Unpaired rows (A.T1 vs B.T1, A.T2 vs B.T2) should match\n")
    cat("=================================\n\n")

    expect_equal(nrow(unpaired_result), nrow(rm_result))
    expect_true(is.data.frame(rm_result))
  })
})

# =============================================================================
# perform_rm_parametric_posthoc — validation tests
# =============================================================================

describe("perform_rm_parametric_posthoc hybrid approach", {
  it("returns same row count as filter_valid unpaired", {
    df <- make_rm_twoway_data(n_subjects = 10)

    unpaired <- parametric_posthoc$perform_combined_parametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      p_adjust_method = "none",
      filter_valid = TRUE,
      is_rm = FALSE
    )

    rm_result <- parametric_posthoc$perform_rm_parametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      p_adjust_method = "none"
    )

    expect_equal(nrow(rm_result), nrow(unpaired))
  })

  it("has different p-values for paired comparisons vs unpaired", {
    df <- make_rm_twoway_data(n_subjects = 10)

    unpaired <- parametric_posthoc$perform_combined_parametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      p_adjust_method = "none",
      filter_valid = TRUE,
      is_rm = FALSE
    )

    rm_result <- parametric_posthoc$perform_rm_parametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      p_adjust_method = "none"
    )

    # Find paired comparisons (A.T1 vs A.T2, B.T1 vs B.T2)
    paired_interactions <- c("A.T1 vs. A.T2", "B.T1 vs. B.T2")

    for (int in paired_interactions) {
      unpaired_row <- unpaired[unpaired$Interaction == int, ]
      rm_row <- rm_result[rm_result$Interaction == int, ]

      if (nrow(unpaired_row) == 1 && nrow(rm_row) == 1) {
        # Paired test should give different p-value than unpaired
        expect_false(
          isTRUE(all.equal(unpaired_row$Tukey.p.value, rm_row$Tukey.p.value)),
          info = paste("Paired comparison", int, "should differ")
        )
      }
    }
  })

  it("has same p-values for unpaired (between-subject) comparisons", {
    df <- make_rm_twoway_data(n_subjects = 10)

    unpaired <- parametric_posthoc$perform_combined_parametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      p_adjust_method = "none",
      filter_valid = TRUE,
      is_rm = FALSE
    )

    rm_result <- parametric_posthoc$perform_rm_parametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      p_adjust_method = "none"
    )

    # Find unpaired comparisons (A.T1 vs B.T1, A.T2 vs B.T2)
    unpaired_interactions <- c("A.T1 vs. B.T1", "A.T2 vs. B.T2")

    for (int in unpaired_interactions) {
      unpaired_row <- unpaired[unpaired$Interaction == int, ]
      rm_row <- rm_result[rm_result$Interaction == int, ]

      if (nrow(unpaired_row) == 1 && nrow(rm_row) == 1) {
        # Between-subject comparisons should be identical
        expect_equal(
          unpaired_row$Tukey.p.value, rm_row$Tukey.p.value,
          tolerance = 1e-6,
          info = paste("Unpaired comparison", int, "should match")
        )
      }
    }
  })

  it("applies p-adjustment correctly across all comparisons", {
    df <- make_rm_twoway_data(n_subjects = 10)

    rm_result <- parametric_posthoc$perform_rm_parametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      p_adjust_method = "bonferroni"
    )

    # p.adjusted should be >= p.value (bonferroni multiplies by n)
    expect_true(all(rm_result$Tukey.p.adjusted >= rm_result$Tukey.p.value - 1e-10))

    # With 4 comparisons, bonferroni should multiply by 4 (capped at 1)
    expected_adj <- pmin(rm_result$Tukey.p.value * 4, 1)
    expect_equal(rm_result$Tukey.p.adjusted, expected_adj, tolerance = 1e-3)
  })
})
