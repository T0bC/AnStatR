box::use(
  testthat[describe, expect_equal, expect_true, expect_false, it],
)

box::use(
  app/logic/shared/error_handling,
  app/logic/statistics/robust_posthoc,
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
# perform_lincon — 1-way happy path
# =============================================================================

describe("perform_lincon 1-way", {
  it("returns data.frame with expected columns", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- robust_posthoc$perform_lincon(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      tr_value = 0.2
    )
    expect_true(is.data.frame(result))
    expect_true("Interaction" %in% names(result))
    expect_true("Lincon.psihat" %in% names(result))
    expect_true("Lincon.ci.lower" %in% names(result))
    expect_true("Lincon.ci.upper" %in% names(result))
    expect_true("Lincon.p.value" %in% names(result))
  })

  it("returns correct number of comparisons for 3 groups", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- robust_posthoc$perform_lincon(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      tr_value = 0.2
    )
    # C(3,2) = 3 comparisons
    expect_equal(nrow(result), 3)
  })

  it("returns raw p-values without p.adjusted column", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- robust_posthoc$perform_lincon(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      tr_value = 0.2
    )
    expect_false("p.adjusted" %in% names(result))
    expect_false("Lincon.p.adjusted" %in% names(result))
  })
})

# =============================================================================
# perform_lincon — 2-way happy path (structured contrasts)
# =============================================================================

describe("perform_lincon 2-way", {
  it("returns data.frame with structured contrasts", {
    df <- make_twoway_data(n_per_cell = 10)
    result <- robust_posthoc$perform_lincon(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure",
      tr_value = 0.2
    )
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true("Interaction" %in% names(result))
    expect_true("Lincon.psihat" %in% names(result))
  })
})

# =============================================================================
# perform_lincon — 3-way happy path (structured contrasts)
# =============================================================================

describe("perform_lincon 3-way", {
  it("returns data.frame with structured contrasts", {
    df <- make_threeway_data(n_per_cell = 5)
    result <- robust_posthoc$perform_lincon(
      df = df,
      x_axis = c("f1", "f2", "f3"),
      measure_col = "measure",
      tr_value = 0.2
    )
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true("Interaction" %in% names(result))
  })
})

# =============================================================================
# perform_lincon — validation
# =============================================================================

describe("perform_lincon validation", {
  it("returns app_error when < 2 groups", {
    df <- data.frame(
      group = rep("A", 10),
      measure = rnorm(10),
      stringsAsFactors = FALSE
    )
    result <- robust_posthoc$perform_lincon(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      tr_value = 0.2
    )
    expect_true(error_handling$is_app_error(result))
  })
})

# =============================================================================
# perform_lincon — bootstrap
# =============================================================================

describe("perform_lincon bootstrap", {
  it("returns CI-formatted strings in bootstrap mode", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- robust_posthoc$perform_lincon(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      tr_value = 0.2,
      use_bootstrap = TRUE,
      boot_samples = 5,
      boot_sample_size = NULL
    )
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    # Bootstrap results are formatted as "mean [lower - upper]"
    expect_true(grepl("\\[", result$Lincon.psihat[1]))
  })
})

# =============================================================================
# perform_cliff — 1-way happy path
# =============================================================================

describe("perform_cliff 1-way", {
  it("returns data.frame with expected columns", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- robust_posthoc$perform_cliff(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_true(is.data.frame(result))
    expect_true("Interaction" %in% names(result))
    expect_true("Cliff.psihat" %in% names(result))
    expect_true("Cliff.ci.lower" %in% names(result))
    expect_true("Cliff.ci.upper" %in% names(result))
    expect_true("Cliff.p.value" %in% names(result))
  })

  it("returns correct number of comparisons for 3 groups", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- robust_posthoc$perform_cliff(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    # C(3,2) = 3 comparisons
    expect_equal(nrow(result), 3)
  })

  it("returns raw p-values without p.adjusted column", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- robust_posthoc$perform_cliff(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_false("Cliff.p.adjusted" %in% names(result))
  })
})

# =============================================================================
# perform_cliff — 2-way
# =============================================================================

describe("perform_cliff 2-way", {
  it("returns data.frame with combined groups", {
    df <- make_twoway_data(n_per_cell = 10)
    result <- robust_posthoc$perform_cliff(
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
# perform_cliff — validation
# =============================================================================

describe("perform_cliff validation", {
  it("returns app_error when < 2 groups", {
    df <- data.frame(
      group = rep("A", 10),
      measure = rnorm(10),
      stringsAsFactors = FALSE
    )
    result <- robust_posthoc$perform_cliff(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_true(error_handling$is_app_error(result))
  })
})

# =============================================================================
# perform_cliff — bootstrap
# =============================================================================

describe("perform_cliff bootstrap", {
  it("returns CI-formatted strings in bootstrap mode", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- robust_posthoc$perform_cliff(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      use_bootstrap = TRUE,
      boot_samples = 5,
      boot_sample_size = NULL
    )
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true(grepl("\\[", result$Cliff.psihat[1]))
  })
})

# =============================================================================
# perform_combined_posthoc — happy path
# =============================================================================

describe("perform_combined_posthoc 1-way", {
  it("returns merged table with both lincon and cliff columns", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- robust_posthoc$perform_combined_posthoc(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      tr_value = 0.2,
      p_adjust_method = "bonferroni"
    )
    expect_true(is.data.frame(result))
    expect_true("Interaction" %in% names(result))
    expect_true("Lincon.psihat" %in% names(result))
    expect_true("Cliff.psihat" %in% names(result))
    expect_true("Lincon.p.adjusted" %in% names(result))
    expect_true("Cliff.p.adjusted" %in% names(result))
  })

  it("has p.adjusted different from raw when bonferroni", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- robust_posthoc$perform_combined_posthoc(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      tr_value = 0.2,
      p_adjust_method = "bonferroni"
    )
    # With 3 comparisons, bonferroni should differ from raw
    expect_true(is.data.frame(result))
    if (nrow(result) > 1) {
      raw <- result$Lincon.p.value
      adj <- result$Lincon.p.adjusted
      # At least one adjusted value should differ from raw
      # (unless all are already 1.0)
      expect_true(
        any(adj != raw) || all(adj == 1)
      )
    }
  })
})

# =============================================================================
# perform_combined_posthoc — 2-way merge (regression: separator mismatch)
# =============================================================================

describe("perform_combined_posthoc 2-way", {
  it("successfully merges lincon and cliff for 2-way design", {
    df <- make_twoway_data(n_per_cell = 10)
    result <- robust_posthoc$perform_combined_posthoc(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure",
      tr_value = 0.2,
      p_adjust_method = "bonferroni"
    )
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true("Lincon.psihat" %in% names(result))
    expect_true("Cliff.psihat" %in% names(result))
  })
})

# =============================================================================
# perform_combined_posthoc — 3-way merge (regression: separator mismatch)
# =============================================================================

describe("perform_combined_posthoc 3-way", {
  it("successfully merges lincon and cliff for 3-way design", {
    df <- make_threeway_data(n_per_cell = 5)
    result <- robust_posthoc$perform_combined_posthoc(
      df = df,
      x_axis = c("f1", "f2", "f3"),
      measure_col = "measure",
      tr_value = 0.2,
      p_adjust_method = "bonferroni"
    )
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true("Lincon.psihat" %in% names(result))
    expect_true("Cliff.psihat" %in% names(result))
  })
})

# =============================================================================
# perform_combined_posthoc — filter_valid with 2-way
# =============================================================================

describe("perform_combined_posthoc filter_valid", {
  it("reduces row count with filter_valid for 2-way", {
    df <- make_twoway_data(n_per_cell = 10)
    result_all <- robust_posthoc$perform_combined_posthoc(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure",
      tr_value = 0.2,
      filter_valid = FALSE
    )
    result_filtered <- robust_posthoc$perform_combined_posthoc(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure",
      tr_value = 0.2,
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
# perform_combined_posthoc — error propagation
# =============================================================================

describe("perform_combined_posthoc error propagation", {
  it("returns app_error when < 2 groups", {
    df <- data.frame(
      group = rep("A", 10),
      measure = rnorm(10),
      stringsAsFactors = FALSE
    )
    result <- robust_posthoc$perform_combined_posthoc(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      tr_value = 0.2
    )
    expect_true(error_handling$is_app_error(result))
  })
})

# =============================================================================
# perform_combined_posthoc — bootstrap
# =============================================================================

describe("perform_combined_posthoc bootstrap", {
  it("returns combined table with CI strings", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- robust_posthoc$perform_combined_posthoc(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      tr_value = 0.2,
      use_bootstrap = TRUE,
      boot_samples = 5,
      boot_sample_size = NULL
    )
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true("Lincon.psihat" %in% names(result))
    expect_true("Cliff.psihat" %in% names(result))
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
# perform_rm_robust_posthoc — 2-way RM happy path
# =============================================================================

describe("perform_rm_robust_posthoc 2-way RM", {
  it("returns data.frame with Lincon and Cliff columns matching non-RM structure", {
    df <- make_rm_twoway_data(n_subjects = 10)
    result <- robust_posthoc$perform_rm_robust_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      tr_value = 0.2,
      p_adjust_method = "bonferroni"
    )
    expect_true(is.data.frame(result))
    expect_true("Interaction" %in% names(result))
    expect_false("Type" %in% names(result))
    expect_true("Lincon.psihat" %in% names(result))
    expect_true("Lincon.p.value" %in% names(result))
    expect_true("Lincon.p.adjusted" %in% names(result))
    expect_true("Cliff.psihat" %in% names(result))
    expect_true("Cliff.p.adjusted" %in% names(result))
    expect_true(nrow(result) > 0)
  })

  it("returns same row count as filter_valid unpaired", {
    df <- make_rm_twoway_data(n_subjects = 10)

    unpaired <- robust_posthoc$perform_combined_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      tr_value = 0.2,
      p_adjust_method = "none",
      filter_valid = TRUE,
      is_rm = FALSE
    )

    rm_result <- robust_posthoc$perform_rm_robust_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      tr_value = 0.2,
      p_adjust_method = "none"
    )

    expect_equal(nrow(rm_result), nrow(unpaired))
  })

  it("has different p-values for paired comparisons vs unpaired", {
    df <- make_rm_twoway_data(n_subjects = 10)

    unpaired <- robust_posthoc$perform_combined_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      tr_value = 0.2,
      p_adjust_method = "none",
      filter_valid = TRUE,
      is_rm = FALSE
    )

    rm_result <- robust_posthoc$perform_rm_robust_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      tr_value = 0.2,
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
          isTRUE(all.equal(unpaired_row$Lincon.p.value, rm_row$Lincon.p.value)),
          info = paste("Paired comparison", int, "should differ")
        )
      }
    }
  })

  it("has same p-values for unpaired (between-subject) comparisons", {
    df <- make_rm_twoway_data(n_subjects = 10)

    unpaired <- robust_posthoc$perform_combined_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      tr_value = 0.2,
      p_adjust_method = "none",
      filter_valid = TRUE,
      is_rm = FALSE
    )

    rm_result <- robust_posthoc$perform_rm_robust_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      tr_value = 0.2,
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
          unpaired_row$Lincon.p.value, rm_row$Lincon.p.value,
          tolerance = 1e-6,
          info = paste("Unpaired comparison", int, "should match")
        )
      }
    }
  })

  it("applies p-adjustment correctly across all comparisons", {
    df <- make_rm_twoway_data(n_subjects = 10)

    rm_result <- robust_posthoc$perform_rm_robust_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      tr_value = 0.2,
      p_adjust_method = "bonferroni"
    )

    # p.adjusted should be >= p.value (bonferroni multiplies by n)
    expect_true(all(rm_result$Lincon.p.adjusted >= rm_result$Lincon.p.value - 1e-10))

    # With 4 comparisons, bonferroni should multiply by 4 (capped at 1)
    expected_adj <- pmin(rm_result$Lincon.p.value * 4, 1)
    expect_equal(rm_result$Lincon.p.adjusted, expected_adj, tolerance = 1e-3)
  })
})

# =============================================================================
# perform_combined_posthoc — RM path via is_rm flag
# =============================================================================

describe("perform_combined_posthoc RM path", {
  it("routes to RM posthoc when is_rm=TRUE", {
    df <- make_rm_twoway_data(n_subjects = 10)
    result <- robust_posthoc$perform_combined_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      tr_value = 0.2,
      p_adjust_method = "bonferroni",
      is_rm = TRUE,
      id_col = "ID",
      within_col = "TIME"
    )
    expect_true(is.data.frame(result))
    expect_false("Type" %in% names(result))
    expect_true("Lincon.p.value" %in% names(result))
    expect_true("Cliff.psihat" %in% names(result))
    expect_true(nrow(result) > 0)
  })
})

# =============================================================================
# DEBUG: Inspect robust column structures
# =============================================================================

describe("DEBUG: Robust column structure inspection", {
  it("prints unpaired vs RM comparison", {
    df <- make_rm_twoway_data(n_subjects = 10)

    # Standard unpaired result with filter_valid=TRUE (no RM)
    unpaired_result <- robust_posthoc$perform_combined_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      tr_value = 0.2,
      p_adjust_method = "none",
      filter_valid = TRUE,
      is_rm = FALSE
    )

    # RM result (hybrid approach)
    rm_result <- robust_posthoc$perform_rm_robust_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      tr_value = 0.2,
      p_adjust_method = "none"
    )

    cat("\n\n========== ROBUST UNPAIRED (filter_valid=TRUE) ==========\n")
    print(unpaired_result[, c("Interaction", "Lincon.psihat", "Lincon.p.value", "Cliff.psihat")])

    cat("\n========== ROBUST RM (HYBRID) ==========\n")
    print(rm_result[, c("Interaction", "Lincon.psihat", "Lincon.p.value", "Cliff.psihat")])

    cat("\n========== COMPARISON ==========\n")
    cat("Both should have 4 rows (valid comparisons only)\n")
    cat("Paired rows (A.T1 vs A.T2, B.T1 vs B.T2) Lincon p-values should differ\n")
    cat("Unpaired rows (A.T1 vs B.T1, A.T2 vs B.T2) should match\n")
    cat("Paired-row Cliff.psihat is yuend's effect size (replaced)\n")
    cat("=================================\n\n")

    expect_equal(nrow(unpaired_result), nrow(rm_result))
    expect_true(is.data.frame(rm_result))
  })
})

# =============================================================================
# perform_rm_robust_posthoc — paired effect size correctness
# =============================================================================

describe("perform_rm_robust_posthoc paired effect size", {
  it("uses yuend effect size for paired rows (differs from unpaired Cliff)", {
    df <- make_rm_twoway_data(n_subjects = 10)

    unpaired <- robust_posthoc$perform_combined_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      tr_value = 0.2,
      p_adjust_method = "none",
      filter_valid = TRUE,
      is_rm = FALSE
    )

    rm_result <- robust_posthoc$perform_rm_robust_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      tr_value = 0.2,
      p_adjust_method = "none"
    )

    paired_interactions <- c("A.T1 vs. A.T2", "B.T1 vs. B.T2")
    for (int in paired_interactions) {
      unpaired_row <- unpaired[unpaired$Interaction == int, ]
      rm_row <- rm_result[rm_result$Interaction == int, ]
      if (nrow(unpaired_row) == 1 && nrow(rm_row) == 1) {
        # Effect size for paired rows uses the paired P(X<Y) sign test,
        # not the independent-samples Cliff's method
        expect_false(
          isTRUE(all.equal(
            unpaired_row$Cliff.psihat, rm_row$Cliff.psihat
          )),
          info = paste("Paired effect size", int, "should differ")
        )
        # P(X<Y) is a probability with a binomial CI and a sign-test p-value
        expect_true(rm_row$Cliff.psihat >= 0 && rm_row$Cliff.psihat <= 1)
        expect_false(is.na(rm_row$Cliff.ci.lower))
        expect_false(is.na(rm_row$Cliff.ci.upper))
        expect_false(is.na(rm_row$Cliff.p.value))
      }
    }
  })

  it("keeps unpaired Cliff effect size for between-subject rows", {
    df <- make_rm_twoway_data(n_subjects = 10)

    unpaired <- robust_posthoc$perform_combined_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      tr_value = 0.2,
      p_adjust_method = "none",
      filter_valid = TRUE,
      is_rm = FALSE
    )

    rm_result <- robust_posthoc$perform_rm_robust_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      tr_value = 0.2,
      p_adjust_method = "none"
    )

    unpaired_interactions <- c("A.T1 vs. B.T1", "A.T2 vs. B.T2")
    for (int in unpaired_interactions) {
      unpaired_row <- unpaired[unpaired$Interaction == int, ]
      rm_row <- rm_result[rm_result$Interaction == int, ]
      if (nrow(unpaired_row) == 1 && nrow(rm_row) == 1) {
        expect_equal(
          unpaired_row$Cliff.psihat, rm_row$Cliff.psihat,
          tolerance = 1e-6,
          info = paste("Between-subject effect size", int, "should match")
        )
        expect_false(is.na(rm_row$Cliff.ci.lower))
      }
    }
  })
})
