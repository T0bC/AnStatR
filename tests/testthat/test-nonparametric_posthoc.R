box::use(
  testthat[describe, expect_equal, expect_true, expect_false, it],
)

box::use(
  app/logic/shared/error_handling,
  app/logic/statistics/nonparametric_posthoc,
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
# perform_dunn_test — 1-way happy path
# =============================================================================

describe("perform_dunn_test 1-way", {
  it("returns data.frame with expected columns", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- nonparametric_posthoc$perform_dunn_test(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_true(is.data.frame(result))
    expect_true("Interaction" %in% names(result))
    expect_true("Dunn.Z" %in% names(result))
    expect_true("Dunn.p.value" %in% names(result))
  })

  it("returns correct number of comparisons for 3 groups", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- nonparametric_posthoc$perform_dunn_test(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    # C(3,2) = 3 comparisons
    expect_equal(nrow(result), 3)
  })

  it("returns raw p-values without p.adjusted column", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- nonparametric_posthoc$perform_dunn_test(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_false("Dunn.p.adjusted" %in% names(result))
  })
})

# =============================================================================
# perform_dunn_test — validation
# =============================================================================

describe("perform_dunn_test validation", {
  it("returns app_error when < 2 groups", {
    df <- data.frame(
      group = rep("A", 10),
      measure = rnorm(10),
      stringsAsFactors = FALSE
    )
    result <- nonparametric_posthoc$perform_dunn_test(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_true(error_handling$is_app_error(result))
  })
})

# =============================================================================
# perform_wilcox_pairwise — 1-way happy path
# =============================================================================

describe("perform_wilcox_pairwise 1-way", {
  it("returns data.frame with expected columns", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- nonparametric_posthoc$perform_wilcox_pairwise(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_true(is.data.frame(result))
    expect_true("Interaction" %in% names(result))
    expect_true("Wilcox.p.value" %in% names(result))
  })

  it("returns correct number of comparisons for 3 groups", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- nonparametric_posthoc$perform_wilcox_pairwise(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    # C(3,2) = 3 comparisons
    expect_equal(nrow(result), 3)
  })

  it("returns raw p-values without p.adjusted column", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- nonparametric_posthoc$perform_wilcox_pairwise(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_false("Wilcox.p.adjusted" %in% names(result))
  })
})

# =============================================================================
# perform_wilcox_pairwise — validation
# =============================================================================

describe("perform_wilcox_pairwise validation", {
  it("returns app_error when < 2 groups", {
    df <- data.frame(
      group = rep("A", 10),
      measure = rnorm(10),
      stringsAsFactors = FALSE
    )
    result <- nonparametric_posthoc$perform_wilcox_pairwise(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_true(error_handling$is_app_error(result))
  })
})

# =============================================================================
# perform_art_contrasts — 2-way happy path
# =============================================================================

describe("perform_art_contrasts 2-way", {
  it("returns data.frame with expected columns", {
    df <- make_twoway_data(n_per_cell = 10)
    result <- nonparametric_posthoc$perform_art_contrasts(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure"
    )
    expect_true(is.data.frame(result))
    expect_true("Interaction" %in% names(result))
    expect_true("ART.estimate" %in% names(result))
    expect_true("ART.SE" %in% names(result))
    expect_true("ART.t.ratio" %in% names(result))
    expect_true("ART.p.value" %in% names(result))
    expect_true("ART.d" %in% names(result))
    expect_true("ART.d.ci.lower" %in% names(result))
    expect_true("ART.d.ci.upper" %in% names(result))
  })

  it("returns correct number of comparisons for 2x2", {
    df <- make_twoway_data(n_per_cell = 10)
    result <- nonparametric_posthoc$perform_art_contrasts(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure"
    )
    # 4 combined groups -> C(4,2) = 6 comparisons
    expect_equal(nrow(result), 6)
  })
})

# =============================================================================
# perform_art_contrasts — 3-way happy path
# =============================================================================

describe("perform_art_contrasts 3-way", {
  it("returns data.frame with expected columns", {
    df <- make_threeway_data(n_per_cell = 5)
    result <- nonparametric_posthoc$perform_art_contrasts(
      df = df,
      x_axis = c("f1", "f2", "f3"),
      measure_col = "measure"
    )
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true("Interaction" %in% names(result))
    expect_true("ART.d" %in% names(result))
  })
})

# =============================================================================
# perform_art_contrasts — validation
# =============================================================================

describe("perform_art_contrasts validation", {
  it("returns app_error when < 2 groups", {
    df <- data.frame(
      f1 = rep("A", 10),
      f2 = rep("X", 10),
      measure = rnorm(10),
      stringsAsFactors = FALSE
    )
    result <- nonparametric_posthoc$perform_art_contrasts(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure"
    )
    expect_true(error_handling$is_app_error(result))
  })
})

# =============================================================================
# perform_combined_nonparametric_posthoc — 1-way Dunn happy path
# =============================================================================

describe("perform_combined_nonparametric_posthoc 1-way dunn", {
  it("returns merged table with Dunn and Cliff columns", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- nonparametric_posthoc$perform_combined_nonparametric_posthoc(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      p_adjust_method = "bonferroni",
      posthoc_method = "dunn"
    )
    expect_true(is.data.frame(result))
    expect_true("Interaction" %in% names(result))
    expect_true("Dunn.Z" %in% names(result))
    expect_true("Dunn.p.adjusted" %in% names(result))
    expect_true("Cliff.psihat" %in% names(result))
    expect_true("Cliff.p.adjusted" %in% names(result))
  })

  it("has p.adjusted different from raw when bonferroni", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- nonparametric_posthoc$perform_combined_nonparametric_posthoc(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      p_adjust_method = "bonferroni",
      posthoc_method = "dunn"
    )
    expect_true(is.data.frame(result))
    if (nrow(result) > 1) {
      raw <- result$Dunn.p.value
      adj <- result$Dunn.p.adjusted
      expect_true(
        any(adj != raw) || all(adj == 1)
      )
    }
  })
})

# =============================================================================
# perform_combined_nonparametric_posthoc — 1-way Wilcox happy path
# =============================================================================

describe("perform_combined_nonparametric_posthoc 1-way wilcox", {
  it("returns merged table with Wilcox and Cliff columns", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- nonparametric_posthoc$perform_combined_nonparametric_posthoc(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      p_adjust_method = "bonferroni",
      posthoc_method = "wilcox"
    )
    expect_true(is.data.frame(result))
    expect_true("Interaction" %in% names(result))
    expect_true("Wilcox.p.value" %in% names(result))
    expect_true("Wilcox.p.adjusted" %in% names(result))
    expect_true("Cliff.psihat" %in% names(result))
  })
})

# =============================================================================
# perform_combined_nonparametric_posthoc — 2-way ART
# =============================================================================

describe("perform_combined_nonparametric_posthoc 2-way", {
  it("returns ART contrasts with Cohen's d for 2-way", {
    df <- make_twoway_data(n_per_cell = 10)
    result <- nonparametric_posthoc$perform_combined_nonparametric_posthoc(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure",
      p_adjust_method = "bonferroni"
    )
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true("ART.estimate" %in% names(result))
    expect_true("ART.p.adjusted" %in% names(result))
    expect_true("ART.d" %in% names(result))
  })
})

# =============================================================================
# perform_combined_nonparametric_posthoc — 3-way ART
# =============================================================================

describe("perform_combined_nonparametric_posthoc 3-way", {
  it("returns ART contrasts with Cohen's d for 3-way", {
    df <- make_threeway_data(n_per_cell = 5)
    result <- nonparametric_posthoc$perform_combined_nonparametric_posthoc(
      df = df,
      x_axis = c("f1", "f2", "f3"),
      measure_col = "measure",
      p_adjust_method = "bonferroni"
    )
    expect_true(is.data.frame(result))
    expect_true(nrow(result) > 0)
    expect_true("ART.estimate" %in% names(result))
    expect_true("ART.d" %in% names(result))
  })
})

# =============================================================================
# perform_combined_nonparametric_posthoc — filter_valid 2-way
# =============================================================================

describe("perform_combined_nonparametric_posthoc filter_valid", {
  it("reduces row count with filter_valid for 2-way", {
    df <- make_twoway_data(n_per_cell = 10)
    result_all <- nonparametric_posthoc$perform_combined_nonparametric_posthoc(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure",
      filter_valid = FALSE
    )
    result_filtered <- nonparametric_posthoc$perform_combined_nonparametric_posthoc(
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
# perform_combined_nonparametric_posthoc — error propagation
# =============================================================================

describe("perform_combined_nonparametric_posthoc error propagation", {
  it("returns app_error when < 2 groups", {
    df <- data.frame(
      group = rep("A", 10),
      measure = rnorm(10),
      stringsAsFactors = FALSE
    )
    result <- nonparametric_posthoc$perform_combined_nonparametric_posthoc(
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

make_rm_oneway_data <- function(n_subjects = 15, n_times = 3) {
  set.seed(42)
  grid <- expand.grid(
    ID = paste0("S", seq_len(n_subjects)),
    TIME = paste0("T", seq_len(n_times)),
    stringsAsFactors = FALSE
  )
  grid$measure <- rnorm(nrow(grid)) +
    as.integer(factor(grid$TIME)) +
    rep(rnorm(n_subjects, sd = 0.5), times = n_times)
  grid
}

# =============================================================================
# perform_rm_nonparametric_posthoc — 1-way RM (pure within, paired Wilcoxon)
# =============================================================================

describe("perform_rm_nonparametric_posthoc 1-way RM", {
  it("returns Wilcox + Cliff columns (no ART) for pure within design", {
    df <- make_rm_oneway_data(n_subjects = 15, n_times = 3)
    result <- nonparametric_posthoc$perform_rm_nonparametric_posthoc(
      df = df,
      x_axis = "TIME",
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      p_adjust_method = "bonferroni"
    )
    expect_true(is.data.frame(result))
    expect_true("Interaction" %in% names(result))
    expect_true("Wilcox.p.value" %in% names(result))
    expect_true("Wilcox.p.adjusted" %in% names(result))
    expect_true("Cliff.psihat" %in% names(result))
    expect_false(any(grepl("^ART\\.", names(result))))
  })

  it("returns C(k,2) comparisons for k within-subject levels", {
    df <- make_rm_oneway_data(n_subjects = 15, n_times = 3)
    result <- nonparametric_posthoc$perform_rm_nonparametric_posthoc(
      df = df,
      x_axis = "TIME",
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME"
    )
    expect_equal(nrow(result), 3)
  })

  it("produces Cliff's delta bounded in [-1, 1]", {
    df <- make_rm_oneway_data(n_subjects = 15, n_times = 3)
    result <- nonparametric_posthoc$perform_rm_nonparametric_posthoc(
      df = df,
      x_axis = "TIME",
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME"
    )
    expect_true(all(result$Cliff.psihat >= -1 & result$Cliff.psihat <= 1))
  })
})

# =============================================================================
# perform_rm_nonparametric_posthoc — 2-way RM happy path
# =============================================================================

describe("perform_rm_nonparametric_posthoc 2-way RM", {
  it("returns data.frame with ART columns matching non-RM structure", {
    df <- make_rm_twoway_data(n_subjects = 10)
    result <- nonparametric_posthoc$perform_rm_nonparametric_posthoc(
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
    expect_true("ART.estimate" %in% names(result))
    expect_true("ART.p.value" %in% names(result))
    expect_true("ART.p.adjusted" %in% names(result))
    expect_true("ART.d" %in% names(result))
    expect_true(nrow(result) > 0)
  })

  it("returns same row count as filter_valid unpaired", {
    df <- make_rm_twoway_data(n_subjects = 10)

    unpaired <- nonparametric_posthoc$perform_combined_nonparametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      p_adjust_method = "none",
      filter_valid = TRUE,
      is_rm = FALSE
    )

    rm_result <- nonparametric_posthoc$perform_rm_nonparametric_posthoc(
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

    unpaired <- nonparametric_posthoc$perform_combined_nonparametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      p_adjust_method = "none",
      filter_valid = TRUE,
      is_rm = FALSE
    )

    rm_result <- nonparametric_posthoc$perform_rm_nonparametric_posthoc(
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
          isTRUE(all.equal(unpaired_row$ART.p.value, rm_row$ART.p.value)),
          info = paste("Paired comparison", int, "should differ")
        )
      }
    }
  })

  it("has same p-values for unpaired (between-subject) comparisons", {
    df <- make_rm_twoway_data(n_subjects = 10)

    unpaired <- nonparametric_posthoc$perform_combined_nonparametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      p_adjust_method = "none",
      filter_valid = TRUE,
      is_rm = FALSE
    )

    rm_result <- nonparametric_posthoc$perform_rm_nonparametric_posthoc(
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
          unpaired_row$ART.p.value, rm_row$ART.p.value,
          tolerance = 1e-6,
          info = paste("Unpaired comparison", int, "should match")
        )
      }
    }
  })

  it("applies p-adjustment correctly across all comparisons", {
    df <- make_rm_twoway_data(n_subjects = 10)

    rm_result <- nonparametric_posthoc$perform_rm_nonparametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      p_adjust_method = "bonferroni"
    )

    # p.adjusted should be >= p.value (bonferroni multiplies by n)
    expect_true(all(rm_result$ART.p.adjusted >= rm_result$ART.p.value - 1e-10))

    # With 4 comparisons, bonferroni should multiply by 4 (capped at 1)
    expected_adj <- pmin(rm_result$ART.p.value * 4, 1)
    expect_equal(rm_result$ART.p.adjusted, expected_adj, tolerance = 1e-3)
  })

  it("blanks ART-only columns and sets Cliff's delta for paired rows", {
    df <- make_rm_twoway_data(n_subjects = 10)

    rm_result <- nonparametric_posthoc$perform_rm_nonparametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      p_adjust_method = "none"
    )

    paired_interactions <- c("A.T1 vs. A.T2", "B.T1 vs. B.T2")
    unpaired_interactions <- c("A.T1 vs. B.T1", "A.T2 vs. B.T2")

    for (int in paired_interactions) {
      row <- rm_result[rm_result$Interaction == int, ]
      if (nrow(row) == 1) {
        # ART-only statistics have no paired analog -> blanked
        expect_true(is.na(row$ART.estimate))
        expect_true(is.na(row$ART.t.ratio))
        # Paired Cliff's delta is a real effect size in [-1, 1]
        expect_false(is.na(row$ART.d))
        expect_true(row$ART.d >= -1 && row$ART.d <= 1)
      }
    }

    for (int in unpaired_interactions) {
      row <- rm_result[rm_result$Interaction == int, ]
      if (nrow(row) == 1) {
        # Between-subject rows keep their ART-C statistics
        expect_false(is.na(row$ART.estimate))
        expect_false(is.na(row$ART.t.ratio))
      }
    }
  })
})

# =============================================================================
# perform_combined_nonparametric_posthoc — RM path via is_rm flag
# =============================================================================

describe("perform_combined_nonparametric_posthoc RM path", {
  it("routes to RM posthoc when is_rm=TRUE", {
    df <- make_rm_twoway_data(n_subjects = 10)
    result <- nonparametric_posthoc$perform_combined_nonparametric_posthoc(
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
    expect_true("ART.p.value" %in% names(result))
    expect_true("ART.d" %in% names(result))
    expect_true(nrow(result) > 0)
  })
})

# =============================================================================
# DEBUG: Inspect nonparametric column structures
# =============================================================================

describe("DEBUG: Nonparametric column structure inspection", {
  it("prints unpaired vs RM comparison", {
    df <- make_rm_twoway_data(n_subjects = 10)

    # Standard unpaired result with filter_valid=TRUE (no RM)
    unpaired_result <- nonparametric_posthoc$perform_combined_nonparametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      p_adjust_method = "none",
      filter_valid = TRUE,
      is_rm = FALSE
    )

    # RM result (hybrid approach)
    rm_result <- nonparametric_posthoc$perform_rm_nonparametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      p_adjust_method = "none"
    )

    cat("\n\n========== NP UNPAIRED (filter_valid=TRUE) ==========\n")
    print(unpaired_result[, c("Interaction", "ART.estimate", "ART.p.value", "ART.d")])

    cat("\n========== NP RM (HYBRID) ==========\n")
    print(rm_result[, c("Interaction", "ART.estimate", "ART.p.value", "ART.d")])

    cat("\n========== COMPARISON ==========\n")
    cat("Both should have 4 rows (valid comparisons only)\n")
    cat("Paired rows (A.T1 vs A.T2, B.T1 vs B.T2) p-values should differ\n")
    cat("Unpaired rows (A.T1 vs B.T1, A.T2 vs B.T2) should match\n")
    cat("=================================\n\n")

    expect_equal(nrow(unpaired_result), nrow(rm_result))
    expect_true(is.data.frame(rm_result))
  })
})
