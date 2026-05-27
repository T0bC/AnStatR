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
  it("returns data.frame with both paired and unpaired comparisons", {
    df <- make_rm_twoway_data(n_subjects = 10)
    result <- parametric_posthoc$perform_rm_parametric_posthoc(
      df = df,
      x_axis = c("COMPOSITE", "TIME"),
      measure_col = "measure",
      id_col = "ID",
      within_col = "TIME",
      p_adjust_method = "bonferroni"
    )

    cat("\n=== DEBUG: RM Parametric Posthoc Result ===\n")
    cat("Column names:", paste(names(result), collapse = ", "), "\n\n")

    cat("--- PAIRED rows (Type == 'Paired') ---\n")
    paired <- result[result$Type == "Paired", ]
    print(paired[1:min(2, nrow(paired)), ])

    cat("\n--- UNPAIRED rows (Type == 'Unpaired') ---\n")
    unpaired <- result[result$Type == "Unpaired", ]
    print(unpaired[1:min(2, nrow(unpaired)), ])
    cat("=== END DEBUG ===\n\n")

    expect_true(is.data.frame(result))
    expect_true("Interaction" %in% names(result))
    expect_true("Type" %in% names(result))
    expect_true(nrow(result) > 0)
  })

  it("contains both Paired and Unpaired types", {
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
    expect_true("Paired" %in% result$Type)
    expect_true("Unpaired" %in% result$Type)
  })

  it("has correct columns for paired results", {
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
    paired_rows <- result[result$Type == "Paired", ]
    expect_true(nrow(paired_rows) > 0)
  })

  it("has correct columns for unpaired results", {
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
    unpaired_rows <- result[result$Type == "Unpaired", ]
    expect_true(nrow(unpaired_rows) > 0)
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
    expect_true("Type" %in% names(result))
    expect_true(nrow(result) > 0)
  })
})
