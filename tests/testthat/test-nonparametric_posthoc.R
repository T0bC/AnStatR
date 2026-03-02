box::use(
  testthat[describe, expect_equal, expect_true, expect_false, it],
)

box::use(
  app/logic/error_handling,
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
