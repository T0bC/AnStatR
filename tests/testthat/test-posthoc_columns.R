box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/statistics/nonparametric_posthoc,
  app/logic/statistics/parametric_posthoc,
  app/logic/statistics/posthoc_columns,
)

# =============================================================================
# Synthetic fixtures — one per schema
# =============================================================================

make_parametric_df <- function() {
  data.frame(
    Interaction = c("A vs. B", "A vs. C"),
    Tukey.diff = c(1.2, -0.4),
    Tukey.p.value = c(0.01, 0.30),
    Tukey.p.adjusted = c(0.03, 0.60),
    Cohen.d = c(0.8, -0.2),
    stringsAsFactors = FALSE
  )
}

make_robust_df <- function() {
  data.frame(
    Interaction = c("A vs. B", "A vs. C"),
    Lincon.psihat = c(0.5, 0.1),
    Lincon.p.value = c(0.02, 0.40),
    Lincon.p.adjusted = c(0.04, 0.80),
    Cliff.psihat = c(0.7, 0.55),
    stringsAsFactors = FALSE
  )
}

make_art_df <- function() {
  data.frame(
    Interaction = c("A vs. B", "A vs. C"),
    ART.estimate = c(1.0, 0.5),
    ART.p.value = c(0.01, 0.20),
    ART.p.adjusted = c(0.02, 0.40),
    ART.d = c(0.9, 0.3),
    stringsAsFactors = FALSE
  )
}

make_unknown_df <- function() {
  data.frame(
    Interaction = c("A vs. B"),
    Foo.p.value = c(0.05),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# detect_posthoc_schema
# =============================================================================

describe("detect_posthoc_schema", {
  it("resolves the parametric schema", {
    schema <- posthoc_columns$detect_posthoc_schema(make_parametric_df())
    expect_equal(schema$approach, "parametric")
    expect_equal(schema$p_raw_col, "Tukey.p.value")
    expect_equal(schema$effect_col, "Cohen.d")
    expect_equal(schema$effect_null, 0)
  })

  it("resolves the robust schema with Cliff.psihat and null 0.5", {
    schema <- posthoc_columns$detect_posthoc_schema(make_robust_df())
    expect_equal(schema$approach, "robust")
    expect_equal(schema$p_raw_col, "Lincon.p.value")
    expect_equal(schema$effect_col, "Cliff.psihat")
    expect_equal(schema$effect_null, 0.5)
  })

  it("resolves the nonparametric multiway (ART) schema", {
    schema <- posthoc_columns$detect_posthoc_schema(make_art_df())
    expect_equal(schema$approach, "nonparametric_multiway")
    expect_equal(schema$p_raw_col, "ART.p.value")
    expect_equal(schema$effect_col, "ART.d")
    expect_equal(schema$effect_null, 0)
  })

  it("gives an unknown frame NA p and effect columns", {
    schema <- posthoc_columns$detect_posthoc_schema(make_unknown_df())
    expect_equal(schema$approach, "unknown")
    expect_true(is.na(schema$p_raw_col))
    expect_true(is.na(schema$effect_col))
  })

  it("prioritizes rm_parametric when both Paired.t and Tukey columns exist", {
    df <- make_parametric_df()
    df$Paired.t.statistic <- c(1, 2)
    df$Paired.t.p.value <- c(0.01, 0.02)
    df$Paired.t.p.adjusted <- c(0.02, 0.04)
    df$Paired.d <- c(0.5, 0.6)
    schema <- posthoc_columns$detect_posthoc_schema(df)
    expect_equal(schema$approach, "rm_parametric")
    expect_equal(schema$p_raw_col, "Paired.t.p.value")
  })

  it("distinguishes Dunn from Wilcox within nonparametric_1way", {
    dunn_df <- data.frame(
      Interaction = "A vs. B",
      Dunn.Z = 1.5,
      Dunn.p.value = 0.02,
      Dunn.p.adjusted = 0.04,
      Cliff.psihat = 0.6,
      stringsAsFactors = FALSE
    )
    wilcox_df <- data.frame(
      Interaction = "A vs. B",
      Wilcox.p.value = 0.02,
      Wilcox.p.adjusted = 0.04,
      Cliff.psihat = 0.6,
      stringsAsFactors = FALSE
    )
    dunn_schema <- posthoc_columns$detect_posthoc_schema(dunn_df)
    wilcox_schema <- posthoc_columns$detect_posthoc_schema(wilcox_df)
    expect_equal(dunn_schema$left_prefix, "Dunn")
    expect_equal(wilcox_schema$left_prefix, "Wilcox")
  })
})

# =============================================================================
# Golden test — real output from parametric_posthoc
# =============================================================================

describe("detect_posthoc_schema golden test (real posthoc output)", {
  it("resolves columns that actually exist in real parametric output", {
    set.seed(1)
    df <- data.frame(
      GROUP = factor(rep(c("A", "B", "C"), each = 15)),
      value = c(
        rnorm(15, mean = 0), rnorm(15, mean = 1), rnorm(15, mean = 2)
      )
    )
    result <- parametric_posthoc$perform_combined_parametric_posthoc(
      df = df,
      x_axis = "GROUP",
      measure_col = "value",
      p_adjust_method = "bonferroni",
      filter_valid = FALSE,
      is_rm = FALSE,
      id_col = NULL,
      within_col = NULL
    )
    schema <- posthoc_columns$detect_posthoc_schema(result)
    expect_true(schema$p_raw_col %in% names(result))
    expect_true(schema$p_adj_col %in% names(result))
    expect_true(schema$effect_col %in% names(result))
  })

  it("resolves columns that actually exist in real robust/nonparametric output", {
    set.seed(1)
    df <- data.frame(
      GROUP = factor(rep(c("A", "B", "C"), each = 15)),
      value = c(
        rnorm(15, mean = 0), rnorm(15, mean = 1), rnorm(15, mean = 2)
      )
    )
    result <- nonparametric_posthoc$perform_combined_nonparametric_posthoc(
      df = df,
      x_axis = "GROUP",
      measure_col = "value",
      p_adjust_method = "bonferroni",
      filter_valid = FALSE,
      posthoc_method = "dunn",
      is_rm = FALSE,
      id_col = NULL,
      within_col = NULL
    )
    schema <- posthoc_columns$detect_posthoc_schema(result)
    expect_true(schema$p_raw_col %in% names(result))
    expect_true(schema$p_adj_col %in% names(result))
    expect_true(schema$effect_col %in% names(result))
  })
})
