box::use(
  testthat[describe, expect_equal, expect_false, expect_true, it],
)

box::use(
  app/logic/plotting/assumption_checks,
)

# =============================================================================
# Test data helpers
# =============================================================================

make_normal_data <- function() {
  set.seed(42)
  data.frame(
    SPECIES = rep(c("A", "B", "C"), each = 50),
    value = c(rnorm(50, 10, 2), rnorm(50, 12, 2), rnorm(50, 11, 2)),
    value_outlier = FALSE,
    value_trimmed = FALSE,
    stringsAsFactors = FALSE
  )
}

make_nonnormal_data <- function() {
  set.seed(42)
  data.frame(
    SPECIES = rep(c("A", "B", "C"), each = 50),
    value = c(rexp(50, 0.5), rexp(50, 0.3), rexp(50, 0.1)),
    value_outlier = FALSE,
    value_trimmed = FALSE,
    stringsAsFactors = FALSE
  )
}

make_mixed_data <- function() {
  set.seed(42)
  data.frame(
    SPECIES = rep(c("A", "B", "C"), each = 50),
    value = c(rnorm(50, 10, 2), rnorm(50, 12, 2), rexp(50, 0.1)),
    value_outlier = FALSE,
    value_trimmed = FALSE,
    stringsAsFactors = FALSE
  )
}

make_outlier_flagged_data <- function() {
  set.seed(42)
  df <- make_normal_data()
  # Flag some as outliers
  df$value_outlier[c(1, 2, 51, 52)] <- TRUE
  df
}

# =============================================================================
# check_normality
# =============================================================================

describe("check_normality", {
  it("returns a data frame with correct columns", {
    df <- make_normal_data()
    grp <- factor(df$SPECIES)
    result <- assumption_checks$check_normality(df, "value", grp)

    expect_true(is.data.frame(result))
    expect_equal(
      names(result),
      c("group", "n", "W", "p_value", "normal")
    )
  })

  it("detects normally distributed groups", {
    df <- make_normal_data()
    grp <- factor(df$SPECIES)
    result <- assumption_checks$check_normality(df, "value", grp)

    expect_equal(nrow(result), 3)
    # With normally distributed data, most groups should pass
    n_normal <- sum(result$normal == "yes", na.rm = TRUE)
    expect_true(n_normal >= 2)
  })

  it("respects outlier flag columns", {
    df <- make_outlier_flagged_data()
    grp <- factor(df$SPECIES)
    result <- assumption_checks$check_normality(df, "value", grp)

    # Group A should have n = 48 (50 - 2 outliers)
    group_a <- result[result$group == "A", ]
    expect_equal(group_a$n, 48)
  })

  it("handles small groups gracefully", {
    df <- data.frame(
      group = c("A", "A", "B", "B"),
      value = c(1, 2, 3, 4),
      stringsAsFactors = FALSE
    )
    grp <- factor(df$group)
    result <- assumption_checks$check_normality(df, "value", grp)

    # n < 3 per group → NA results
    expect_true(all(is.na(result$W)))
  })
})

# =============================================================================
# check_homogeneity
# =============================================================================

describe("check_homogeneity", {
  it("returns correct structure", {
    df <- make_normal_data()
    grp <- factor(df$SPECIES)
    result <- assumption_checks$check_homogeneity(df, "value", grp)

    expect_true(is.list(result))
    expect_true(all(c(
      "F_statistic", "df1", "df2", "p_value", "equal_variances"
    ) %in% names(result)))
  })

  it("detects equal variances for similar distributions", {
    df <- make_normal_data()
    grp <- factor(df$SPECIES)
    result <- assumption_checks$check_homogeneity(df, "value", grp)

    expect_true(!is.na(result$p_value))
    expect_equal(result$df1, 2)
    expect_equal(result$df2, 147)
  })

  it("detects unequal variances when present", {
    set.seed(42)
    df <- data.frame(
      group = rep(c("A", "B"), each = 50),
      value = c(rnorm(50, 10, 1), rnorm(50, 10, 10)),
      value_outlier = FALSE,
      value_trimmed = FALSE,
      stringsAsFactors = FALSE
    )
    grp <- factor(df$group)
    result <- assumption_checks$check_homogeneity(df, "value", grp)

    expect_equal(result$equal_variances, "no")
  })

  it("returns NA for single group", {
    df <- data.frame(
      group = rep("A", 20),
      value = rnorm(20),
      stringsAsFactors = FALSE
    )
    grp <- factor(df$group)
    result <- assumption_checks$check_homogeneity(df, "value", grp)

    expect_true(is.na(result$p_value))
  })
})

# =============================================================================
# recommend_transformation
# =============================================================================

describe("recommend_transformation", {
  it("does not recommend when all groups are normal", {
    df <- make_normal_data()
    grp <- factor(df$SPECIES)
    norm <- assumption_checks$check_normality(df, "value", grp)

    # Force all normal for deterministic test
    norm$normal <- "yes"
    rec <- assumption_checks$recommend_transformation(norm, 0.5)

    expect_false(rec$recommend)
    expect_equal(rec$n_non_normal, 0)
  })

  it("recommends when proportion exceeds threshold", {
    norm_df <- data.frame(
      group = c("A", "B", "C", "D"),
      n = c(50, 50, 50, 50),
      W = c(0.99, 0.80, 0.75, 0.70),
      p_value = c(0.5, 0.01, 0.001, 0.001),
      normal = c("yes", "no", "no", "no"),
      stringsAsFactors = FALSE
    )
    rec <- assumption_checks$recommend_transformation(norm_df, 0.5)

    expect_true(rec$recommend)
    expect_equal(rec$n_non_normal, 3)
    expect_equal(rec$n_groups, 4)
  })

  it("does not recommend when below threshold", {
    norm_df <- data.frame(
      group = c("A", "B", "C", "D"),
      n = c(50, 50, 50, 50),
      W = c(0.99, 0.99, 0.99, 0.70),
      p_value = c(0.5, 0.5, 0.5, 0.001),
      normal = c("yes", "yes", "yes", "no"),
      stringsAsFactors = FALSE
    )
    rec <- assumption_checks$recommend_transformation(norm_df, 0.5)

    expect_false(rec$recommend)
    expect_equal(rec$n_non_normal, 1)
  })
})

# =============================================================================
# build_recommendation_banner
# =============================================================================

describe("build_recommendation_banner", {
  it("returns success class when all normal + equal variances", {
    rec <- list(
      recommend = FALSE, n_non_normal = 0, n_groups = 3,
      proportion = 0,
      message = "All groups are normally distributed."
    )
    levene <- list(
      F_statistic = 1, df1 = 2, df2 = 100,
      p_value = 0.5, equal_variances = "yes"
    )
    banner <- assumption_checks$build_recommendation_banner(rec, levene)

    expect_equal(banner$css_class, "success")
  })

  it("returns danger class when transformation recommended", {
    rec <- list(
      recommend = TRUE, n_non_normal = 3, n_groups = 4,
      proportion = 0.75,
      message = "3/4 groups non-normal."
    )
    levene <- list(
      F_statistic = 1, df1 = 2, df2 = 100,
      p_value = 0.5, equal_variances = "yes"
    )
    banner <- assumption_checks$build_recommendation_banner(rec, levene)

    expect_equal(banner$css_class, "danger")
  })

  it("returns danger when variances are unequal", {
    rec <- list(
      recommend = FALSE, n_non_normal = 0, n_groups = 3,
      proportion = 0,
      message = "All groups normally distributed."
    )
    levene <- list(
      F_statistic = 10, df1 = 2, df2 = 100,
      p_value = 0.001, equal_variances = "no"
    )
    banner <- assumption_checks$build_recommendation_banner(rec, levene)

    expect_equal(banner$css_class, "danger")
  })
})

# =============================================================================
# check_normality_residuals
# =============================================================================

describe("check_normality_residuals", {
  it("returns correct structure", {
    df <- make_normal_data()
    grp <- factor(df$SPECIES)
    result <- assumption_checks$check_normality_residuals(
      df, "value", grp
    )

    expect_true(is.list(result))
    expect_equal(
      sort(names(result)),
      sort(c("n", "W", "p_value", "normal"))
    )
  })

  it("detects normal residuals for normally distributed data", {
    df <- make_normal_data()
    grp <- factor(df$SPECIES)
    result <- assumption_checks$check_normality_residuals(
      df, "value", grp
    )

    expect_true(!is.na(result$W))
    expect_equal(result$n, 150)
    # Normal data → residuals should typically pass
    expect_equal(result$normal, "yes")
  })

  it("detects non-normal residuals for skewed data", {
    df <- make_nonnormal_data()
    grp <- factor(df$SPECIES)
    result <- assumption_checks$check_normality_residuals(
      df, "value", grp
    )

    expect_true(!is.na(result$W))
    expect_equal(result$normal, "no")
  })

  it("respects outlier flag columns", {
    df <- make_outlier_flagged_data()
    grp <- factor(df$SPECIES)
    result <- assumption_checks$check_normality_residuals(
      df, "value", grp
    )

    # 150 - 4 outliers = 146
    expect_equal(result$n, 146)
  })

  it("returns NA for single group", {
    df <- data.frame(
      group = rep("A", 20),
      value = rnorm(20),
      stringsAsFactors = FALSE
    )
    grp <- factor(df$group)
    result <- assumption_checks$check_normality_residuals(
      df, "value", grp
    )

    expect_true(is.na(result$W))
  })
})

# =============================================================================
# format_p
# =============================================================================

describe("format_p", {
  it("formats very small p-values", {
    expect_equal(assumption_checks$format_p(0.0001), "< 0.001")
  })

  it("formats regular p-values", {
    expect_equal(assumption_checks$format_p(0.05), "0.050")
  })

  it("handles NA", {
    expect_equal(assumption_checks$format_p(NA), "NA")
  })
})
