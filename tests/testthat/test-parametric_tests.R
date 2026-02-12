box::use(
  testthat[describe, expect_equal, expect_true, expect_false, it],
)

box::use(
  app/logic/error_handling,
  app/logic/statistics/parametric_tests,
)

# =============================================================================
# Helper: create test data with multiple groups and a numeric measure
# =============================================================================

make_oneway_data <- function(n_per_group = 20, n_groups = 3) {
  set.seed(42)
  groups <- rep(paste0("G", seq_len(n_groups)), each = n_per_group)
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

# =============================================================================
# perform_anova1way — happy path
# =============================================================================

describe("perform_anova1way", {
  it("returns a data frame with expected columns for valid 1-way data", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- parametric_tests$perform_anova1way(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_true(is.data.frame(result))
    expect_equal(
      names(result),
      c("Effect", "Df", "SS", "MS", "F.Statistic", "p.value")
    )
    expect_equal(nrow(result), 1)
  })

  it("returns correct effect label", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- parametric_tests$perform_anova1way(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_equal(result$Effect, "group")
  })

  it("returns numeric values in the result", {
    df <- make_oneway_data(n_per_group = 15, n_groups = 4)
    result <- parametric_tests$perform_anova1way(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_true(is.numeric(result$F.Statistic))
    expect_true(is.numeric(result$p.value))
    expect_true(result$p.value >= 0 && result$p.value <= 1)
  })

  it("returns integer Df", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- parametric_tests$perform_anova1way(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_true(is.integer(result$Df))
    expect_equal(result$Df, 2L)
  })

  it("works with 2 groups (minimum)", {
    df <- make_oneway_data(n_per_group = 15, n_groups = 2)
    result <- parametric_tests$perform_anova1way(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 1)
    expect_equal(result$Df, 1L)
  })

  it("detects significant differences between groups", {
    set.seed(123)
    df <- data.frame(
      group = rep(c("Low", "High"), each = 30),
      measure = c(rnorm(30, mean = 0), rnorm(30, mean = 5)),
      stringsAsFactors = FALSE
    )
    result <- parametric_tests$perform_anova1way(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_true(result$p.value < 0.05)
    expect_true(result$F.Statistic > 1)
  })
})

# =============================================================================
# perform_anova1way — validation errors
# =============================================================================

describe("perform_anova1way validation", {
  it("returns app_error when only 1 group exists", {
    df <- data.frame(
      group = rep("A", 10),
      measure = rnorm(10),
      stringsAsFactors = FALSE
    )
    result <- parametric_tests$perform_anova1way(
      df = df,
      x_axis = "group",
      measure_col = "measure"
    )
    expect_true(error_handling$is_app_error(result))
  })

  it("returns app_error when x_axis has 2 variables", {
    df <- make_oneway_data()
    df$group2 <- rep(c("X", "Y"), length.out = nrow(df))
    result <- parametric_tests$perform_anova1way(
      df = df,
      x_axis = c("group", "group2"),
      measure_col = "measure"
    )
    expect_true(error_handling$is_app_error(result))
  })
})

# =============================================================================
# perform_anova1way — bootstrap is silently ignored
# =============================================================================

describe("perform_anova1way ignores bootstrap", {
  it("returns same result regardless of bootstrap flag", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result_no_boot <- parametric_tests$perform_anova1way(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      use_bootstrap = FALSE
    )
    result_with_boot <- parametric_tests$perform_anova1way(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      use_bootstrap = TRUE,
      boot_samples = 10
    )
    expect_equal(result_no_boot, result_with_boot)
  })
})

# =============================================================================
# Helper: create test data with two factors
# =============================================================================

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

# =============================================================================
# perform_anova2way — happy path
# =============================================================================

describe("perform_anova2way", {
  it("returns a data frame with expected columns", {
    df <- make_twoway_data(n_per_cell = 10)
    result <- parametric_tests$perform_anova2way(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure"
    )
    expect_true(is.data.frame(result))
    expect_equal(
      names(result),
      c("Effect", "Df", "SS", "MS",
        "F.Statistic", "p.value")
    )
    expect_equal(nrow(result), 3)
  })

  it("returns correct effect labels", {
    df <- make_twoway_data(n_per_cell = 10)
    result <- parametric_tests$perform_anova2way(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure"
    )
    expect_equal(
      result$Effect,
      c("f1", "f2", "f1:f2")
    )
  })

  it("returns numeric F statistics and p-values", {
    df <- make_twoway_data(n_per_cell = 15)
    result <- parametric_tests$perform_anova2way(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure"
    )
    expect_true(is.numeric(result$F.Statistic))
    expect_true(all(result$p.value >= 0 & result$p.value <= 1))
  })

  it("returns integer Df values", {
    df <- make_twoway_data(n_per_cell = 10)
    result <- parametric_tests$perform_anova2way(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure"
    )
    expect_true(is.integer(result$Df))
    # 2 levels each => Df=1 for main effects, Df=1 for interaction
    expect_equal(result$Df, c(1L, 1L, 1L))
  })

  it("detects main effect of factor with large difference", {
    set.seed(99)
    df <- expand.grid(
      f1 = c("Low", "High"),
      f2 = c("X", "Y"),
      stringsAsFactors = FALSE
    )
    df <- df[rep(seq_len(nrow(df)), each = 20), ]
    df$measure <- rnorm(nrow(df)) +
      ifelse(df$f1 == "High", 5, 0)
    result <- parametric_tests$perform_anova2way(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure"
    )
    # f1 should be highly significant
    expect_true(result$p.value[1] < 0.05)
  })
})

# =============================================================================
# perform_anova2way — validation errors
# =============================================================================

describe("perform_anova2way validation", {
  it("returns app_error when only 1 grouping variable given", {
    df <- make_twoway_data()
    result <- parametric_tests$perform_anova2way(
      df = df,
      x_axis = "f1",
      measure_col = "measure"
    )
    expect_true(error_handling$is_app_error(result))
  })

  it("returns app_error when 3 grouping variables given", {
    df <- make_twoway_data()
    df$f3 <- rep(c("P", "Q"), length.out = nrow(df))
    result <- parametric_tests$perform_anova2way(
      df = df,
      x_axis = c("f1", "f2", "f3"),
      measure_col = "measure"
    )
    expect_true(error_handling$is_app_error(result))
  })

  it("returns app_error when a factor has only 1 level", {
    df <- make_twoway_data()
    df$f1 <- "A"
    result <- parametric_tests$perform_anova2way(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure"
    )
    expect_true(error_handling$is_app_error(result))
  })
})

# =============================================================================
# perform_anova2way — bootstrap is silently ignored
# =============================================================================

describe("perform_anova2way ignores bootstrap", {
  it("returns same result regardless of bootstrap flag", {
    df <- make_twoway_data(n_per_cell = 10)
    result_no_boot <- parametric_tests$perform_anova2way(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure",
      use_bootstrap = FALSE
    )
    result_with_boot <- parametric_tests$perform_anova2way(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure",
      use_bootstrap = TRUE,
      boot_samples = 10
    )
    expect_equal(result_no_boot, result_with_boot)
  })
})

# =============================================================================
# Helper: create test data with three factors
# =============================================================================

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
# perform_anova3way — happy path
# =============================================================================

describe("perform_anova3way", {
  it("returns a data frame with 7 effects", {
    df <- make_threeway_data(n_per_cell = 5)
    result <- parametric_tests$perform_anova3way(
      df = df,
      x_axis = c("f1", "f2", "f3"),
      measure_col = "measure"
    )
    expect_true(is.data.frame(result))
    expect_equal(
      names(result),
      c("Effect", "Df", "SS", "MS",
        "F.Statistic", "p.value")
    )
    expect_equal(nrow(result), 7)
  })

  it("returns correct effect labels", {
    df <- make_threeway_data(n_per_cell = 5)
    result <- parametric_tests$perform_anova3way(
      df = df,
      x_axis = c("f1", "f2", "f3"),
      measure_col = "measure"
    )
    expect_equal(
      result$Effect,
      c("f1", "f2", "f3", "f1:f2", "f1:f3",
        "f2:f3", "f1:f2:f3")
    )
  })

  it("returns numeric F statistics and p-values", {
    df <- make_threeway_data(n_per_cell = 5)
    result <- parametric_tests$perform_anova3way(
      df = df,
      x_axis = c("f1", "f2", "f3"),
      measure_col = "measure"
    )
    expect_true(is.numeric(result$F.Statistic))
    expect_true(all(result$p.value >= 0 & result$p.value <= 1))
  })

  it("returns integer Df values", {
    df <- make_threeway_data(n_per_cell = 5)
    result <- parametric_tests$perform_anova3way(
      df = df,
      x_axis = c("f1", "f2", "f3"),
      measure_col = "measure"
    )
    expect_true(is.integer(result$Df))
    # 2 levels each => Df=1 for all effects
    expect_equal(result$Df, rep(1L, 7))
  })
})

# =============================================================================
# perform_anova3way — validation errors
# =============================================================================

describe("perform_anova3way validation", {
  it("returns app_error when only 2 grouping variables given", {
    df <- make_threeway_data()
    result <- parametric_tests$perform_anova3way(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure"
    )
    expect_true(error_handling$is_app_error(result))
  })

  it("returns app_error when 1 grouping variable given", {
    df <- make_threeway_data()
    result <- parametric_tests$perform_anova3way(
      df = df,
      x_axis = "f1",
      measure_col = "measure"
    )
    expect_true(error_handling$is_app_error(result))
  })

  it("returns app_error when a factor has only 1 level", {
    df <- make_threeway_data()
    df$f3 <- "L"
    result <- parametric_tests$perform_anova3way(
      df = df,
      x_axis = c("f1", "f2", "f3"),
      measure_col = "measure"
    )
    expect_true(error_handling$is_app_error(result))
  })
})

# =============================================================================
# perform_anova3way — bootstrap is silently ignored
# =============================================================================

describe("perform_anova3way ignores bootstrap", {
  it("returns same result regardless of bootstrap flag", {
    df <- make_threeway_data(n_per_cell = 5)
    result_no_boot <- parametric_tests$perform_anova3way(
      df = df,
      x_axis = c("f1", "f2", "f3"),
      measure_col = "measure",
      use_bootstrap = FALSE
    )
    result_with_boot <- parametric_tests$perform_anova3way(
      df = df,
      x_axis = c("f1", "f2", "f3"),
      measure_col = "measure",
      use_bootstrap = TRUE,
      boot_samples = 10
    )
    expect_equal(result_no_boot, result_with_boot)
  })
})
