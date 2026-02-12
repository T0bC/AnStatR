box::use(
  testthat[describe, expect_equal, expect_true, expect_false, it],
)

box::use(
  app/logic/error_handling,
  app/logic/statistics/robust_tests,
)

# =============================================================================
# Helper: create test data with multiple groups and a numeric measure
# =============================================================================

make_oneway_data <- function(n_per_group = 20, n_groups = 3) {
  set.seed(42)
  groups <- rep(paste0("G", seq_len(n_groups)), each = n_per_group)
  values <- rnorm(n_per_group * n_groups, mean = rep(1:n_groups, each = n_per_group))
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

# =============================================================================
# perform_t1way — happy path
# =============================================================================

describe("perform_t1way", {
  it("returns a data frame with expected columns for valid 1-way data", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- robust_tests$perform_t1way(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      tr_value = 0.2,
      use_bootstrap = FALSE
    )
    expect_true(is.data.frame(result))
    expect_equal(
      names(result),
      c("F_statistic", "df1", "df2", "Effect_Size", "p_value")
    )
    expect_equal(nrow(result), 1)
  })

  it("returns numeric values in the result", {
    df <- make_oneway_data(n_per_group = 15, n_groups = 4)
    result <- robust_tests$perform_t1way(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      tr_value = 0.2,
      use_bootstrap = FALSE
    )
    expect_true(is.numeric(result$F_statistic))
    expect_true(is.numeric(result$p_value))
    expect_true(result$p_value >= 0 && result$p_value <= 1)
  })

  it("works with 2 groups (minimum)", {
    df <- make_oneway_data(n_per_group = 15, n_groups = 2)
    result <- robust_tests$perform_t1way(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      tr_value = 0.2,
      use_bootstrap = FALSE
    )
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 1)
  })
})

# =============================================================================
# perform_t1way — validation errors
# =============================================================================

describe("perform_t1way validation", {
  it("returns app_error when only 1 group exists", {
    df <- data.frame(
      group = rep("A", 10),
      measure = rnorm(10),
      stringsAsFactors = FALSE
    )
    result <- robust_tests$perform_t1way(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      tr_value = 0.2
    )
    expect_true(error_handling$is_app_error(result))
  })

  it("returns app_error when x_axis has 2 variables", {
    df <- make_oneway_data()
    df$group2 <- rep(c("X", "Y"), length.out = nrow(df))
    result <- robust_tests$perform_t1way(
      df = df,
      x_axis = c("group", "group2"),
      measure_col = "measure",
      tr_value = 0.2
    )
    expect_true(error_handling$is_app_error(result))
  })
})

# =============================================================================
# perform_t1way — bootstrap mode
# =============================================================================

describe("perform_t1way bootstrap", {
  it("returns formatted CI results in bootstrap mode", {
    df <- make_oneway_data(n_per_group = 20, n_groups = 3)
    result <- robust_tests$perform_t1way(
      df = df,
      x_axis = "group",
      measure_col = "measure",
      tr_value = 0.2,
      use_bootstrap = TRUE,
      boot_samples = 5,
      boot_sample_size = NULL
    )
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 1)
    # Bootstrap results are formatted as "mean [lower - upper]"
    expect_true(grepl("\\[", result$F_statistic))
  })
})

# =============================================================================
# perform_t2way — happy path
# =============================================================================

describe("perform_t2way", {
  it("returns a data frame with Effect, Q.Statistic, p.value", {
    df <- make_twoway_data(n_per_cell = 10)
    result <- robust_tests$perform_t2way(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure",
      tr_value = 0.2,
      use_bootstrap = FALSE
    )
    expect_true(is.data.frame(result))
    expect_equal(
      names(result),
      c("Effect", "Q.Statistic", "p.value")
    )
    expect_equal(nrow(result), 3)
  })

  it("returns correct effect labels", {
    df <- make_twoway_data(n_per_cell = 10)
    result <- robust_tests$perform_t2way(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure",
      tr_value = 0.2,
      use_bootstrap = FALSE
    )
    expect_equal(
      result$Effect,
      c("f1", "f2", "f1:f2")
    )
  })

  it("returns numeric Q statistics and p-values", {
    df <- make_twoway_data(n_per_cell = 15)
    result <- robust_tests$perform_t2way(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure",
      tr_value = 0.2,
      use_bootstrap = FALSE
    )
    expect_true(is.numeric(result$Q.Statistic))
    expect_true(all(result$p.value >= 0 & result$p.value <= 1))
  })
})

# =============================================================================
# perform_t2way — validation errors
# =============================================================================

describe("perform_t2way validation", {
  it("returns app_error when only 1 grouping variable given", {
    df <- make_twoway_data()
    result <- robust_tests$perform_t2way(
      df = df,
      x_axis = "f1",
      measure_col = "measure",
      tr_value = 0.2
    )
    expect_true(error_handling$is_app_error(result))
  })

  it("returns app_error when 3 grouping variables given", {
    df <- make_twoway_data()
    df$f3 <- rep(c("P", "Q"), length.out = nrow(df))
    result <- robust_tests$perform_t2way(
      df = df,
      x_axis = c("f1", "f2", "f3"),
      measure_col = "measure",
      tr_value = 0.2
    )
    expect_true(error_handling$is_app_error(result))
  })

  it("returns app_error when a factor has only 1 level", {
    df <- make_twoway_data()
    df$f1 <- "A"
    result <- robust_tests$perform_t2way(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure",
      tr_value = 0.2
    )
    expect_true(error_handling$is_app_error(result))
  })
})

# =============================================================================
# perform_t2way — bootstrap mode
# =============================================================================

describe("perform_t2way bootstrap", {
  it("returns formatted CI results in bootstrap mode", {
    df <- make_twoway_data(n_per_cell = 10)
    result <- robust_tests$perform_t2way(
      df = df,
      x_axis = c("f1", "f2"),
      measure_col = "measure",
      tr_value = 0.2,
      use_bootstrap = TRUE,
      boot_samples = 5,
      boot_sample_size = NULL
    )
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)
    # Bootstrap results are formatted as "mean [CI]"
    expect_true(grepl("\\[", result$Q.Statistic[1]))
    expect_true(grepl("\\[", result$p.value[1]))
  })
})
