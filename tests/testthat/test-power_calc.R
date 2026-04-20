box::use(
  testthat[describe, expect_equal, expect_true, expect_false, it],
)

box::use(
  app/logic/shared/error_handling,
  app/logic/power/power_calc,
)

# =============================================================================
# Helper: create minimal valid power analysis params
# =============================================================================

make_power_params <- function(
  solve_for = "sample_size",
  alpha = 0.05,
  power_target = 0.80,
  n_per_group = 20,
  effect_size = 0.25,
  effect_type = "standardized",
  n_groups = 3,
  n_ways = 1,
  approach = "parametric",
  n_sim = 100
) {
  list(
    solve_for = solve_for,
    alpha = alpha,
    power_target = power_target,
    n_per_group = n_per_group,
    effect_size = effect_size,
    effect_type = effect_type,
    group_means = NULL,
    group_sd = NULL,
    n_groups = n_groups,
    n_ways = n_ways,
    approach = approach,
    n_sim = n_sim
  )
}

# =============================================================================
# solve_sample_size — parametric
# =============================================================================

describe("solve_sample_size", {
  it("returns a positive integer N for valid parametric 1-way input", {
    params <- make_power_params(
      solve_for = "sample_size",
      effect_size = 0.25,
      n_groups = 3,
      approach = "parametric"
    )
    result <- power_calc$perform_power_analysis(params)

    expect_false(error_handling$is_app_error(result))
    expect_true(result$result$value > 0)
    expect_true(result$result$value == floor(result$result$value))
    expect_equal(result$result$type, "sample_size")
  })

  it("returns a list with power_curve_df containing n and power columns", {
    params <- make_power_params(
      solve_for = "sample_size",
      effect_size = 0.25,
      n_groups = 3,
      approach = "parametric"
    )
    result <- power_calc$perform_power_analysis(params)

    expect_false(error_handling$is_app_error(result))
    expect_true(is.data.frame(result$power_curve_df))
    expect_true("n" %in% names(result$power_curve_df))
    expect_true("power" %in% names(result$power_curve_df))
  })

  it("returns an app error for alpha >= 1", {
    params <- make_power_params(
      solve_for = "sample_size",
      alpha = 1.5
    )
    result <- power_calc$perform_power_analysis(params)

    expect_true(error_handling$is_app_error(result))
  })

  it("returns an app error for alpha <= 0", {
    params <- make_power_params(
      solve_for = "sample_size",
      alpha = 0
    )
    result <- power_calc$perform_power_analysis(params)

    expect_true(error_handling$is_app_error(result))
  })
})

# =============================================================================
# solve_power — parametric
# =============================================================================

describe("solve_power", {
  it("returns a numeric in (0,1) for valid N + effect", {
    params <- make_power_params(
      solve_for = "power",
      n_per_group = 30,
      effect_size = 0.25,
      n_groups = 3,
      approach = "parametric"
    )
    result <- power_calc$perform_power_analysis(params)

    expect_false(error_handling$is_app_error(result))
    expect_true(result$result$value > 0)
    expect_true(result$result$value < 1)
    expect_equal(result$result$type, "power")
  })

  it("handles 2-way parametric design", {
    params <- make_power_params(
      solve_for = "power",
      n_per_group = 20,
      effect_size = 0.30,
      n_groups = 4,
      n_ways = 2,
      approach = "parametric"
    )
    result <- power_calc$perform_power_analysis(params)

    expect_false(error_handling$is_app_error(result))
    expect_true(result$result$value > 0)
    expect_true(result$result$value < 1)
  })
})

# =============================================================================
# solve_mde — parametric
# =============================================================================

describe("solve_mde", {
  it("returns a positive numeric effect size", {
    params <- make_power_params(
      solve_for = "mde",
      n_per_group = 30,
      power_target = 0.80,
      n_groups = 3,
      approach = "parametric"
    )
    result <- power_calc$perform_power_analysis(params)

    expect_false(error_handling$is_app_error(result))
    expect_true(result$result$value > 0)
    expect_equal(result$result$type, "mde")
  })
})

# =============================================================================
# simulation path — robust and non-parametric
# =============================================================================

describe("simulation path", {
  it("non-parametric 1-way returns a numeric power in (0,1) with n_sim = 100", {
    params <- make_power_params(
      solve_for = "power",
      n_per_group = 30,
      effect_size = 0.40,
      n_groups = 3,
      approach = "nonparametric",
      n_sim = 100
    )
    result <- power_calc$perform_power_analysis(params)

    expect_false(error_handling$is_app_error(result))
    expect_true(result$result$value >= 0)
    expect_true(result$result$value <= 1)
  })

  it("robust approach returns a numeric power in (0,1)", {
    params <- make_power_params(
      solve_for = "power",
      n_per_group = 30,
      effect_size = 0.40,
      n_groups = 3,
      approach = "robust",
      n_sim = 100
    )
    result <- power_calc$perform_power_analysis(params)

    expect_false(error_handling$is_app_error(result))
    expect_true(result$result$value >= 0)
    expect_true(result$result$value <= 1)
  })

  it("returns an app error when n_sim is NA in simulation mode", {
    params <- make_power_params(
      solve_for = "power",
      n_per_group = 30,
      effect_size = 0.40,
      n_groups = 3,
      approach = "robust",
      n_sim = NA_real_
    )
    result <- power_calc$perform_power_analysis(params)

    expect_true(error_handling$is_app_error(result))
  })

  it("returns an app error when n_sim is non-finite in simulation mode", {
    params <- make_power_params(
      solve_for = "power",
      n_per_group = 30,
      effect_size = 0.40,
      n_groups = 3,
      approach = "nonparametric",
      n_sim = Inf
    )
    result <- power_calc$perform_power_analysis(params)

    expect_true(error_handling$is_app_error(result))
  })
})

# =============================================================================
# generate_power_curve
# =============================================================================

describe("generate_power_curve", {
  it("returns a data frame with n and power columns", {
    params <- make_power_params(
      effect_size = 0.25,
      n_groups = 3
    )
    curve <- power_calc$generate_power_curve(params)

    expect_true(is.data.frame(curve))
    expect_true("n" %in% names(curve))
    expect_true("power" %in% names(curve))
    expect_true(nrow(curve) > 0)
  })

  it("power increases with sample size", {
    params <- make_power_params(
      effect_size = 0.25,
      n_groups = 3
    )
    curve <- power_calc$generate_power_curve(params, n_range = c(10, 50, 100))

    expect_true(curve$power[3] > curve$power[1])
  })
})

# =============================================================================
# raw effect type with per-group SDs
# =============================================================================

describe("raw effect type with per-group SDs", {
  it("handles vector of per-group SDs correctly", {
    params <- list(
      solve_for = "power",
      alpha = 0.05,
      power_target = 0.80,
      n_per_group = 20,
      effect_type = "raw",
      group_means = c(A = 10, B = 10.5),
      group_sd = c(1, 2),
      n_groups = 2,
      n_ways = 1,
      approach = "parametric",
      n_sim = 100
    )
    result <- power_calc$perform_power_analysis(params)

    expect_false(error_handling$is_app_error(result))
    expect_true(result$result$value > 0)
    expect_true(result$result$value < 1)
  })

  it("computes different Cohen's f for different SD vectors", {
    # Same means, different SDs should produce different effect sizes
    params_small_sd <- list(
      solve_for = "power",
      alpha = 0.05,
      power_target = 0.80,
      n_per_group = 20,
      effect_type = "raw",
      group_means = c(A = 10, B = 11),
      group_sd = c(1, 1),
      n_groups = 2,
      n_ways = 1,
      approach = "parametric",
      n_sim = 100
    )

    params_large_sd <- list(
      solve_for = "power",
      alpha = 0.05,
      power_target = 0.80,
      n_per_group = 20,
      effect_type = "raw",
      group_means = c(A = 10, B = 11),
      group_sd = c(4, 4),
      n_groups = 2,
      n_ways = 1,
      approach = "parametric",
      n_sim = 100
    )

    result_small <- power_calc$perform_power_analysis(params_small_sd)
    result_large <- power_calc$perform_power_analysis(params_large_sd)

    expect_false(error_handling$is_app_error(result_small))
    expect_false(error_handling$is_app_error(result_large))
    # Smaller SD = larger effect size = higher power
    expect_true(result_small$effect_f > result_large$effect_f)
    expect_true(result_small$result$value > result_large$result$value)
  })
})
