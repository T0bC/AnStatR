box::use(
  pwr,
  stats[anova, aov, kruskal.test, lm, oneway.test, pf, rnorm, var],
)

box::use(
  app/logic/shared/error_handling,
  app/logic/power/validate,
)

# =============================================================================
# Public API
# =============================================================================

#' Perform power analysis for ANOVA designs
#'
#' @param params List containing:
#'   - solve_for: "sample_size", "power", or "mde"
#'   - alpha: significance level
#'   - power_target: desired power (for sample_size/mde modes)
#'   - n_per_group: sample size per group (for power/mde modes)
#'   - effect_size: Cohen's f (standardized mode)
#'   - effect_type: "standardized" or "raw"
#'
#'   - group_means: numeric vector (raw mode)
#'   - group_sd: pooled SD (raw mode)
#'   - n_groups: number of groups (1-way) or total cells (factorial)
#'   - n_ways: 1, 2, or 3 (factorial design complexity)
#'   - approach: "parametric", "robust", or "nonparametric"
#'   - n_sim: number of simulations (for non-parametric/robust)
#' @return List with: result, power_curve_df, design_table_df
#' @export
perform_power_analysis <- function(params) {
  # Validate inputs
  validation_error <- validate$validate_power_inputs(params)
  if (!is.null(validation_error)) {
    return(validation_error)
  }

  # Convert raw effect to Cohen's f if needed
  effect_f <- if (params$effect_type == "raw") {
    raw_to_cohens_f(params$group_means, params$group_sd)
  } else {
    params$effect_size
  }

  # Route to appropriate method
  if (params$approach == "parametric") {
    result <- parametric_power(params, effect_f)
  } else {
    result <- simulation_power(params, effect_f)
  }

  result
}

#' Generate power curve data
#'
#' @param params Power analysis parameters
#' @param n_range Numeric vector of sample sizes to evaluate
#' @return Data frame with columns: n, power
#' @export
generate_power_curve <- function(params, n_range = NULL) {
  if (is.null(n_range)) {
    n_range <- seq(5, 100, by = 5)
  }

  effect_f <- if (params$effect_type == "raw") {
    raw_to_cohens_f(params$group_means, params$group_sd)
  } else {
    params$effect_size
  }

  k <- params$n_groups

  power_values <- sapply(n_range, function(n) {
    tryCatch({
      res <- pwr$pwr.anova.test(
        k = k,
        n = n,
        f = effect_f,
        sig.level = params$alpha
      )
      res$power
    }, error = function(e) NA_real_)
  })

  data.frame(
    n = n_range,
    power = power_values
  )
}

# =============================================================================
# Private helpers
# =============================================================================

#' Convert raw group means and SD to Cohen's f
#'
#' @param group_means Numeric vector of group means
#' @param group_sd Numeric vector of per-group SDs or single pooled SD
#' @return Cohen's f effect size
raw_to_cohens_f <- function(group_means, group_sd) {
  grand_mean <- mean(group_means)
  ss_between <- sum((group_means - grand_mean)^2)
  k <- length(group_means)

  # Compute pooled SD from per-group SDs if vector provided
  pooled_sd <- if (length(group_sd) > 1) {
    sqrt(mean(group_sd^2))
  } else {
    group_sd
  }

  # Cohen's f = sqrt(variance of means / pooled variance)
  sqrt(ss_between / k) / pooled_sd
}

#' Parametric power analysis using pwr package
#'
#' @param params Power analysis parameters
#' @param effect_f Cohen's f effect size
#' @return List with result, power_curve_df, design_table_df
parametric_power <- function(params, effect_f) {
  k <- params$n_groups

  result <- tryCatch({
    if (params$solve_for == "sample_size") {
      res <- pwr$pwr.anova.test(
        k = k,
        f = effect_f,
        sig.level = params$alpha,
        power = params$power_target
      )
      list(
        value = ceiling(res$n),
        type = "sample_size",
        description = paste0(
          "Required sample size per group: ", ceiling(res$n),
          " (total N = ", ceiling(res$n) * k, ")"
        )
      )
    } else if (params$solve_for == "power") {
      res <- pwr$pwr.anova.test(
        k = k,
        n = params$n_per_group,
        f = effect_f,
        sig.level = params$alpha
      )
      list(
        value = round(res$power, 4),
        type = "power",
        description = paste0(
          "Achieved power: ", round(res$power * 100, 1), "%"
        )
      )
    } else if (params$solve_for == "mde") {
      res <- pwr$pwr.anova.test(
        k = k,
        n = params$n_per_group,
        sig.level = params$alpha,
        power = params$power_target
      )
      list(
        value = round(res$f, 4),
        type = "mde",
        description = paste0(
          "Minimum detectable effect (Cohen's f): ", round(res$f, 4)
        )
      )
    }
  }, error = function(e) {
    error_handling$simple_error(
      message = paste0("Power calculation failed: ", e$message),
      operation_name = "parametric_power"
    )
  })

  if (error_handling$is_app_error(result)) {
    return(result)
  }

  # Generate power curve
  power_curve <- generate_power_curve(params)

  # Generate design table
  design_table <- generate_design_table(params, result$value)

  list(
    result = result,
    power_curve_df = power_curve,
    design_table_df = design_table,
    effect_f = effect_f,
    params = params
  )
}

#' Simulation-based power analysis for robust/non-parametric tests
#'
#' @param params Power analysis parameters
#' @param effect_f Cohen's f effect size
#' @return List with result, power_curve_df, design_table_df
simulation_power <- function(params, effect_f) {
  n_sim <- params$n_sim %||% 1000
  k <- params$n_groups
  alpha <- params$alpha

  # For simulation, we need group means
  if (params$effect_type == "raw") {
    group_means <- params$group_means
    # Compute pooled SD from per-group SDs if vector provided
    pooled_sd <- if (length(params$group_sd) > 1) {
      sqrt(mean(params$group_sd^2))
    } else {
      params$group_sd
    }
  } else {
    # Reconstruct means from effect size
    pooled_sd <- 1
    # Create means that produce the target Cohen's f
    group_means <- seq(0, effect_f * sqrt(k) * pooled_sd, length.out = k)
    group_means <- group_means - mean(group_means)
  }

  if (params$solve_for == "power") {
    n <- params$n_per_group
    power_est <- simulate_power(
      group_means, pooled_sd, n, n_sim, alpha, params$approach
    )

    result <- list(
      value = round(power_est, 4),
      type = "power",
      description = paste0(
        "Estimated power (", n_sim, " simulations): ",
        round(power_est * 100, 1), "%"
      )
    )
  } else if (params$solve_for == "sample_size") {
    # Binary search for required N
    n_required <- find_required_n(
      group_means, pooled_sd, params$power_target,
      n_sim, alpha, params$approach
    )

    result <- list(
      value = n_required,
      type = "sample_size",
      description = paste0(
        "Required sample size per group: ", n_required,
        " (total N = ", n_required * k, ")",
        " [", n_sim, " simulations]"
      )
    )
  } else if (params$solve_for == "mde") {
    # Binary search for minimum detectable effect
    mde <- find_mde(
      params$n_per_group, pooled_sd, params$power_target,
      n_sim, alpha, params$approach, k
    )

    result <- list(
      value = round(mde, 4),
      type = "mde",
      description = paste0(
        "Minimum detectable effect (Cohen's f): ", round(mde, 4),
        " [", n_sim, " simulations]"
      )
    )
  }

  # Generate power curve via simulation
  power_curve <- generate_simulation_power_curve(
    group_means, pooled_sd, alpha, params$approach, n_sim / 10
  )

  design_table <- generate_design_table(params, result$value)

  list(
    result = result,
    power_curve_df = power_curve,
    design_table_df = design_table,
    effect_f = effect_f,
    params = params
  )
}

#' Simulate power for a given configuration
#'
#' @param group_means Numeric vector of group means
#' @param pooled_sd Pooled standard deviation
#' @param n Sample size per group
#' @param n_sim Number of simulations
#' @param alpha Significance level
#' @param approach "robust" or "nonparametric"
#' @return Estimated power (proportion of significant results)
simulate_power <- function(group_means, pooled_sd, n, n_sim, alpha, approach) {
  k <- length(group_means)

  significant <- replicate(n_sim, {
    # Generate data
    data_list <- lapply(seq_along(group_means), function(i) {
      data.frame(
        group = rep(paste0("G", i), n),
        value = rnorm(n, mean = group_means[i], sd = pooled_sd)
      )
    })
    df <- do.call(rbind, data_list)
    df$group <- factor(df$group)

    # Run appropriate test
    p_value <- tryCatch({
      if (approach == "nonparametric") {
        kruskal.test(value ~ group, data = df)$p.value
      } else {
        # Robust: use Welch's ANOVA as approximation
        oneway.test(value ~ group, data = df, var.equal = FALSE)$p.value
      }
    }, error = function(e) 1)

    p_value < alpha
  })

  mean(significant)
}

#' Binary search for required sample size
find_required_n <- function(group_means, pooled_sd, target_power,
                            n_sim, alpha, approach) {
  n_low <- 2
  n_high <- 500

  while (n_high - n_low > 1) {
    n_mid <- floor((n_low + n_high) / 2)
    power_est <- simulate_power(
      group_means, pooled_sd, n_mid, n_sim, alpha, approach
    )

    if (power_est < target_power) {
      n_low <- n_mid
    } else {
      n_high <- n_mid
    }
  }

  n_high
}

#' Binary search for minimum detectable effect
find_mde <- function(n, pooled_sd, target_power, n_sim, alpha, approach, k) {
  f_low <- 0.01
  f_high <- 2.0

  while (f_high - f_low > 0.01) {
    f_mid <- (f_low + f_high) / 2
    # Create means from effect size
    group_means <- seq(0, f_mid * sqrt(k) * pooled_sd, length.out = k)
    group_means <- group_means - mean(group_means)

    power_est <- simulate_power(
      group_means, pooled_sd, n, n_sim, alpha, approach
    )

    if (power_est < target_power) {
      f_low <- f_mid
    } else {
      f_high <- f_mid
    }
  }

  f_high
}

#' Generate power curve via simulation
generate_simulation_power_curve <- function(group_means, pooled_sd, alpha,
                                            approach, n_sim_per_point) {
  n_range <- seq(5, 100, by = 10)

  power_values <- sapply(n_range, function(n) {
    simulate_power(group_means, pooled_sd, n, n_sim_per_point, alpha, approach)
  })

  data.frame(
    n = n_range,
    power = power_values
  )
}

#' Generate design table showing N per cell
generate_design_table <- function(params, result_value) {
  k <- params$n_groups
  n_ways <- params$n_ways %||% 1

  if (params$solve_for == "sample_size") {
    n_per_cell <- result_value
  } else {
    n_per_cell <- params$n_per_group
  }

  if (n_ways == 1) {
    data.frame(
      Group = paste0("Group ", seq_len(k)),
      N = rep(n_per_cell, k),
      stringsAsFactors = FALSE
    )
  } else {
    # For factorial designs, show total
    data.frame(
      Design = paste0(n_ways, "-way factorial"),
      Groups = k,
      N_per_cell = n_per_cell,
      Total_N = n_per_cell * k,
      stringsAsFactors = FALSE
    )
  }
}
