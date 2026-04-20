box::use(
  pwr,
  stats[aov, kruskal.test, lm, oneway.test, pf, qnorm, rnorm, rlnorm, rexp, var],
)

box::use(
  app/logic/shared/error_handling,
  app/logic/power/validate,
)

# Constants
CURVE_SIM_DIVISOR <- 10
MAX_SEARCH_N <- 500
MDE_SEARCH_RANGE <- c(0.01, 2.0)

emit_progress <- function(progress_cb, value, detail = NULL) {
  if (is.null(progress_cb)) {
    return(invisible(NULL))
  }

  safe_value <- max(0, min(1, value))
  try(progress_cb(safe_value, detail), silent = TRUE)
  invisible(NULL)
}

make_scaled_progress <- function(progress_cb, start, end, default_detail = NULL) {
  if (is.null(progress_cb)) {
    return(NULL)
  }

  force(start)
  force(end)
  force(default_detail)

  function(value, detail = NULL) {
    local_value <- if (is.null(value)) 0 else value
    scaled <- start + (end - start) * max(0, min(1, local_value))
    local_detail <- if (is.null(detail)) default_detail else detail
    emit_progress(progress_cb, scaled, local_detail)
  }
}

simulate_power_with_step_progress <- function(
    dist_params,
    n,
    n_sim,
    alpha,
    approach,
    progress_cb,
    step_idx,
    max_steps,
    step_label,
    progress_detail) {
  step_start <- min((step_idx - 1) / max_steps, 1)
  step_end <- min(step_idx / max_steps, 1)
  step_progress <- make_scaled_progress(progress_cb, step_start, step_end, step_label)

  simulate_power(
    dist_params,
    n,
    n_sim,
    alpha,
    approach,
    progress_cb = step_progress,
    progress_detail = progress_detail
  )
}

make_sim_runner <- function(dist_params, n_sim, alpha, approach,
                            progress_cb, max_steps) {
  force(dist_params)
  force(n_sim)
  force(alpha)
  force(approach)
  force(progress_cb)
  force(max_steps)

  function(n, step_idx, step_label, progress_detail) {
    simulate_power_with_step_progress(
      dist_params = dist_params,
      n = n,
      n_sim = n_sim,
      alpha = alpha,
      approach = approach,
      progress_cb = progress_cb,
      step_idx = step_idx,
      max_steps = max_steps,
      step_label = step_label,
      progress_detail = progress_detail
    )
  }
}

build_effect_size_dist_params <- function(effect_size_f, pooled_sd, k, distribution) {
  group_means <- seq(0, effect_size_f * sqrt(k) * pooled_sd, length.out = k)
  group_means <- group_means - mean(group_means)
  group_sds <- rep(pooled_sd, k)

  build_dist_params(group_means, group_sds, distribution)
}

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
#'   - distribution: "normal", "lognormal", or "exponential"
#'   - input_mode: "mean_sd" or "median_iqr"
#'   - group_means: numeric vector (raw mode, mean_sd)
#'   - group_medians: numeric vector (raw mode, median_iqr)
#'   - group_sd: pooled SD or per-group SDs (raw mode, mean_sd)
#'   - group_iqr: per-group IQRs (raw mode, median_iqr)
#'   - n_groups: number of groups (1-way) or total cells (factorial)
#'   - n_ways: 1, 2, or 3 (factorial design complexity)
#'   - approach: "parametric", "robust", or "nonparametric"
#'   - n_sim: number of simulations (for non-parametric/robust)
#' @param progress_cb Optional callback function(value, detail) for progress updates
#' @return List with: result, power_curve_df, design_table_df, messages
#' @export
perform_power_analysis <- function(params, progress_cb = NULL) {

  # Validate inputs
  validation_error <- validate$validate_power_inputs(params)
  if (!is.null(validation_error)) {
    return(validation_error)
  }


  distribution <- params$distribution %||% "normal"
  input_mode <- params$input_mode %||% "mean_sd"
  messages <- character(0)

  # Auto-switch to simulation for non-normal distributions
  use_simulation <- params$approach != "parametric"
  if (distribution != "normal" && params$approach == "parametric") {
    use_simulation <- TRUE
    messages <- c(messages, paste0(
      "Non-normal distribution (", distribution, ") selected: ",
      "automatically using Monte Carlo simulation instead of parametric formula."
    ))
  }

  # Info message for exponential + median_iqr (IQR is ignored)
  if (distribution == "exponential" && input_mode == "median_iqr") {
    messages <- c(messages,
      "For exponential distribution, IQR values are not used. SD equals mean by definition."
    )
  }

  # Normalize raw inputs to simulation-ready distribution parameters
  dist_params <- normalize_distribution_params(params)
  if (error_handling$is_app_error(dist_params)) {
    return(dist_params)
  }

  # Convert to Cohen's f for reporting (based on means)
  effect_f <- if (params$effect_type == "raw") {
    raw_to_cohens_f(dist_params$group_means, dist_params$pooled_sd)
  } else {
    params$effect_size
  }

  # Route to appropriate method
  if (!use_simulation && distribution == "normal") {
    result <- parametric_power(params, effect_f)
  } else {
    result <- simulation_power(params, effect_f, dist_params, progress_cb = progress_cb)
  }

  # Attach messages to result (merge with any from simulation_power)
  if (!error_handling$is_app_error(result)) {
    all_messages <- c(messages, result$messages)
    if (length(all_messages) > 0) {
      result$messages <- all_messages
    }
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
    normalized <- normalize_distribution_params(params)
    if (error_handling$is_app_error(normalized)) {
      return(data.frame(
        n = n_range,
        power = rep(NA_real_, length(n_range))
      ))
    }
    raw_to_cohens_f(normalized$group_means, normalized$pooled_sd)
  } else {
    params$effect_size
  }

  if (!is.finite(effect_f) || length(effect_f) != 1 || effect_f <= 0) {
    return(data.frame(
      n = n_range,
      power = rep(NA_real_, length(n_range))
    ))
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
# Private helpers: Distribution parameter normalization
# =============================================================================

#' Normalize user inputs to simulation-ready distribution parameters
#'
#' Converts mean+SD or median+IQR inputs into consistent parameters
#' for each distribution type.
#'
#' @param params Power analysis parameters
#' @return List with group_means, group_sds, pooled_sd, distribution, dist_params
normalize_distribution_params <- function(params) {
  distribution <- params$distribution %||% "normal"
  input_mode <- params$input_mode %||% "mean_sd"
  k <- params$n_groups

  # For standardized effect mode, construct synthetic parameters
  if (params$effect_type == "standardized") {
    pooled_sd <- 1
    f <- params$effect_size
    group_means <- seq(0, f * sqrt(k) * pooled_sd, length.out = k)
    group_means <- group_means - mean(group_means)
    group_sds <- rep(pooled_sd, k)

    return(list(
      group_means = group_means,
      group_sds = group_sds,
      pooled_sd = pooled_sd,
      distribution = distribution,
      dist_params = lapply(seq_len(k), function(i) {
        list(type = "normal", mean = group_means[i], sd = pooled_sd)
      })
    ))
  }

  # Raw mode: convert based on input_mode and distribution
  if (input_mode == "mean_sd") {
    group_means <- params$group_means
    group_sds <- if (length(params$group_sd) == 1) {
      rep(params$group_sd, k)
    } else {
      params$group_sd
    }
  } else {
    # median_iqr mode: convert to mean/sd based on distribution
    group_medians <- params$group_medians
    group_iqrs <- if (length(params$group_iqr) == 1) {
      rep(params$group_iqr, k)
    } else {
      params$group_iqr
    }

    converted <- convert_median_iqr_to_mean_sd(
      group_medians, group_iqrs, distribution
    )
    if (error_handling$is_app_error(converted)) {
      return(converted)
    }
    group_means <- converted$means
    group_sds <- converted$sds
  }

  pooled_sd <- if (length(group_sds) > 1) {
    sqrt(mean(group_sds^2))
  } else {
    group_sds[1]
  }

  # Build distribution-specific parameters for simulation

  dist_params <- build_dist_params(group_means, group_sds, distribution)

  list(
    group_means = group_means,
    group_sds = group_sds,
    pooled_sd = pooled_sd,
    distribution = distribution,
    dist_params = dist_params
  )
}

#' Convert median + IQR to mean + SD for each distribution
#'
#' @param medians Numeric vector of group medians
#' @param iqrs Numeric vector of group IQRs
#' @param distribution Distribution type
#' @return List with means and sds vectors
convert_median_iqr_to_mean_sd <- function(medians, iqrs, distribution) {
  k <- length(medians)

  if (distribution == "normal") {
    # For normal: median = mean, IQR = 2 * qnorm(0.75) * sd ≈ 1.349 * sd
    sds <- iqrs / (2 * qnorm(0.75))
    return(list(means = medians, sds = sds))
  }

  if (distribution == "lognormal") {
    # For lognormal: median = exp(mu), IQR = exp(mu) * (exp(sigma*z) - exp(-sigma*z))
    # where z = qnorm(0.75)
    z <- qnorm(0.75)
    means <- numeric(k)
    sds <- numeric(k)

    for (i in seq_len(k)) {
      med <- medians[i]
      iqr <- iqrs[i]

      if (med <= 0) {
        return(error_handling$simple_error(

          message = "Log-normal distribution requires positive median values.",
          operation_name = "convert_median_iqr"
        ))
      }

      # Solve for sigma: IQR/median = exp(sigma*z) - exp(-sigma*z) = 2*sinh(sigma*z)
      ratio <- iqr / med
      # sigma = asinh(ratio/2) / z
      sigma <- asinh(ratio / 2) / z
      mu <- log(med)

      # Convert to observed-scale mean and sd
      means[i] <- exp(mu + sigma^2 / 2)
      sds[i] <- sqrt((exp(sigma^2) - 1) * exp(2 * mu + sigma^2))
    }
    return(list(means = means, sds = sds))
  }

  if (distribution == "exponential") {
    # For exponential: median = ln(2)/lambda, mean = 1/lambda
    # So mean = median / ln(2)
    # SD = mean for exponential
    means <- medians / log(2)
    sds <- means
    return(list(means = means, sds = sds))
  }

  # Fallback to normal assumption
  sds <- iqrs / (2 * qnorm(0.75))
  list(means = medians, sds = sds)
}

#' Build distribution-specific parameters for simulation
#'
#' @param means Group means (observed scale)
#' @param sds Group SDs (observed scale)
#' @param distribution Distribution type
#' @return List of per-group distribution parameters
build_dist_params <- function(means, sds, distribution) {
  k <- length(means)

  lapply(seq_len(k), function(i) {
    mu <- means[i]
    sigma <- sds[i]

    if (distribution == "normal") {
      list(type = "normal", mean = mu, sd = sigma)
    } else if (distribution == "lognormal") {
      # Convert observed mean/sd to log-scale parameters
      if (mu <= 0) mu <- 0.01
      log_mu <- log(mu^2 / sqrt(sigma^2 + mu^2))
      log_sigma <- sqrt(log(1 + (sigma^2 / mu^2)))
      list(type = "lognormal", meanlog = log_mu, sdlog = log_sigma)
    } else if (distribution == "exponential") {
      # Exponential: rate = 1/mean
      rate <- if (mu > 0) 1 / mu else 1
      list(type = "exponential", rate = rate)
    } else {
      list(type = "normal", mean = mu, sd = sigma)
    }
  })
}

#' Convert raw group means and SD to Cohen's f
#'
#' @param group_means Numeric vector of group means
#' @param pooled_sd Pooled standard deviation
#' @return Cohen's f effect size
raw_to_cohens_f <- function(group_means, pooled_sd) {
  grand_mean <- mean(group_means)
  ss_between <- sum((group_means - grand_mean)^2)
  k <- length(group_means)

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
      res <- tryCatch({
        pwr$pwr.anova.test(
          k = k,
          f = effect_f,
          sig.level = params$alpha,
          power = params$power_target
        )
      }, error = function(e) {
        # Fallback for very large effects where required n is below 2
        power_at_n2 <- tryCatch({
          pwr$pwr.anova.test(
            k = k,
            n = 2,
            f = effect_f,
            sig.level = params$alpha
          )$power
        }, error = function(e2) NA_real_)

        if (is.finite(power_at_n2) && power_at_n2 >= params$power_target) {
          return(list(n = 2))
        }

        stop(e)
      })

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
#' @param dist_params Normalized distribution parameters from normalize_distribution_params
#' @param progress_cb Optional callback function(value, detail) for progress updates
#' @return List with result, power_curve_df, design_table_df
simulation_power <- function(params, effect_f, dist_params, progress_cb = NULL) {
  # Validate solve_for parameter

  valid_solve_for <- c("power", "sample_size", "mde")
  if (!params$solve_for %in% valid_solve_for) {
    return(error_handling$simple_error(
      message = paste0(
        "Invalid solve_for value: '", params$solve_for, "'. ",
        "Must be one of: ", paste(valid_solve_for, collapse = ", ")
      ),
      operation_name = "simulation_power"
    ))
  }

  n_sim <- params$n_sim %||% 1000
  k <- params$n_groups
  alpha <- params$alpha
  approach <- params$approach %||% "parametric"
  distribution <- dist_params$distribution
  messages <- character(0)

  if (!is.numeric(n_sim) || length(n_sim) != 1 ||
      is.na(n_sim) || !is.finite(n_sim) || n_sim < 1) {
    return(error_handling$simple_error(
      message = "Number of simulations must be a finite number greater than or equal to 1.",
      operation_name = "simulation_power",
      context = list(n_sim = n_sim)
    ))
  }

  n_sim <- as.integer(n_sim)
  n_sim <- max(1L, n_sim)

  result_progress <- make_scaled_progress(progress_cb, 0, 0.8)
  curve_progress <- make_scaled_progress(progress_cb, 0.8, 1.0)

  emit_progress(progress_cb, 0, "Preparing simulation...")

  if (params$solve_for == "power") {
    n <- params$n_per_group
    power_est <- simulate_power(
      dist_params$dist_params, n, n_sim, alpha, approach,
      progress_cb = result_progress,
      progress_detail = "Estimating power"
    )

    result <- list(
      value = round(power_est, 4),
      type = "power",
      description = paste0(
        "Estimated power (", n_sim, " simulations, ", distribution, "): ",
        round(power_est * 100, 1), "%"
      )
    )
  } else if (params$solve_for == "sample_size") {
    search_result <- find_required_n(
      dist_params$dist_params, params$power_target,
      n_sim, alpha, approach,
      progress_cb = result_progress
    )
    n_required <- search_result$n

    if (!is.null(search_result$warning)) {
      messages <- c(messages, search_result$warning)
    }

    result <- list(
      value = n_required,
      type = "sample_size",
      description = paste0(
        "Required sample size per group: ", n_required,
        " (total N = ", n_required * k, ")",
        " [", n_sim, " simulations, ", distribution, "]"
      )
    )
  } else if (params$solve_for == "mde") {
    mde_result <- find_mde(
      params$n_per_group, dist_params$pooled_sd, params$power_target,
      n_sim, alpha, approach, k, distribution,
      progress_cb = result_progress
    )
    mde <- mde_result$f

    if (!is.null(mde_result$warning)) {
      messages <- c(messages, mde_result$warning)
    }

    result <- list(
      value = round(mde, 4),
      type = "mde",
      description = paste0(
        "Minimum detectable effect (Cohen's f): ", round(mde, 4),
        " [", n_sim, " simulations, ", distribution, "]"
      )
    )
  }

  # Generate power curve via simulation
  power_curve <- generate_simulation_power_curve(
    dist_params$dist_params, alpha, approach, max(100, n_sim / CURVE_SIM_DIVISOR),
    progress_cb = curve_progress
  )

  emit_progress(progress_cb, 1, "Finalizing results...")

  design_table <- generate_design_table(params, result$value)

  output <- list(
    result = result,
    power_curve_df = power_curve,
    design_table_df = design_table,
    effect_f = effect_f,
    params = params
  )

  if (length(messages) > 0) {
    output$messages <- messages
  }

  output
}

#' Generate random samples from distribution parameters
#'
#' @param dist_param Single group's distribution parameters
#' @param n Sample size
#' @return Numeric vector of n samples
generate_samples <- function(dist_param, n) {
  if (dist_param$type == "normal") {
    rnorm(n, mean = dist_param$mean, sd = dist_param$sd)
  } else if (dist_param$type == "lognormal") {
    rlnorm(n, meanlog = dist_param$meanlog, sdlog = dist_param$sdlog)
  } else if (dist_param$type == "exponential") {
    rexp(n, rate = dist_param$rate)
  } else {
    rnorm(n, mean = dist_param$mean %||% 0, sd = dist_param$sd %||% 1)
  }
}

#' Simulate power for a given configuration
#'
#' @param dist_params List of per-group distribution parameters
#' @param n Sample size per group
#' @param n_sim Number of simulations
#' @param alpha Significance level
#' @param approach "parametric", "robust", or "nonparametric"
#' @param progress_cb Optional callback function(value, detail) for progress updates
#' @param progress_detail Optional detail text prefix for progress updates
#' @return Estimated power (proportion of significant results)
simulate_power <- function(dist_params, n, n_sim, alpha, approach,
                           progress_cb = NULL, progress_detail = NULL) {
  k <- length(dist_params)
  if (!is.numeric(n_sim) || length(n_sim) != 1 ||
      is.na(n_sim) || !is.finite(n_sim)) {
    n_sim <- 1L
  } else {
    n_sim <- as.integer(n_sim)
  }
  n_sim <- max(1L, n_sim)
  update_every <- max(1, floor(n_sim / 50))

  significant <- logical(n_sim)

  emit_progress(progress_cb, 0, progress_detail)

  for (i in seq_len(n_sim)) {
    data_list <- lapply(seq_len(k), function(i) {
      data.frame(
        group = rep(paste0("G", i), n),
        value = generate_samples(dist_params[[i]], n)
      )
    })
    df <- do.call(rbind, data_list)
    df$group <- factor(df$group)

    p_value <- tryCatch({
      if (approach == "nonparametric") {
        kruskal.test(value ~ group, data = df)$p.value
      } else if (approach == "robust") {
        oneway.test(value ~ group, data = df, var.equal = FALSE)$p.value
      } else {
        # Parametric: standard ANOVA F-test
        summary(aov(value ~ group, data = df))[[1]][["Pr(>F)"]][1]
      }
    }, error = function(e) 1)

    significant[i] <- p_value < alpha

    if (!is.null(progress_cb) && (i %% update_every == 0 || i == n_sim)) {
      detail <- if (!is.null(progress_detail)) {
        paste0(progress_detail, " (", i, "/", n_sim, ")")
      } else {
        paste0("Running simulation ", i, "/", n_sim)
      }
      emit_progress(progress_cb, i / n_sim, detail)
    }
  }

  mean(significant)
}

#' Binary search for required sample size
#' @return List with n (sample size) and optional warning message
find_required_n <- function(dist_params, target_power, n_sim, alpha, approach,
                            progress_cb = NULL) {
  n_low <- 2
  n_high <- MAX_SEARCH_N
  max_steps <- ceiling(log2(MAX_SEARCH_N - n_low)) + 1

  run_sim <- make_sim_runner(dist_params, n_sim, alpha, approach,
                             progress_cb, max_steps)
  step_idx <- 0

  while (n_high - n_low > 1) {
    step_idx <- step_idx + 1
    n_mid <- floor((n_low + n_high) / 2)
    power_est <- run_sim(
      n = n_mid,
      step_idx = step_idx,
      step_label = paste0("Searching sample size (step ", step_idx, "/", max_steps, ")"),
      progress_detail = paste0("Searching sample size (n=", n_mid, ")")
    )

    if (power_est < target_power) {
      n_low <- n_mid
    } else {
      n_high <- n_mid
    }
  }

  # Check if target power is achievable at max N
  warning_msg <- NULL
  if (n_high == MAX_SEARCH_N) {
    step_idx <- step_idx + 1
    power_at_max <- run_sim(
      n = MAX_SEARCH_N,
      step_idx = step_idx,
      step_label = paste0("Checking upper bound n=", MAX_SEARCH_N),
      progress_detail = paste0("Checking upper bound n=", MAX_SEARCH_N)
    )
    if (power_at_max < target_power) {
      warning_msg <- paste0(
        "Target power (", round(target_power * 100), "%) may not be achievable ",
        "within n \u2264 ", MAX_SEARCH_N, " per group. ",
        "Power at n=", MAX_SEARCH_N, ": ", round(power_at_max * 100, 1), "%"
      )
    }
  }

  list(n = n_high, warning = warning_msg)
}

#' Binary search for minimum detectable effect
#' @return List with f (effect size) and optional warning message
find_mde <- function(n, pooled_sd, target_power, n_sim, alpha, approach, k,
                     distribution = "normal", progress_cb = NULL) {
  f_low <- MDE_SEARCH_RANGE[1]
  f_high <- MDE_SEARCH_RANGE[2]
  max_steps <- ceiling(log2((MDE_SEARCH_RANGE[2] - MDE_SEARCH_RANGE[1]) / 0.01)) + 1

  step_idx <- 0
  while (f_high - f_low > 0.01) {
    step_idx <- step_idx + 1
    f_mid <- (f_low + f_high) / 2
    dist_params <- build_effect_size_dist_params(f_mid, pooled_sd, k, distribution)

    run_sim <- make_sim_runner(dist_params, n_sim, alpha, approach,
                               progress_cb, max_steps)
    power_est <- run_sim(
      n = n,
      step_idx = step_idx,
      step_label = paste0("Searching minimum detectable effect (step ", step_idx, "/", max_steps, ")"),
      progress_detail = paste0("Searching minimum detectable effect (f=", round(f_mid, 3), ")")
    )

    if (power_est < target_power) {
      f_low <- f_mid
    } else {
      f_high <- f_mid
    }
  }

  # Check if target power is achievable at max effect size
  warning_msg <- NULL
  if (abs(f_high - MDE_SEARCH_RANGE[2]) < 0.02) {
    step_idx <- step_idx + 1
    dist_params <- build_effect_size_dist_params(f_high, pooled_sd, k, distribution)
    run_sim <- make_sim_runner(dist_params, n_sim, alpha, approach,
                               progress_cb, max_steps)
    power_at_max <- run_sim(
      n = n,
      step_idx = step_idx,
      step_label = "Checking upper effect-size bound",
      progress_detail = "Checking upper effect-size bound"
    )

    if (power_at_max < target_power) {
      warning_msg <- paste0(
        "Target power (", round(target_power * 100), "%) may not be achievable ",
        "even with large effect sizes (f \u2264 ", MDE_SEARCH_RANGE[2], "). ",
        "Consider increasing sample size."
      )
    }
  }

  list(f = f_high, warning = warning_msg)
}

#' Generate power curve via simulation
generate_simulation_power_curve <- function(dist_params, alpha, approach,
                                            n_sim_per_point,
                                            progress_cb = NULL) {
  n_range <- seq(5, 100, by = 10)
  n_points <- length(n_range)

  power_values <- vapply(seq_along(n_range), function(idx) {
    n <- n_range[idx]
    point_start <- (idx - 1) / n_points
    point_end <- idx / n_points
    point_progress <- make_scaled_progress(
      progress_cb,
      point_start,
      point_end,
      paste0("Generating power curve (", idx, "/", n_points, ")")
    )

    simulate_power(
      dist_params, n, n_sim_per_point, alpha, approach,
      progress_cb = point_progress,
      progress_detail = paste0("Generating power curve at n=", n)
    )
  }, numeric(1))

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
