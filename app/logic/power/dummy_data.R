box::use(
  stats[rnorm, rlnorm, rexp, sd],
)

box::use(

  app/logic/shared/error_handling,
)

# Internal separator for multi-way group names (unlikely to appear in user input)
GROUP_SEP <- ":::"

#' Simulate group data for power analysis visualization
#'
#' @param group_means Named numeric vector of means per group
#' @param group_sd Numeric, pooled standard deviation (or vector per group)
#' @param n_per_group Integer, sample size per group
#' @param distribution Character: "normal", "lognormal", or "exponential"
#' @param factor_structure List of factor definitions (for multi-way designs)
#' @param measure_name Character, name for the measurement column
#' @param seed Optional integer for reproducibility
#' @return A data frame with factor columns and a measurement column
#' @export
simulate_group_data <- function(group_means,
                                group_sd,
                                n_per_group,
                                distribution = "normal",
                                factor_structure = NULL,
                                measure_name = "measure",
                                seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Validate SD

  if (any(group_sd <= 0)) {
    return(error_handling$simple_error(
      message = "Standard deviation must be positive.",
      operation_name = "simulate_group_data"
    ))
  }

  group_names <- names(group_means)
  if (is.null(group_names)) {
    group_names <- paste0("Group_", seq_along(group_means))
  }

  # Expand SD to per-group if single value

  if (length(group_sd) == 1) {
    group_sd <- rep(group_sd, length(group_means))
  }

  # Generate data per group using distribution-aware sampling
  data_list <- lapply(seq_along(group_means), function(i) {
    mu <- group_means[i]
    sigma <- group_sd[i]
    n <- n_per_group

    values <- generate_distribution_samples(mu, sigma, n, distribution)

    data.frame(
      .group = rep(group_names[i], n),
      .value = values,
      stringsAsFactors = FALSE
    )
  })

  df <- do.call(rbind, data_list)

  # Parse factor structure from group names if multi-way

  if (!is.null(factor_structure) && length(factor_structure) > 1) {
    # Build a lookup table: group_name -> list of factor levels
    level_lists <- lapply(factor_structure, function(f) f$levels)
    grid <- expand.grid(level_lists, stringsAsFactors = FALSE)
    # Name the grid columns by factor names
    names(grid) <- sapply(factor_structure, function(f) f$name)
    # Create the combined group names using internal separator
    grid$.group_key <- apply(grid, 1, paste, collapse = GROUP_SEP)

    # Match each row's .group to the grid and extract factor levels
    for (factor_name in names(grid)[names(grid) != ".group_key"]) {
      df[[factor_name]] <- grid[[factor_name]][match(df$.group, grid$.group_key)]
    }
    df$.group <- NULL
  } else if (!is.null(factor_structure) && length(factor_structure) == 1) {
    # Single factor: rename .group to factor name
    names(df)[names(df) == ".group"] <- factor_structure[[1]]$name
  } else {
    names(df)[names(df) == ".group"] <- "group"
  }

  # Rename measurement column
  names(df)[names(df) == ".value"] <- measure_name

  df
}

#' Generate samples from a specified distribution
#'
#' @param mu Mean (observed scale)
#' @param sigma SD (observed scale)
#' @param n Sample size
#' @param distribution Distribution type: "normal", "lognormal", or "exponential"
#' @return Numeric vector of n samples
generate_distribution_samples <- function(mu, sigma, n, distribution) {
  if (distribution == "normal") {
    rnorm(n, mean = mu, sd = sigma)
  } else if (distribution == "lognormal") {
    # Convert observed mean/sd to log-scale parameters
    if (mu <= 0) mu <- 0.01
    log_mu <- log(mu^2 / sqrt(sigma^2 + mu^2))
    log_sigma <- sqrt(log(1 + (sigma^2 / mu^2)))
    rlnorm(n, meanlog = log_mu, sdlog = log_sigma)
  } else if (distribution == "exponential") {
    # Exponential: rate = 1/mean (SD is ignored for exponential)
    rate <- if (mu > 0) 1 / mu else 1
    rexp(n, rate = rate)
  } else {
    # Fallback to normal
    rnorm(n, mean = mu, sd = sigma)
  }
}

#' Extract group statistics from pilot data
#'
#' @param data Data frame with pilot data
#' @param factor_cols Character vector of factor column names
#' @param measure_col Character, measurement column name
#' @return List with group_means (named vector) and pooled_sd
#' @export
extract_pilot_stats <- function(data, factor_cols, measure_col) {
  if (!measure_col %in% names(data)) {
    return(error_handling$simple_error(
      message = paste0("Measurement column '", measure_col, "' not found.
"),
      operation_name = "extract_pilot_stats"
    ))
  }

  # Create interaction term for grouping
  if (length(factor_cols) == 1) {
    data$.interaction <- as.character(data[[factor_cols]])
  } else {
    data$.interaction <- apply(
      data[, factor_cols, drop = FALSE], 1,
      function(x) paste(x, collapse = "_")
    )
  }

  # Calculate per-group means
  group_means <- tapply(
    data[[measure_col]],
    data$.interaction,
    mean, na.rm = TRUE
  )

  # Calculate pooled SD
  group_sds <- tapply(
    data[[measure_col]],
    data$.interaction,
    sd, na.rm = TRUE
  )
  group_ns <- tapply(
    data[[measure_col]],
    data$.interaction,
    function(x) sum(!is.na(x))
  )

  # Pooled SD formula
  pooled_var <- sum((group_ns - 1) * group_sds^2, na.rm = TRUE) /
    sum(group_ns - 1, na.rm = TRUE)
  pooled_sd <- sqrt(pooled_var)

  list(
    group_means = as.numeric(group_means),
    group_names = names(group_means),
    pooled_sd = pooled_sd,
    group_ns = as.numeric(group_ns)
  )
}
