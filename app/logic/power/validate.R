box::use(
  app/logic/shared/error_handling,
)

# =============================================================================
# Input Sanitization
# =============================================================================

#' Sanitize a factor or level name for safe use in R operations
#'
#' Removes or replaces problematic characters while preserving readability.
#' Underscores are removed to avoid conflicts with nested axis parsing.
#' @param name Character string to sanitize
#' @return Sanitized character string
#' @export
sanitize_name <- function(name) {

  if (is.null(name) || !is.character(name)) return("")

  sanitized <- trimws(name)

  # Remove all special characters, spaces, dots, and underscores
  # Only keep alphanumeric characters

  sanitized <- gsub("[^[:alnum:]]", "", sanitized)

  # Ensure it doesn't start with a number (prefix with L)
  if (grepl("^[0-9]", sanitized)) {
    sanitized <- paste0("L", sanitized)
  }

  # If empty after sanitization, return placeholder

  if (nchar(sanitized) == 0) {
    sanitized <- "unnamed"
  }

  sanitized
}

#' Sanitize factor structure and report changes
#'
#' @param factors List of factor definitions with name and levels
#' @return List with sanitized factors and any warnings
#' @export
sanitize_factor_structure <- function(factors) {
  if (is.null(factors) || length(factors) == 0) {
    return(list(factors = list(), warnings = character(0)))
  }

  warnings <- character(0)
  sanitized_factors <- lapply(seq_along(factors), function(i) {
    f <- factors[[i]]

    # Sanitize factor name
    original_name <- f$name
    sanitized_name <- sanitize_name(original_name)
    if (sanitized_name != original_name) {
      warnings <<- c(warnings, paste0(
        "Factor '", original_name, "' renamed to '", sanitized_name, "'"
      ))
    }

    # Sanitize levels
    original_levels <- f$levels
    sanitized_levels <- sapply(original_levels, sanitize_name, USE.NAMES = FALSE)

    # Check for duplicates after sanitization
    if (anyDuplicated(sanitized_levels)) {
      # Make unique by appending index
      dups <- duplicated(sanitized_levels)
      for (j in which(dups)) {
        sanitized_levels[j] <- paste0(sanitized_levels[j], "_", j)
      }
      warnings <<- c(warnings, paste0(
        "Duplicate levels in '", sanitized_name, "' were made unique"
      ))
    }

    # Report level changes
    changed_levels <- which(sanitized_levels != original_levels)
    if (length(changed_levels) > 0 && length(changed_levels) <= 3) {
      for (j in changed_levels) {
        warnings <<- c(warnings, paste0(
          "Level '", original_levels[j], "' renamed to '", sanitized_levels[j], "'"
        ))
      }
    } else if (length(changed_levels) > 3) {
      warnings <<- c(warnings, paste0(
        length(changed_levels), " levels in '", sanitized_name, "' were sanitized"
      ))
    }

    list(
      name = sanitized_name,
      levels = sanitized_levels
    )
  })

  # Check for duplicate factor names
  factor_names <- sapply(sanitized_factors, function(f) f$name)
  if (anyDuplicated(factor_names)) {
    for (i in which(duplicated(factor_names))) {
      sanitized_factors[[i]]$name <- paste0(factor_names[i], "_", i)
    }
    warnings <- c(warnings, "Duplicate factor names were made unique")
  }

  list(
    factors = sanitized_factors,
    warnings = warnings
  )
}

#' Check if a name needs sanitization
#'
#' @param name Character string to check
#' @return TRUE if name contains problematic characters
#' @export
needs_sanitization <- function(name) {
  if (is.null(name) || !is.character(name)) return(TRUE)
  sanitize_name(name) != trimws(name)
}

#' Validate power analysis input parameters
#'
#' @param params List with: solve_for, alpha, power_target, n_per_group,
#'   effect_size, effect_type, n_groups, group_means, group_sd, distribution,
#'   input_mode, group_medians, group_iqr
#' @return NULL if valid, or an app_error object
#' @export
validate_power_inputs <- function(params) {
  distribution <- params$distribution %||% "normal"
  input_mode <- params$input_mode %||% "mean_sd"
  approach <- params$approach %||% "parametric"

  # --- Distribution validation ---
  valid_distributions <- c("normal", "lognormal", "exponential")
  if (!distribution %in% valid_distributions) {
    return(error_handling$simple_error(
      message = paste0(
        "Invalid distribution. Must be one of: ",
        paste(valid_distributions, collapse = ", ")
      ),
      operation_name = "power_validate"
    ))
  }

  # --- Standardized mode not allowed for non-normal ---
  if (params$effect_type == "standardized" && distribution != "normal") {
    return(error_handling$simple_error(
      message = paste0(
        "Standardized effect size (Cohen's f) is only valid for normal distributions. ",
        "Please use raw distribution parameters for ", distribution, " data."
      ),
      operation_name = "power_validate"
    ))
  }

  # --- Alpha ---
  if (is.null(params$alpha) || !is.numeric(params$alpha) ||
      params$alpha <= 0 || params$alpha >= 1) {
    return(error_handling$simple_error(
      message = "Alpha must be a number between 0 and 1 (exclusive).",
      operation_name = "power_validate"
    ))
  }

  # --- Power target (required for sample_size and mde modes) ---
  if (params$solve_for %in% c("sample_size", "mde")) {
    if (is.null(params$power_target) || !is.numeric(params$power_target) ||
        params$power_target <= 0 || params$power_target >= 1) {
      return(error_handling$simple_error(
        message = "Target power must be a number between 0 and 1 (exclusive).",
        operation_name = "power_validate"
      ))
    }
  }

  # --- N per group (required for power and mde modes) ---
  if (params$solve_for %in% c("power", "mde")) {
    if (is.null(params$n_per_group) || !is.numeric(params$n_per_group) ||
        params$n_per_group < 2) {
      return(error_handling$simple_error(
        message = "Sample size per group must be at least 2.",
        operation_name = "power_validate"
      ))
    }
  }

  # --- Number of groups ---
  n_groups <- params$n_groups
  if (is.null(n_groups) || !is.numeric(n_groups) || n_groups < 2) {
    return(error_handling$simple_error(
      message = "At least 2 groups are required for power analysis.",
      operation_name = "power_validate"
    ))
  }

  # --- Number of simulations (required when simulation path is used) ---
  requires_simulation <- approach != "parametric" || distribution != "normal"
  if (requires_simulation) {
    if (is.null(params$n_sim) || !is.numeric(params$n_sim) ||
        length(params$n_sim) != 1 || !is.finite(params$n_sim) ||
        is.na(params$n_sim) || params$n_sim < 1) {
      return(error_handling$simple_error(
        message = "Number of simulations must be a finite number greater than or equal to 1.",
        operation_name = "power_validate"
      ))
    }
  }

  # --- Effect size validation ---
  if (params$effect_type == "standardized") {
    if (is.null(params$effect_size) || !is.numeric(params$effect_size) ||
        params$effect_size <= 0) {
      return(error_handling$simple_error(
        message = "Effect size must be a positive number.",
        operation_name = "power_validate"
      ))
    }
  } else if (params$effect_type == "raw") {
    # Raw mode validation depends on input_mode
    if (input_mode == "mean_sd") {
      if (is.null(params$group_means) || length(params$group_means) < 2) {
        return(error_handling$simple_error(
          message = "Group means must be provided for all groups.",
          operation_name = "power_validate"
        ))
      }
      if (is.null(params$group_sd) || !is.numeric(params$group_sd) ||
          any(params$group_sd <= 0)) {
        return(error_handling$simple_error(
          message = "Standard deviation must be positive for all groups.",
          operation_name = "power_validate"
        ))
      }
      # Distribution-specific: lognormal/exponential require positive means
      if (distribution %in% c("lognormal", "exponential")) {
        if (any(params$group_means <= 0)) {
          return(error_handling$simple_error(
            message = paste0(
              distribution, " distribution requires positive mean values."
            ),
            operation_name = "power_validate"
          ))
        }
      }
    } else if (input_mode == "median_iqr") {
      if (is.null(params$group_medians) || length(params$group_medians) < 2) {
        return(error_handling$simple_error(
          message = "Group medians must be provided for all groups.",
          operation_name = "power_validate"
        ))
      }
      if (is.null(params$group_iqr) || !is.numeric(params$group_iqr) ||
          any(params$group_iqr <= 0)) {
        return(error_handling$simple_error(
          message = "IQR must be positive for all groups.",
          operation_name = "power_validate"
        ))
      }
      # Distribution-specific: lognormal/exponential require positive medians
      if (distribution %in% c("lognormal", "exponential")) {
        if (any(params$group_medians <= 0)) {
          return(error_handling$simple_error(
            message = paste0(
              distribution, " distribution requires positive median values."
            ),
            operation_name = "power_validate"
          ))
        }
      }
    }
  }

  NULL
}

#' Validate factor/level design structure
#'
#' @param factors List of factor definitions, each with name and levels
#' @return NULL if valid, or an app_error object
#' @export
validate_design_structure <- function(factors) {
  if (is.null(factors) || length(factors) == 0) {
    return(error_handling$simple_error(
      message = "At least one factor must be defined.",
      operation_name = "power_validate"
    ))
  }

  if (length(factors) > 3) {
    return(error_handling$simple_error(
      message = "Maximum 3-way factorial designs are supported.",
      operation_name = "power_validate"
    ))
  }

  for (i in seq_along(factors)) {
    f <- factors[[i]]
    if (is.null(f$name) || nchar(trimws(f$name)) == 0) {
      return(error_handling$simple_error(
        message = paste0("Factor ", i, " must have a name."),
        operation_name = "power_validate"
      ))
    }
    if (is.null(f$levels) || length(f$levels) < 2) {
      return(error_handling$simple_error(
        message = paste0(
          "Factor '", f$name, "' must have at least 2 levels."
        ),
        operation_name = "power_validate"
      ))
    }
  }

  NULL
}
