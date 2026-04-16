box::use(
  app/logic/shared/error_handling,
)

#' Validate power analysis input parameters
#'
#' @param params List with: solve_for, alpha, power_target, n_per_group,
#'   effect_size, effect_type, n_groups, group_means, group_sd, distribution
#' @return NULL if valid, or an app_error object
#' @export
validate_power_inputs <- function(params) {
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
    # Raw mode: need group_means and group_sd
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
