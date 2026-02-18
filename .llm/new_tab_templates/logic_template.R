box::use(
  rhino,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for {TabName}
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Example: validate inputs before computation
#' @param columns Character vector of selected column names
#' @param data Data frame to validate against
#' @return List with $valid (logical) and $error (app_error or NULL)
#' @export
validate_inputs <- function(columns, data) {
  if (is.null(columns) || length(columns) == 0) {
    rhino$log$warn("{TabName}: no columns selected")
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = "Please select at least one column.",
        operation_name = "{tab_name}_validate_inputs"
      )
    ))
  }

  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    rhino$log$warn(
      "{TabName}: columns not found: {paste(missing, collapse = ', ')}"
    )
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = paste(
          "Columns not found in data:",
          paste(missing, collapse = ", ")
        ),
        operation_name = "{tab_name}_validate_inputs"
      )
    ))
  }

  list(valid = TRUE, error = NULL)
}

#' Example: run a computation wrapped in safe_execute
#' @param data Data frame
#' @param columns Character vector of column names
#' @return List with $success, $result or $error
#' @export
run_analysis <- function(data, columns) {
  error_handling$safe_execute(
    expr = {
      subset <- data[, columns, drop = FALSE]
      # ... your computation here ...
      rhino$log$info(
        "{TabName}: analysis complete ({length(columns)} columns)"
      )
      subset
    },
    operation_name = "{tab_name}_analysis"
  )
}
