box::use(
  rhino,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for Statistics
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Validate inputs before statistics computation
#' @param columns Character vector of selected column names
#' @param data Data frame to validate against
#' @return List with $valid (logical) and $error (app_error or NULL)
#' @export
validate_inputs <- function(columns, data) {
  if (is.null(columns) || length(columns) == 0) {
    rhino$log$warn("Statistics: no columns selected")
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = "Please select at least one column.",
        operation_name = "statistics_validate_inputs"
      )
    ))
  }

  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    rhino$log$warn(
      "Statistics: columns not found: {paste(missing, collapse = ', ')}"
    )
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = paste(
          "Columns not found in data:",
          paste(missing, collapse = ", ")
        ),
        operation_name = "statistics_validate_inputs"
      )
    ))
  }

  list(valid = TRUE, error = NULL)
}

#' Run statistical analysis wrapped in safe_execute
#' @param data Data frame
#' @param columns Character vector of column names
#' @return List with $success, $result or $error
#' @export
run_analysis <- function(data, columns) {
  error_handling$safe_execute(
    expr = {
      subset <- data[, columns, drop = FALSE]
      # ... statistics computation will go here ...
      rhino$log$info(
        "Statistics: analysis complete ({length(columns)} columns)"
      )
      subset
    },
    operation_name = "statistics_analysis"
  )
}
