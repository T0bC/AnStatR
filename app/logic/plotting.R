box::use(
  rhino,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for Plotting
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Validate inputs before computation
#' @param columns Character vector of selected column names
#' @param data Data frame to validate against
#' @return List with $valid (logical) and $error (app_error or NULL)
#' @export
validate_inputs <- function(columns, data) {
  if (is.null(columns) || length(columns) == 0) {
    rhino$log$warn("Plotting: no columns selected")
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = "Please select at least one column.",
        operation_name = "plotting_validate_inputs"
      )
    ))
  }

  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    rhino$log$warn(
      "Plotting: columns not found: {paste(missing, collapse = ', ')}"
    )
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = paste(
          "Columns not found in data:",
          paste(missing, collapse = ", ")
        ),
        operation_name = "plotting_validate_inputs"
      )
    ))
  }

  list(valid = TRUE, error = NULL)
}
