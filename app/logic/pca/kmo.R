box::use(
  psych,
  rhino,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for KMO measure
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Calculate KMO measure
#'
#' Wraps psych::KMO in safe_execute with a domain-specific error parser.
#' Returns list with overall MSA and individual variable MSAs on success,
#' or a structured error on failure.
#'
#' @param data Data frame with numeric columns (already prepared)
#' @return List with $success, $result (list with $overall, $individual)
#'   or $error (structured error object)
#' @export
calculate_kmo <- function(data) {
  error_context <- list(
    n_variables = ncol(data),
    n_observations = nrow(data),
    variables = paste(names(data), collapse = ", ")
  )

  result <- error_handling$safe_execute(
    expr = psych$KMO(data),
    operation_name = "KMO",
    context = error_context,
    error_parser = kmo_error_parser
  )

  if (!result$success) {
    rhino$log$warn(
      "KMO: computation failed",
      " ({ncol(data)} vars, {nrow(data)} obs)"
    )
    return(result)
  }

  kmo <- result$result
  rhino$log$info(
    "KMO: overall MSA = {round(kmo$MSA, 3)}",
    " ({ncol(data)} variables)"
  )

  list(
    success = TRUE,
    result = list(
      overall = kmo$MSA,
      individual = kmo$MSAi
    ),
    error = NULL
  )
}

#' Error parser for KMO-specific errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
kmo_error_parser <- function(error_msg,
                             operation_name = "KMO") {
  if (grepl(
    "singular|invertible",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Correlation matrix is singular.",
      " Remove highly correlated or constant variables."
    )
  } else if (grepl(
    "\\bNA\\b|missing|NaN",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Data contains missing values.",
      " Please handle missing data first."
    )
  } else if (grepl("numeric", error_msg, ignore.case = TRUE)) {
    paste0(
      operation_name,
      ": All selected columns must be numeric."
    )
  } else {
    paste0(operation_name, " calculation failed: ", error_msg)
  }
}

#' Get interpretation text for a KMO value
#'
#' Uses Kaiser's classification scale.
#'
#' @param kmo Numeric KMO value
#' @return Character interpretation label
#' @export
kmo_interpretation <- function(kmo) {
  if (kmo >= 0.9) "Marvelous"
  else if (kmo >= 0.8) "Meritorious"
  else if (kmo >= 0.7) "Middling"
  else if (kmo >= 0.6) "Mediocre"
  else if (kmo >= 0.5) "Miserable"
  else "Unacceptable"
}

#' Get Bootstrap badge CSS class for a KMO value
#'
#' @param kmo Numeric KMO value
#' @return Character CSS class string
#' @export
kmo_badge_class <- function(kmo) {
  if (kmo >= 0.8) "bg-success"
  else if (kmo >= 0.6) "bg-warning text-dark"
  else "bg-danger"
}
