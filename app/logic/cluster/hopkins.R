box::use(
  hopkins,
  rhino,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for Hopkins clusterability statistic
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Compute Hopkins statistic for clusterability assessment
#'
#' Validates inputs and computes the Hopkins statistic using the
#' hopkins package. Data should already be cleaned (NAs removed)
#' and scaled as needed by the caller.
#'
#' Guidelines from the hopkins package author:
#' - n should be > 100 and m should be at most 10% of n
#' - High-dimensional data (>10 dims) may have edge effects
#' - Data should be centered and scaled to unit variance
#'
#' @param data Data frame containing the measurement columns
#'   (already cleaned and scaled)
#' @param measurement_cols Character vector of column names
#' @return List with $success, $result or $error.
#'   Result contains $H, $m, $n, $n_dims, $interpretation,
#'   and $warnings (list of warning metadata).
#' @export
compute_hopkins <- function(data, measurement_cols) {
  error_handling$safe_execute(
    expr = {
      validate_hopkins_inputs(data, measurement_cols)

      hopkins_data <- data[, measurement_cols, drop = FALSE]
      n <- nrow(hopkins_data)
      n_dims <- length(measurement_cols)

      # Determine m: 10% of n when n > 100, 5% otherwise
      # with a minimum of 1
      m <- if (n > 100) {
        ceiling(n * 0.1)
      } else {
        max(ceiling(n * 0.05), 1)
      }

      # Compute Hopkins statistic
      h_value <- hopkins$hopkins(hopkins_data, m = m)

      # Build warnings metadata
      warnings <- build_warnings(n, n_dims, m)

      interpretation <- interpret_hopkins(h_value)

      rhino$log$info(
        "Hopkins: H={round(h_value, 4)}, ",
        "m={m}, n={n}, dims={n_dims}, ",
        "interpretation='{interpretation$label}'"
      )

      list(
        H = h_value,
        m = m,
        n = n,
        n_dims = n_dims,
        interpretation = interpretation,
        warnings = warnings
      )
    },
    operation_name = "Hopkins Statistic",
    context = list(
      n_samples = nrow(data),
      n_columns = length(measurement_cols)
    ),
    error_parser = hopkins_error_parser
  )
}

#' Error parser for Hopkins statistic errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
hopkins_error_parser <- function(error_msg,
                                  operation_name = "Hopkins Statistic") {
  if (grepl(
    "numeric|non-numeric",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": All selected columns must be numeric."
    )
  } else if (grepl(
    "\\bNA\\b|missing|NaN",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Data contains missing values. ",
      "Please handle missing data first."
    )
  } else if (grepl(
    "columns|measurement|at least",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": At least 1 measurement column is required."
    )
  } else if (grepl(
    "constant|variance|zero",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Data contains constant columns with zero variance."
    )
  } else if (grepl(
    "observations|rows|sample",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Not enough observations to compute the statistic."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}

# =============================================================================
# Internal helpers (not exported)
# =============================================================================

validate_hopkins_inputs <- function(data, measurement_cols) {
  if (is.null(data) || nrow(data) == 0) {
    stop("Data is NULL or empty")
  }

  if (is.null(measurement_cols) || length(measurement_cols) < 1) {
    stop("At least 1 measurement column is required")
  }

  missing_cols <- setdiff(measurement_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste(
      "Columns not found in data:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  hopkins_data <- data[, measurement_cols, drop = FALSE]

  non_numeric <- names(hopkins_data)[!vapply(
    hopkins_data, is.numeric, logical(1)
  )]
  if (length(non_numeric) > 0) {
    stop(paste(
      "All columns must be numeric. Non-numeric columns:",
      paste(non_numeric, collapse = ", ")
    ))
  }

  if (nrow(data) < 2) {
    stop("Not enough observations to compute Hopkins statistic")
  }

  invisible(TRUE)
}

interpret_hopkins <- function(h_value) {
  if (h_value >= 0.75) {
    list(
      label = "Highly clusterable",
      level = "success",
      description = paste(
        "The data has a strong tendency to cluster.",
        "Cluster analysis is likely to produce",
        "meaningful results."
      )
    )
  } else if (h_value >= 0.5) {
    list(
      label = "Moderately clusterable",
      level = "warning",
      description = paste(
        "The data shows some clustering tendency,",
        "but results should be interpreted with caution.",
        "Consider whether the clusters are meaningful."
      )
    )
  } else {
    list(
      label = "Not clusterable",
      level = "danger",
      description = paste(
        "The data appears to be uniformly distributed",
        "with no meaningful clustering structure.",
        "Cluster analysis may not produce reliable results."
      )
    )
  }
}

build_warnings <- function(n, n_dims, m) {
  warns <- list()

  if (n <= 100) {
    warns$small_n <- paste0(
      "Sample size (n=", n, ") is small (\u2264 100). ",
      "The Hopkins statistic is most reliable with ",
      "n > 100. Interpret with caution."
    )
  }

  if (n_dims > 10) {
    warns$high_dims <- paste0(
      "High dimensionality (", n_dims, " variables). ",
      "Edge effects are more common in higher dimensions, ",
      "which may affect the reliability of the Hopkins ",
      "statistic."
    )
  }

  warns
}
