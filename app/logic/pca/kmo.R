box::use(
  psych,
  rhino,
  stats,
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

  # psych::KMO may return NaN instead of erroring
  if (is.na(kmo$MSA) || is.nan(kmo$MSA)) {
    issues <- diagnose_kmo_issues(data, kmo$MSAi)
    rhino$log$warn(
      "KMO: overall MSA is NaN/NA",
      " ({ncol(data)} vars, {nrow(data)} obs)"
    )

    hint_parts <- character(0)
    if (length(issues$nan_kmo) > 0) {
      hint_parts <- c(hint_parts, paste(
        "Variables with invalid individual KMO:",
        paste(issues$nan_kmo, collapse = ", ")
      ))
    }
    if (length(issues$constant) > 0) {
      hint_parts <- c(hint_parts, paste(
        "Constant columns (no variance):",
        paste(issues$constant, collapse = ", ")
      ))
    }
    if (length(issues$high_cor) > 0) {
      hint_parts <- c(hint_parts, paste(
        "Highly correlated pairs (|r| > 0.95):",
        paste(issues$high_cor, collapse = "; ")
      ))
    }
    hint <- if (length(hint_parts) > 0) {
      paste(
        "Try removing the following problematic",
        "variables.",
        paste(hint_parts, collapse = ". ")
      )
    } else {
      "Check your variable selection for redundant columns."
    }

    error_context$problematic_variables <- hint

    return(list(
      success = FALSE,
      result = NULL,
      error = error_handling$simple_error(
        message = paste(
          "KMO: Could not compute a valid KMO measure.",
          "The correlation matrix may be singular.",
          hint
        ),
        operation_name = "KMO",
        context = error_context
      )
    ))
  }

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
  if (is.na(kmo) || is.nan(kmo)) return("N/A")
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
  if (is.na(kmo) || is.nan(kmo)) return("bg-secondary")
  if (kmo >= 0.8) "bg-success"
  else if (kmo >= 0.6) "bg-warning text-dark"
  else "bg-danger"
}

# =============================================================================
# Internal helpers (not exported)
# =============================================================================

diagnose_kmo_issues <- function(data, individual_msa = NULL) {
  col_names <- names(data)

  # Variables with NaN/NA individual KMO
  nan_kmo <- character(0)
  if (!is.null(individual_msa)) {
    nan_kmo <- names(individual_msa)[
      is.na(individual_msa) | is.nan(individual_msa)
    ]
  }

  # Detect constant columns (zero variance)
  constant <- col_names[vapply(data, function(x) {
    v <- stats$var(x, na.rm = TRUE)
    is.na(v) || v == 0
  }, logical(1))]

  # Detect highly correlated pairs (|r| > 0.95)
  high_cor <- character(0)
  if (ncol(data) >= 2) {
    cor_mat <- tryCatch(
      stats$cor(data, use = "pairwise.complete.obs"),
      error = function(e) NULL
    )
    if (!is.null(cor_mat)) {
      for (i in seq_len(ncol(cor_mat) - 1)) {
        for (j in (i + 1):ncol(cor_mat)) {
          r <- cor_mat[i, j]
          if (!is.na(r) && abs(r) > 0.95) {
            high_cor <- c(
              high_cor,
              paste0(
                col_names[i], " & ", col_names[j],
                " (r=", sprintf("%.3f", r), ")"
              )
            )
          }
        }
      }
    }
  }

  list(
    nan_kmo = nan_kmo,
    constant = constant,
    high_cor = high_cor
  )
}
