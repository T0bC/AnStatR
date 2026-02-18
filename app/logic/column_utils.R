box::use(
  rhino,
)

# =============================================================================
# Column classification utilities (app-wide)
#
# Naming conventions:
# - Descriptive columns: UPPERCASE + underscores only (e.g. SPECIES, SAMPLE_ID)
# - Measurement columns: mixed case (e.g. Asfc, epLsar, Sq, HAsfc9)
# - Ambiguous columns: uppercase WITH digits (e.g. DATING_MIN_MY is strict,
#   but S10z is measurement because of mixed case)
# =============================================================================

#' Get measurement columns from a data frame
#'
#' Measurement columns are identified by NOT matching the
#' uppercase-only pattern (uppercase letters, digits, underscores).
#' Typically mixed-case numeric columns like Asfc, epLsar, Sq.
#'
#' @param data A data frame
#' @return Character vector of measurement column names
#' @export
get_measurement_cols <- function(data) {
  names(data)[which(
    !grepl("^[A-Z0-9_]+$", names(data))
  )]
}

#' Get descriptive columns from a data frame (strict)
#'
#' Descriptive columns follow the pattern: uppercase letters
#' and underscores only. No digits allowed in column names.
#'
#' @param data A data frame
#' @return Character vector of descriptive column names
#' @export
get_descriptive_cols <- function(data) {
  names(data)[which(
    grepl("^[A-Z_]+$", names(data))
  )]
}

#' Validate column naming conventions
#'
#' Classifies all columns and identifies ambiguous ones
#' (uppercase with digits) that don't fit strictly into
#' either descriptive or measurement.
#'
#' @param data A data frame
#' @return List with valid, descriptive_cols, measurement_cols,
#'   ambiguous_cols, and message
#' @export
validate_column_naming <- function(data) {
  col_names <- names(data)

  descriptive_pattern <- "^[A-Z_]+$"
  uppercase_pattern <- "^[A-Z0-9_]+$"

  is_uppercase <- grepl(uppercase_pattern, col_names)
  is_strict_descriptive <- grepl(
    descriptive_pattern, col_names
  )

  descriptive_cols <- col_names[is_strict_descriptive]
  measurement_cols <- col_names[!is_uppercase]
  ambiguous_cols <- col_names[
    is_uppercase & !is_strict_descriptive
  ]

  has_issues <- length(ambiguous_cols) > 0

  list(
    valid = !has_issues,
    descriptive_cols = descriptive_cols,
    measurement_cols = measurement_cols,
    ambiguous_cols = ambiguous_cols,
    message = if (has_issues) {
      paste0(
        "Some columns have ambiguous naming ",
        "(uppercase with numbers): ",
        paste(ambiguous_cols, collapse = ", "),
        ". These columns will be treated as ",
        "descriptive if they have < 20 unique values."
      )
    } else {
      NULL
    }
  )
}
