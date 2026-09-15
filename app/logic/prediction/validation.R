box::use(
  rhino,
)

# =============================================================================
# Validation logic for unknown data against a bundle
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Validate unknown data against a prediction bundle
#'
#' Checks that the unknown data has all required numeric
#' columns, warns on missing metadata columns, and
#' compares value ranges for plausibility.
#'
#' @param unknown_data Data frame of unknown observations
#' @param bundle The prediction bundle (from load_bundle)
#' @return List with $valid (logical), $errors (character
#'   vector), $warnings (character vector)
#' @export
validate_unknown_data <- function(unknown_data, bundle) {
  errors <- character(0)
  warnings <- character(0)
  numeric_cols <- bundle$numeric_cols

  # Check required numeric columns
  missing_numeric <- setdiff(
    numeric_cols, names(unknown_data)
  )
  if (length(missing_numeric) > 0) {
    errors <- c(errors, paste0(
      "Missing required measurement columns: ",
      paste(missing_numeric, collapse = ", ")
    ))
  }

  # Check that present numeric columns are actually numeric
  present_cols <- intersect(
    numeric_cols, names(unknown_data)
  )
  non_numeric <- vapply(
    present_cols,
    function(col) !is.numeric(unknown_data[[col]]),
    logical(1)
  )
  if (any(non_numeric)) {
    bad_cols <- present_cols[non_numeric]
    errors <- c(errors, paste0(
      "Columns must be numeric: ",
      paste(bad_cols, collapse = ", ")
    ))
  }

  # Warn on missing metadata columns (non-blocking)
  if (
    !is.null(bundle$meta_cols) &&
      length(bundle$meta_cols) > 0
  ) {
    missing_meta <- setdiff(
      bundle$meta_cols, names(unknown_data)
    )
    if (length(missing_meta) > 0) {
      warnings <- c(warnings, paste0(
        "Missing metadata columns (non-critical): ",
        paste(missing_meta, collapse = ", ")
      ))
    }
  }

  # Check value ranges for plausibility
  if (
    length(errors) == 0 &&
      !is.null(bundle$raw_data) &&
      length(present_cols) > 0
  ) {
    range_warnings <- check_value_ranges(
      unknown_data, bundle$raw_data, present_cols
    )
    warnings <- c(warnings, range_warnings)
  }

  valid <- length(errors) == 0

  if (valid) {
    rhino$log$info(
      "Prediction validation: passed",
      " ({length(warnings)} warnings)"
    )
  } else {
    rhino$log$warn(
      "Prediction validation: {length(errors)} errors"
    )
  }

  list(
    valid = valid,
    errors = errors,
    warnings = warnings
  )
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Compare value ranges of unknown data against training
#'
#' @param unknown_data Data frame of unknown observations
#' @param raw_data Data frame of raw training data
#' @param cols Character vector of column names to check
#' @return Character vector of warning messages
check_value_ranges <- function(unknown_data, raw_data,
                               cols) {
  warnings <- character(0)

  for (col in cols) {
    if (!col %in% names(raw_data)) next

    train_vals <- raw_data[[col]]
    unknown_vals <- unknown_data[[col]]

    if (
      !is.numeric(train_vals) ||
        !is.numeric(unknown_vals)
    ) {
      next
    }

    train_range <- range(train_vals, na.rm = TRUE)
    unknown_range <- range(unknown_vals, na.rm = TRUE)

    # Flag if unknowns are outside training range
    # with > 20% margin
    train_span <- diff(train_range)
    if (train_span == 0) next

    margin <- train_span * 0.2
    below <- unknown_range[1] <
      (train_range[1] - margin)
    above <- unknown_range[2] >
      (train_range[2] + margin)

    if (below || above) {
      msg <- paste0(
        "'", col, "': unknown range [",
        round(unknown_range[1], 2), ", ",
        round(unknown_range[2], 2),
        "] extends beyond training range [",
        round(train_range[1], 2), ", ",
        round(train_range[2], 2), "]"
      )
      warnings <- c(warnings, msg)
    }
  }

  if (length(warnings) > 0) {
    rhino$log$info(
      "Prediction: {length(warnings)} range warnings"
    )
  }

  warnings
}
