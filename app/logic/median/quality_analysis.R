# =============================================================================
# Quality column analysis for median calculation
# Column classification (get_descriptive_cols, get_measurement_cols) lives
# in app/logic/column_utils.R for app-wide reuse.
# =============================================================================

#' Analyze a quality column to determine its type and properties
#' @param data Data frame
#' @param col_name Character, name of the quality column
#' @return List with type, and type-specific properties
#' @export
analyze_quality_column <- function(data, col_name) {
  if (is.null(col_name) || col_name == "None") {
    return(list(type = "none"))
  }

  col_data <- data[[col_name]]
  unique_vals <- unique(col_data[!is.na(col_data)])
  n_unique <- length(unique_vals)

  if (is.numeric(col_data)) {
    min_val <- min(col_data, na.rm = TRUE)
    max_val <- max(col_data, na.rm = TRUE)
    all_integers <- all(
      col_data == floor(col_data), na.rm = TRUE
    )

    if (all_integers && n_unique <= 10) {
      return(list(
        type = "categorical",
        unique_values = sort(as.character(unique_vals)),
        n_unique = n_unique,
        hint = paste0(
          "Quality grades detected (", n_unique,
          " levels): ",
          paste(sort(unique_vals), collapse = ", "), "."
        )
      ))
    }

    if (min_val >= 0 && max_val <= 1) {
      return(list(
        type = "percentage_decimal",
        min = min_val,
        max = max_val,
        hint = paste0(
          "Percentage values (",
          round(min_val, 2), " - ", round(max_val, 2),
          ") in decimal format (0-1)."
        )
      ))
    }

    if (min_val >= 0 && max_val <= 100 && n_unique > 10) {
      return(list(
        type = "percentage_100",
        min = min_val,
        max = max_val,
        hint = paste0(
          "Percentage values (",
          round(min_val, 2), " - ", round(max_val, 2),
          ") in 0-100 format."
        )
      ))
    }

    return(list(
      type = "numeric",
      min = min_val,
      max = max_val,
      hint = paste0(
        "Numeric values (",
        round(min_val, 2), " - ", round(max_val, 2),
        "). Set minimum threshold for good quality."
      )
    ))
  }

  list(
    type = "categorical",
    unique_values = sort(as.character(unique_vals)),
    n_unique = n_unique,
    hint = paste0(
      "Categorical values (", n_unique, " levels): ",
      paste(
        head(sort(as.character(unique_vals)), 5),
        collapse = ", "
      ),
      if (n_unique > 5) "..." else ""
    )
  )
}
