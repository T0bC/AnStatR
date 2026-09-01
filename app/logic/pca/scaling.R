box::use(
  rhino,
)

box::use(
  app/logic/shared/error_handling,
)

# =============================================================================
# Pure logic functions for data scaling in PCA
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Scale measurement columns using z-score standardization
#'
#' Applies centering (mean = 0) and scaling (SD = 1) to the
#' selected measurement columns. Non-measurement columns are
#' left untouched. Wrapped in safe_execute for consistent
#' error handling.
#'
#' @param data Data frame (full, including metadata columns)
#' @param measurement_cols Character vector of measurement column names
#' @param center Logical, whether to center (subtract mean). Default TRUE.
#' @param scale Logical, whether to scale (divide by SD). Default TRUE.
#' @return List with $success, $result (scaled data frame) or $error
#' @export
scale_data <- function(data, measurement_cols,
                       center = TRUE, scale = TRUE) {
  error_handling$safe_execute(
    expr = {
      scaled_subset <- base::scale(
        data[, measurement_cols, drop = FALSE],
        center = center,
        scale = scale
      )

      # Check for columns with zero SD (would produce NaN)
      if (scale) {
        scale_vals <- attr(scaled_subset, "scaled:scale")
        zero_sd <- names(scale_vals)[scale_vals == 0]
        if (length(zero_sd) > 0) {
          stop(paste(
            "Cannot scale columns with zero variance:",
            paste(zero_sd, collapse = ", ")
          ))
        }
      }

      # Replace measurement columns with scaled values
      result <- data
      result[, measurement_cols] <- as.data.frame(scaled_subset)

      rhino$log$info(
        "PCA scaling: {length(measurement_cols)} columns",
        " (center={center}, scale={scale})"
      )

      result
    },
    operation_name = "Data Scaling",
    error_parser = scaling_error_parser
  )
}

#' Error parser for scaling-specific errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
scaling_error_parser <- function(error_msg,
                                 operation_name = "Data Scaling") {
  if (grepl("zero variance", error_msg, ignore.case = TRUE)) {
    paste0(
      operation_name,
      ": Some columns have zero variance and cannot",
      " be scaled. Remove constant columns first."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}

# =============================================================================
# residualize_data
# =============================================================================

#' Remove a known confound by group-mean centering
#'
#' For each measurement column, subtracts the mean of that column within
#' each level of `group_col` (e.g. site/location), so downstream analysis
#' no longer sees the average level difference between groups — only the
#' within-group variation. Must be applied before scale_data(), since a
#' global z-score mixes all groups' variance into one SD and makes
#' "subtract this group's mean" meaningless afterward. Does not equalize
#' variance between columns; scale_data() still has a role after this.
#'
#' @param data Data frame (full, including metadata columns)
#' @param measurement_cols Character vector of measurement column names
#' @param group_col Character, name of the categorical metadata column
#'   to residualize by
#' @return List with $success, $result (residualized data frame) or $error
#' @export
residualize_data <- function(data, measurement_cols, group_col) {
  error_handling$safe_execute(
    expr = {
      if (is.null(group_col) || !nzchar(group_col)) {
        stop("No grouping column selected for residualization")
      }
      if (!group_col %in% names(data)) {
        stop(paste0(
          "Grouping column '", group_col, "' not found in data"
        ))
      }
      if (anyNA(data[[group_col]])) {
        stop(paste0(
          "Grouping column '", group_col,
          "' contains missing values; ave() treats each NA",
          " row as its own group, silently zeroing it out —",
          " remove or impute missing values in this column",
          " first"
        ))
      }
      groups <- as.factor(data[[group_col]])
      if (nlevels(groups) < 2) {
        stop(paste0(
          "Grouping column '", group_col,
          "' has fewer than 2 levels; nothing to residualize"
        ))
      }

      subset_df <- data[, measurement_cols, drop = FALSE]
      residualized <- subset_df
      for (col in measurement_cols) {
        group_means <- stats::ave(subset_df[[col]], groups)
        residualized[[col]] <- subset_df[[col]] - group_means
      }

      result <- data
      result[, measurement_cols] <- residualized

      rhino$log$info(
        "Residualize: {length(measurement_cols)} columns",
        " by '{group_col}' ({nlevels(groups)} groups)"
      )

      result
    },
    operation_name = "Residualize by Group",
    error_parser = residualize_error_parser
  )
}

#' Error parser for residualize-specific errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
residualize_error_parser <- function(
    error_msg,
    operation_name = "Residualize by Group") {
  if (grepl("not found", error_msg, ignore.case = TRUE)) {
    paste0(operation_name, ": ", error_msg)
  } else if (grepl(
    "fewer than 2 levels", error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Selected column has only one group — choose a",
      " column with at least two distinct values."
    )
  } else if (grepl(
    "missing values", error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": The grouping column has missing values.",
      " Remove or impute them before residualizing,",
      " or choose a different column."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}
