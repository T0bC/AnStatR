box::use(
  rhino,
)

box::use(
  app/logic/error_handling,
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
