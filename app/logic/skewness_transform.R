box::use(
  rhino,
  stats,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for skewness detection and transformation
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Compute Fisher's skewness for a numeric vector
#'
#' Uses the adjusted Fisher-Pearson formula:
#' g1 = (n / ((n-1)(n-2))) * sum(((x - mean) / sd)^3)
#'
#' @param x Numeric vector (NAs removed upstream)
#' @return Numeric scalar, skewness value
compute_skewness <- function(x) {
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  s <- stats$sd(x)
  if (s == 0) return(NA_real_)
  adjusted <- (n / ((n - 1) * (n - 2))) *
    sum(((x - m) / s)^3)
  adjusted
}

#' Detect skewness in measurement columns
#'
#' Computes per-column skewness and classifies each column
#' as "left" (negative skewness), "right" (positive skewness),
#' or "symmetric" (abs(skewness) <= threshold).
#'
#' @param data Data frame
#' @param measurement_cols Character vector of measurement column names
#' @param threshold Numeric, absolute skewness above which a column
#'   is flagged as highly skewed. Default 2.0 (conservative).
#' @return Data frame with columns: column, skewness, abs_skewness,
#'   direction ("left", "right", "symmetric"), is_skewed (logical).
#'   Sorted by abs_skewness descending.
#' @export
detect_skewness <- function(data, measurement_cols,
                            threshold = 2.0) {
  if (length(measurement_cols) == 0) {
    return(data.frame(
      column = character(0),
      skewness = numeric(0),
      abs_skewness = numeric(0),
      direction = character(0),
      is_skewed = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  skew_vals <- vapply(
    measurement_cols,
    function(col) compute_skewness(data[[col]]),
    numeric(1)
  )

  abs_vals <- abs(skew_vals)
  direction <- ifelse(
    is.na(skew_vals), "symmetric",
    ifelse(
      abs_vals <= threshold, "symmetric",
      ifelse(skew_vals < 0, "left", "right")
    )
  )
  is_skewed <- !is.na(skew_vals) & abs_vals > threshold

  result <- data.frame(
    column = measurement_cols,
    skewness = round(skew_vals, 3),
    abs_skewness = round(abs_vals, 3),
    direction = direction,
    is_skewed = is_skewed,
    stringsAsFactors = FALSE
  )
  result <- result[order(-result$abs_skewness), ]
  rownames(result) <- NULL
  result
}

#' Transform skewed columns using bestNormalize
#'
#' Applies bestNormalize to columns flagged as skewed by
#' detect_skewness(). bestNormalize automatically selects
#' the best transformation (Box-Cox, Yeo-Johnson, log, sqrt,
#' arcsinh, orderNorm, etc.) based on normality tests.
#' Symmetric columns are left untouched.
#'
#' @param data Data frame (full, including metadata columns)
#' @param measurement_cols Character vector of measurement column names
#' @param skew_result Data frame from detect_skewness()
#' @param method Character, kept for API compatibility.
#'   "none" skips transformation, any other value uses
#'   bestNormalize auto-selection.
#' @return List with $success, $result or $error.
#'   $result contains $data, $transformed_cols (data frame),
#'   $transform_params (list of fitted bestNormalize objects),
#'   $skipped_cols (character vector).
#' @export
transform_skewed <- function(data, measurement_cols,
                             skew_result,
                             method = "auto") {
  error_handling$safe_execute(
    expr = {
      skewed <- skew_result[skew_result$is_skewed, ]

      if (nrow(skewed) == 0 || method == "none") {
        rhino$log$info(
          "Skewness: no columns to transform",
          " (method={method})"
        )
        return(list(
          data = data,
          transformed_cols = data.frame(
            column = character(0),
            direction = character(0),
            method_used = character(0),
            skewness_before = numeric(0),
            skewness_after = numeric(0),
            stringsAsFactors = FALSE
          ),
          transform_params = list(),
          skipped_cols = character(0)
        ))
      }

      result_data <- data
      transformed <- list()
      transform_params <- list()
      skipped <- character(0)

      for (i in seq_len(nrow(skewed))) {
        col_name <- skewed$column[i]
        direction <- skewed$direction[i]
        skew_before <- skewed$skewness[i]
        x <- result_data[[col_name]]

        transform_res <- suppressWarnings(fit_bestnormalize_column(x))

        if (is.null(transform_res)) {
          skipped <- c(skipped, col_name)
          rhino$log$warn(
            "Skewness: skipped '{col_name}'",
            " (bestNormalize failed)"
          )
          next
        }

        result_data[[col_name]] <- transform_res$values
        skew_after <- compute_skewness(
          transform_res$values
        )

        transformed[[length(transformed) + 1]] <- list(
          column = col_name,
          direction = direction,
          method_used = transform_res$method_used,
          skewness_before = skew_before,
          skewness_after = round(skew_after, 3)
        )

        # Store fitted bestNormalize object for replay
        transform_params[[length(transform_params) + 1]] <-
          list(
            column = col_name,
            bn_object = transform_res$bn_object,
            method = transform_res$method_used
          )

        rhino$log$info(
          "Skewness: transformed '{col_name}'",
          " ({direction}, {transform_res$method_used},",
          " {skew_before} -> {round(skew_after, 3)})"
        )
      }

      transformed_df <- if (length(transformed) > 0) {
        do.call(rbind, lapply(transformed, as.data.frame,
                              stringsAsFactors = FALSE))
      } else {
        data.frame(
          column = character(0),
          direction = character(0),
          method_used = character(0),
          skewness_before = numeric(0),
          skewness_after = numeric(0),
          stringsAsFactors = FALSE
        )
      }

      n_transformed <- nrow(transformed_df)
      n_skipped <- length(skipped)
      rhino$log$info(
        "Skewness: {n_transformed} columns transformed,",
        " {n_skipped} skipped"
      )

      list(
        data = result_data,
        transformed_cols = transformed_df,
        transform_params = transform_params,
        skipped_cols = skipped
      )
    },
    operation_name = "Skewness Correction",
    error_parser = skewness_error_parser
  )
}

#' Apply a stored transform to a single column
#'
#' Replays the exact transformation on new data using
#' the stored bestNormalize object. Used by the
#' prediction module to apply training transforms to
#' unknown data.
#'
#' @param x Numeric vector (new data column)
#' @param params List with: column, bn_object (fitted
#'   bestNormalize object), method (informational)
#' @return Numeric vector of transformed values
#' @export
apply_stored_transform <- function(x, params) {
  bn_object <- params$bn_object

  if (is.null(bn_object)) {
    stop(paste0(
      "No bestNormalize object stored for column '",
      params$column, "'"
    ))
  }

  # Use predict() on the stored bestNormalize object
  # This applies the exact same transformation learned
  # during training
  tryCatch({
    transformed <- stats::predict(bn_object, newdata = x)
    as.numeric(transformed)
  }, error = function(e) {
    stop(paste0(
      "Failed to apply stored transform for '",
      params$column, "': ", conditionMessage(e)
    ))
  })
}

#' Apply stored transforms to a data frame
#'
#' Applies all stored per-column transform parameters
#' to the corresponding columns in a data frame.
#' Columns not in transform_params are left untouched.
#'
#' @param data Data frame
#' @param transform_params List of per-column param lists,
#'   each with $column and transform parameters
#' @return Data frame with transformed columns
#' @export
apply_stored_transforms <- function(data,
                                    transform_params) {
  if (length(transform_params) == 0) return(data)

  result <- data
  for (params in transform_params) {
    col <- params$column
    if (!col %in% names(result)) {
      warning(paste0(
        "Column '", col,
        "' not found in data; skipping transform"
      ))
      next
    }
    result[[col]] <- apply_stored_transform(
      result[[col]], params
    )
  }
  result
}

#' Error parser for skewness transformation errors
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
skewness_error_parser <- function(
    error_msg,
    operation_name = "Skewness Correction") {
  if (grepl(
    "constant|zero variance",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Some columns have zero variance",
      " and cannot be transformed."
    )
  } else if (grepl(
    "non-numeric|not numeric",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": All measurement columns must be numeric."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}

# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Fit bestNormalize to a single column
#'
#' Uses bestNormalize::bestNormalize() to automatically
#' select and fit the best normalizing transformation.
#' Returns the fitted object for replay on new data.
#'
#' @param x Numeric vector
#' @return List with $values, $method_used, $bn_object,
#'   or NULL on failure
fit_bestnormalize_column <- function(x) {
  # Remove NAs for fitting

  clean_idx <- which(!is.na(x))
  clean_values <- x[clean_idx]

  if (length(clean_values) < 3) {
    return(NULL)
  }

  # Skip constant columns (no variance) - bestNormalize produces NaN warnings
  if (length(unique(clean_values)) == 1) {
    return(NULL)
  }

  tryCatch(
    withCallingHandlers({
      bn_result <- bestNormalize::bestNormalize(clean_values, quiet = TRUE)

      # Get the chosen transformation method name
      method_name <- class(bn_result$chosen_transform)[1]

      # Transform all values (including original positions)
      transformed <- rep(NA_real_, length(x))
      transformed[clean_idx] <- as.numeric(stats::predict(bn_result))

      list(
        values = transformed,
        method_used = method_name,
        bn_object = bn_result
      )
    },
    warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) {
      rhino$log$warn(
        "bestNormalize error: {conditionMessage(e)}"
      )
      NULL
    }
  )
}
