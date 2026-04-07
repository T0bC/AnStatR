box::use(
  rhino,
)

box::use(
  app/logic/shared/data_utils,
  app/logic/plotting/assumption_checks,
  app/logic/preprocessing/normalize,
)

# =============================================================================
# Outlier detection
# =============================================================================

#' Detect outliers in a measurement column, grouped by an interaction factor
#'
#' @param data Data frame
#' @param value_col Character, name of the numeric column to check
#' @param group_col Factor vector (same length as nrow(data)) defining groups
#' @param method Character, one of: "IQR", "zscore", "modified_zscore",
#'   "adjusted_boxplot", "kde", "isolation_forest", "lof", "bootstrap"
#' @param factor Numeric, threshold factor (interpretation depends on method)
#' @param bootstrap_samples Integer, number of bootstrap samples
#' @return Logical vector (same length as nrow(data)), TRUE = outlier
#' @export
detect_outliers <- function(data, value_col, group_col,
                            method = "IQR", factor = 1.5,
                            bootstrap_samples = 1000) {
  valid_methods <- c(
    "IQR", "zscore", "modified_zscore", "adjusted_boxplot",
    "kde", "isolation_forest", "lof", "bootstrap"
  )
  if (!method %in% valid_methods) {
    stop(paste(
      "Invalid method. Choose one of:",
      paste(valid_methods, collapse = ", ")
    ))
  }
  if (!value_col %in% names(data)) {
    stop(paste("Column", value_col, "not found in data"))
  }

  detect_fn <- switch(method,
    IQR              = detect_iqr,
    zscore           = detect_zscore,
    modified_zscore  = detect_modified_zscore,
    adjusted_boxplot = detect_adjusted_boxplot,
    kde              = detect_kde,
    isolation_forest = detect_isolation_forest,
    lof              = detect_lof,
    bootstrap        = detect_bootstrap
  )

  rhino$log$info(
    "Outlier: {method} (factor={factor}) on '{value_col}'"
  )

  result <- rep(FALSE, nrow(data))
  groups <- levels(group_col)
  if (is.null(groups)) groups <- unique(as.character(group_col))

  for (grp in groups) {
    idx <- which(group_col == grp)
    if (length(idx) == 0) next
    x <- data[[value_col]][idx]
    if (method == "bootstrap") {
      result[idx] <- detect_fn(x, factor, bootstrap_samples)
    } else {
      result[idx] <- detect_fn(x, factor)
    }
  }

  n_outliers <- sum(result)
  rhino$log$info(
    "Outlier: {n_outliers}/{nrow(data)} flagged"
  )
  result
}

# =============================================================================
# Trimming
# =============================================================================

#' Mark data points as trimmed based on trim percentage within groups
#'
#' For each group, marks the lowest and highest trim_percent of values.
#'
#' @param values Numeric vector of measurement values
#' @param group_col Factor vector defining groups (same length as values)
#' @param trim_percent Numeric 0-100, percentage trimmed from each end
#' @return Logical vector, TRUE = trimmed
#' @export
mark_trimmed <- function(values, group_col, trim_percent = 0) {
  n <- length(values)
  result <- rep(FALSE, n)
  if (trim_percent <= 0) return(result)

  trim_prop <- min(trim_percent / 100, 0.5)
  groups <- levels(group_col)
  if (is.null(groups)) groups <- unique(as.character(group_col))

  for (grp in groups) {
    idx <- which(group_col == grp)
    ng <- length(idx)
    if (ng == 0) next

    k <- floor(ng * trim_prop)
    if (k == 0) next

    x <- values[idx]
    ord <- order(x)
    trimmed_positions <- c(
      ord[seq_len(k)],
      ord[(ng - k + 1):ng]
    )
    result[idx[trimmed_positions]] <- TRUE
  }

  result
}

# =============================================================================
# Processing pipeline
# =============================================================================

#' Process data: detect outliers and mark trimmed points per measurement
#'
#' For each measurement column, adds `{col}_outlier` and `{col}_trimmed`
#' logical flag columns. Trimming is applied only to non-outlier rows.
#' Optionally runs normalization (mutually exclusive with trimming).
#'
#' @param data Data frame (typically the filtered data)
#' @param measure_cols Character vector of measurement column names
#' @param x_cols Character vector of X-axis column names (for grouping)
#' @param trim_percent Numeric 0-100
#' @param outlier_options List with: enabled (logical), method (character),
#'   factor (numeric), bootstrap_samples (integer)
#' @param normalize_options List with: enabled (logical),
#'   threshold (numeric 0-1, default 0.5)
#' @return Data frame with added flag columns (and optionally
#'   {col}_normalized columns). Attribute "transform_info" attached
#'   when normalization was performed.
#' @export
process_data <- function(data, measure_cols, x_cols,
                         trim_percent = 0,
                         outlier_options = list(enabled = FALSE),
                         normalize_options = list(enabled = FALSE)) {
  if (is.null(measure_cols) || length(measure_cols) == 0) {
    return(data)
  }

  # Build interaction term for grouping
  if (!is.null(x_cols) && length(x_cols) > 0 &&
      all(x_cols %in% names(data))) {
    interaction_term <- data_utils$create_interaction(
      data, x_cols
    )
  } else {
    interaction_term <- factor(rep("all", nrow(data)))
  }

  for (col in measure_cols) {
    if (!col %in% names(data)) next

    outlier_col <- paste0(col, "_outlier")
    trimmed_col <- paste0(col, "_trimmed")
    data[[outlier_col]] <- FALSE
    data[[trimmed_col]] <- FALSE

    # Step 1: Outlier detection (always on raw data)
    if (isTRUE(outlier_options$enabled)) {
      data[[outlier_col]] <- detect_outliers(
        data = data,
        value_col = col,
        group_col = interaction_term,
        method = outlier_options$method %||% "IQR",
        factor = outlier_options$factor %||% 1.5,
        bootstrap_samples = outlier_options$bootstrap_samples
          %||% 1000
      )
    }

    # Step 2: Trimming (only non-outlier rows)
    # Trimming and normalization are mutually exclusive
    if (trim_percent > 0) {
      non_outlier <- which(!data[[outlier_col]])
      if (length(non_outlier) > 0) {
        data[[trimmed_col]][non_outlier] <- mark_trimmed(
          values = data[[col]][non_outlier],
          group_col = interaction_term[non_outlier],
          trim_percent = trim_percent
        )
      }
    }
  }

  # Step 3: Normalization (only when enabled AND no trimming)
  normalize_enabled <- isTRUE(normalize_options$enabled)
  if (normalize_enabled && trim_percent <= 0) {
    norm_threshold <- normalize_options$threshold %||% 0.5

    # Run normality checks per measure column
    normality_results <- list()
    for (col in measure_cols) {
      if (!col %in% names(data)) next
      normality_results[[col]] <- assumption_checks$check_normality(
        data        = data,
        measure_col = col,
        group_col   = interaction_term
      )
    }

    # Normalize columns that fail the threshold
    norm_result <- normalize$normalize_columns(
      data              = data,
      measure_cols      = measure_cols,
      normality_results = normality_results,
      threshold         = norm_threshold
    )
    data <- norm_result$data
    attr(data, "transform_info") <- norm_result$transform_info
    attr(data, "normality_results") <- normality_results

    rhino$log$info(
      "Process: normalization applied to ",
      "{nrow(norm_result$transform_info)} columns"
    )
  }

  data
}

# =============================================================================
# Internal: outlier detection methods
# =============================================================================

detect_iqr <- function(x, fac) {
  result <- rep(FALSE, length(x))
  valid <- is.finite(x)
  if (sum(valid) < 4) return(result)

  q1 <- stats::quantile(x[valid], 0.25)
  q3 <- stats::quantile(x[valid], 0.75)
  iqr <- q3 - q1
  lower <- q1 - fac * iqr
  upper <- q3 + fac * iqr
  result[valid] <- x[valid] < lower | x[valid] > upper
  result
}

detect_zscore <- function(x, fac) {
  result <- rep(FALSE, length(x))
  valid <- is.finite(x)
  if (sum(valid) < 3) return(result)

  z <- (x - mean(x[valid])) / stats::sd(x[valid])
  result[valid] <- abs(z[valid]) > fac
  result
}

detect_modified_zscore <- function(x, fac) {
  result <- rep(FALSE, length(x))
  valid <- is.finite(x)
  if (sum(valid) < 3) return(result)

  med <- stats::median(x[valid])
  # Use raw MAD (constant=1) with 0.6745 scaling per Iglewicz & Hoaglin (1993)
  # Formula: M_i = 0.6745 * (x_i - median) / MAD
  mad_val <- stats::mad(x[valid], constant = 1)
  if (mad_val == 0) return(result)

  mod_z <- 0.6745 * (x - med) / mad_val
  result[valid] <- abs(mod_z[valid]) > fac
  result
}

detect_adjusted_boxplot <- function(x, fac) {
  result <- rep(FALSE, length(x))
  valid <- is.finite(x)
  if (sum(valid) < 4) return(result)

  if (!requireNamespace("robustbase", quietly = TRUE)) {
    warning(paste(
      "Package 'robustbase' needed for adjusted_boxplot.",
      "Falling back to IQR."
    ))
    return(detect_iqr(x, fac))
  }

  xv <- x[valid]
  mc <- robustbase::mc(xv)
  q1 <- stats::quantile(xv, 0.25)
  q3 <- stats::quantile(xv, 0.75)
  iqr <- q3 - q1

  # Hubert & Vandervieren (2008) adjusted boxplot coefficients:
  # MC >= 0: a = -4, b = 3
  # MC < 0:  a = -3, b = 4
  if (mc >= 0) {
    lower <- q1 - fac * exp(-4 * mc) * iqr
    upper <- q3 + fac * exp(3 * mc) * iqr
  } else {
    lower <- q1 - fac * exp(-3 * mc) * iqr
    upper <- q3 + fac * exp(4 * mc) * iqr
  }
  result[valid] <- xv < lower | xv > upper
  result
}

detect_kde <- function(x, fac) {
  result <- rep(FALSE, length(x))
  valid <- is.finite(x)
  if (sum(valid) < 4) return(result)

  xv <- x[valid]
  dens <- stats::density(xv)
  point_dens <- stats::approx(
    dens$x, dens$y, xout = xv
  )$y
  threshold <- stats::quantile(point_dens, fac, na.rm = TRUE)
  result[valid] <- point_dens < threshold
  result
}

detect_isolation_forest <- function(x, fac) {
  result <- rep(FALSE, length(x))
  valid <- is.finite(x)
  if (sum(valid) < 10) return(result)

  if (!requireNamespace("isotree", quietly = TRUE)) {
    warning(paste(
      "Package 'isotree' needed for isolation_forest.",
      "Falling back to IQR."
    ))
    return(detect_iqr(x, 1.5))
  }

  xv <- x[valid]
  iso <- isotree::isolation.forest(
    matrix(xv, ncol = 1), ntrees = 100, nthreads = 1
  )
  scores <- stats::predict(iso, matrix(xv, ncol = 1))
  threshold <- stats::quantile(scores, 1 - fac)
  result[valid] <- scores > threshold
  result
}

detect_lof <- function(x, fac) {
  result <- rep(FALSE, length(x))
  valid <- is.finite(x)
  if (sum(valid) < 10) return(result)

  if (!requireNamespace("dbscan", quietly = TRUE)) {
    warning(paste(
      "Package 'dbscan' needed for LOF.",
      "Falling back to IQR."
    ))
    return(detect_iqr(x, 1.5))
  }

  xv <- x[valid]
  k <- min(5, length(xv) - 1)
  if (k < 1) return(result)

  lof_scores <- dbscan::lof(
    matrix(xv, ncol = 1), minPts = k
  )
  threshold <- stats::quantile(
    lof_scores, 1 - fac, na.rm = TRUE
  )
  result[valid] <- lof_scores > threshold
  result
}

detect_bootstrap <- function(x, fac, n_samples) {
  result <- rep(FALSE, length(x))
  valid <- is.finite(x)
  if (sum(valid) < 4) return(result)

  xv <- x[valid]
  boot_means <- replicate(
    n_samples, mean(sample(xv, replace = TRUE))
  )
  boot_sd <- stats::sd(boot_means)
  if (boot_sd == 0) return(result)

  x_centered <- abs(xv - stats::median(xv))
  result[valid] <- x_centered > (fac * boot_sd)
  result
}
