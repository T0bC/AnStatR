box::use(
  rhino,
)

box::use(
  app/logic/column_utils,
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for Summary Statistics
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Validate grouping columns exist in data
#' @param grouping_vars Character vector of grouping column names
#' @param data Data frame to validate against
#' @return List with $valid (logical) and $error (app_error or NULL)
#' @export
validate_inputs <- function(grouping_vars, data) {
  if (is.null(grouping_vars) || length(grouping_vars) == 0) {
    rhino$log$warn("Summary: no grouping columns selected")
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = "Please select at least one grouping column.",
        operation_name = "summary_validate_inputs"
      )
    ))
  }

  missing <- setdiff(grouping_vars, names(data))
  if (length(missing) > 0) {
    rhino$log$warn(
      "Summary: columns not found: {paste(missing, collapse = ', ')}"
    )
    return(list(
      valid = FALSE,
      error = error_handling$simple_error(
        message = paste(
          "Columns not found in data:",
          paste(missing, collapse = ", ")
        ),
        operation_name = "summary_validate_inputs"
      )
    ))
  }

  list(valid = TRUE, error = NULL)
}

#' Get filtered values excluding outliers and trimmed
#'
#' Uses {col}_outlier and {col}_trimmed flag columns if present.
#'
#' @param values Numeric vector of measurement values
#' @param data Data frame containing the group subset
#' @param col_name Character, the measurement column name
#' @return Numeric vector of retained (non-NA, non-outlier,
#'   non-trimmed) values
#' @export
get_filtered_values <- function(values, data, col_name) {
  outlier_col <- paste0(col_name, "_outlier")
  trimmed_col <- paste0(col_name, "_trimmed")

  outliers <- if (outlier_col %in% names(data)) {
    data[[outlier_col]]
  } else {
    rep(FALSE, length(values))
  }

  trimmed <- if (trimmed_col %in% names(data)) {
    data[[trimmed_col]]
  } else {
    rep(FALSE, length(values))
  }

  values[!outliers & !trimmed & !is.na(values)]
}

#' Compute summary statistics for one group + one measurement
#'
#' @param filtered Numeric vector of filtered values
#' @return Named list with n, mean, median, var, sd, sem, cv
compute_base_stats <- function(filtered) {
  n <- length(filtered)
  m <- if (n > 0) mean(filtered, na.rm = TRUE) else NA_real_
  s <- if (n > 1) stats::sd(filtered, na.rm = TRUE) else NA_real_

  list(
    n      = n,
    mean   = m,
    median = if (n > 0) {
      stats::median(filtered, na.rm = TRUE)
    } else {
      NA_real_
    },
    var    = if (n > 1) stats::var(filtered, na.rm = TRUE) else NA_real_,
    sd     = s,
    sem    = if (n > 1) s / sqrt(n) else NA_real_,
    cv     = if (n > 1 && !is.na(m) && m != 0) s / m else NA_real_
  )
}

#' Count outliers and trimmed values for one group + one measurement
#'
#' @param data Data frame (group subset)
#' @param col_name Character, measurement column name
#' @return Named list with n_outliers, n_trimmed
count_exclusions <- function(data, col_name) {
  values <- data[[col_name]]
  outlier_col <- paste0(col_name, "_outlier")
  trimmed_col <- paste0(col_name, "_trimmed")

  outliers <- if (outlier_col %in% names(data)) {
    data[[outlier_col]]
  } else {
    rep(FALSE, length(values))
  }

  trimmed <- if (trimmed_col %in% names(data)) {
    data[[trimmed_col]]
  } else {
    rep(FALSE, length(values))
  }

  list(
    n_outliers = sum(outliers & !is.na(values), na.rm = TRUE),
    n_trimmed  = sum(
      trimmed & !outliers & !is.na(values), na.rm = TRUE
    )
  )
}

#' Compute Shapiro-Wilk test for one group + one measurement
#'
#' @param filtered Numeric vector of filtered values
#' @return Named list with shapiro_p, shapiro_W, normal
compute_shapiro <- function(filtered) {
  n <- length(filtered)
  if (n < 3 || n > 5000) {
    return(list(
      shapiro_p = NA_real_,
      shapiro_W = NA_real_,
      normal    = NA_character_
    ))
  }

  # All identical values — test is undefined
  if (length(unique(filtered)) == 1) {
    return(list(
      shapiro_p = NA_real_,
      shapiro_W = NA_real_,
      normal    = "identical values"
    ))
  }

  test <- stats::shapiro.test(filtered)
  list(
    shapiro_p = test$p.value,
    shapiro_W = as.numeric(test$statistic),
    normal    = if (test$p.value > 0.05) "yes" else "no"
  )
}

#' Summarize data with grouped statistics per measurement
#'
#' Computes n, mean, median, var, sd, sem, cv (and optionally
#' Shapiro-Wilk) for each measurement column, grouped by the
#' specified grouping variables. Respects {col}_outlier and
#' {col}_trimmed flag columns.
#'
#' @param data Data frame with measurement and flag columns
#' @param grouping_vars Character vector of grouping column names
#' @param measure_vars Character vector of measurement column names
#' @param shapiro_test Logical, include Shapiro-Wilk test
#' @return Data frame in long format (one row per group per
#'   measurement)
#' @export
summarize_data <- function(data, grouping_vars, measure_vars,
                           shapiro_test = FALSE) {
  # Filter out helper columns
  measure_vars <- measure_vars[
    !grepl("_outlier|_trimmed", measure_vars)
  ]

  # Split data by grouping variables
  if (length(grouping_vars) == 1) {
    split_key <- data[[grouping_vars]]
  } else {
    split_key <- interaction(
      data[, grouping_vars, drop = FALSE],
      drop = TRUE, sep = " | "
    )
  }
  groups <- split(data, split_key)

  # Build one row per group per measurement
  rows <- lapply(names(groups), function(grp_name) {
    grp_data <- groups[[grp_name]]
    # Extract grouping values from first row
    grp_vals <- grp_data[1, grouping_vars, drop = FALSE]

    lapply(measure_vars, function(col) {
      filtered <- get_filtered_values(
        grp_data[[col]], grp_data, col
      )
      base <- compute_base_stats(filtered)
      excl <- count_exclusions(grp_data, col)

      row <- data.frame(
        Measurement = col,
        grp_vals,
        n       = base$n,
        mean    = base$mean,
        median  = base$median,
        var     = base$var,
        sd      = base$sd,
        sem     = base$sem,
        cv      = base$cv,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      if (shapiro_test) {
        shap <- compute_shapiro(filtered)
        row$shapiro_p <- shap$shapiro_p
        row$shapiro_W <- shap$shapiro_W
        row$normal    <- shap$normal
      }

      row$n_outliers <- excl$n_outliers
      row$n_trimmed  <- excl$n_trimmed
      row
    })
  })

  result <- do.call(rbind, unlist(rows, recursive = FALSE))
  rownames(result) <- NULL
  result
}

#' Split summary data into per-measurement list
#'
#' Takes the long-format output of summarize_data() and splits
#' it into a list of list(col, df), one per measurement. Removes
#' the Measurement column (redundant per card), rounds numerics,
#' and drops n_outliers/n_trimmed columns if all zeros.
#'
#' @param summary_df Data frame from summarize_data()
#' @return List of list(col = character, df = data.frame)
#' @export
split_by_measurement <- function(summary_df) {
  measurements <- unique(summary_df$Measurement)

  lapply(measurements, function(m) {
    df <- summary_df[summary_df$Measurement == m, , drop = FALSE]
    df$Measurement <- NULL

    # Round numeric columns
    num_cols <- vapply(df, is.numeric, logical(1))
    df[num_cols] <- lapply(df[num_cols], round, digits = 3)

    # Drop n_outliers if all zeros
    if ("n_outliers" %in% names(df) &&
        all(df$n_outliers == 0, na.rm = TRUE)) {
      df$n_outliers <- NULL
    }

    # Drop n_trimmed if all zeros
    if ("n_trimmed" %in% names(df) &&
        all(df$n_trimmed == 0, na.rm = TRUE)) {
      df$n_trimmed <- NULL
    }

    rownames(df) <- NULL
    list(col = m, df = df)
  })
}

#' Run full summary computation (validate + summarize + split)
#'
#' @param data Data frame
#' @param grouping_vars Character vector of grouping column names
#' @param shapiro_test Logical, include Shapiro-Wilk test
#' @return List with $success, $result (list of list(col, df))
#'   or $error
#' @export
run_summary <- function(data, grouping_vars,
                        shapiro_test = FALSE) {
  # Validate grouping columns
  validation <- validate_inputs(grouping_vars, data)
  if (!validation$valid) {
    return(list(
      success = FALSE, result = NULL,
      error = validation$error
    ))
  }

  # Identify measurement columns
  measure_vars <- column_utils$get_measurement_cols(data)
  measure_vars <- measure_vars[
    !grepl("_outlier|_trimmed", measure_vars)
  ]

  if (length(measure_vars) == 0) {
    return(list(
      success = FALSE, result = NULL,
      error = error_handling$simple_error(
        message = "No measurement columns found in data.",
        operation_name = "Summary Statistics"
      )
    ))
  }

  # Compute
  error_handling$safe_execute(
    expr = {
      summary_df <- summarize_data(
        data, grouping_vars, measure_vars, shapiro_test
      )
      result <- split_by_measurement(summary_df)
      rhino$log$info(
        "Summary: computed stats for {length(measure_vars)}",
        " measurements x {length(unique(summary_df[, grouping_vars[1]]))}",
        " groups"
      )
      result
    },
    operation_name = "Summary Statistics",
    context = list(
      grouping_vars = grouping_vars,
      n_measures = length(measure_vars),
      n_rows = nrow(data)
    )
  )
}
