box::use(
  dplyr,
  rhino,
  stats,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Shared infrastructure for omnibus statistical tests (robust + parametric).
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Safely convert columns to factors with validation
#'
#' Validates data before factor conversion to prevent errors from invalid data.
#'
#' @param data Data frame containing the columns to convert
#' @param vars Character vector of column names to convert to factors
#' @return List with $success (logical), $data (modified df), $error (app_error or NULL)
#' @export
safe_factor_conversion <- function(data, vars) {
  for (v in vars) {
    if (!v %in% names(data)) {
      return(list(
        success = FALSE,
        data = data,
        error = error_handling$simple_error(
          message = paste0("Column '", v, "' not found in data."),
          operation_name = "factor_conversion",
          context = list(column = v, available = names(data))
        )
      ))
    }

    col_data <- data[[v]]

    if (all(is.na(col_data))) {
      return(list(
        success = FALSE,
        data = data,
        error = error_handling$simple_error(
          message = paste0("Column '", v, "' contains only NA values."),
          operation_name = "factor_conversion",
          context = list(column = v)
        )
      ))
    }

    non_na_data <- col_data[!is.na(col_data)]
    if (is.character(non_na_data) && all(trimws(non_na_data) == "")) {
      return(list(
        success = FALSE,
        data = data,
        error = error_handling$simple_error(
          message = paste0(
            "Column '", v, "' contains only empty strings."
          ),
          operation_name = "factor_conversion",
          context = list(column = v)
        )
      ))
    }

    unique_vals <- unique(non_na_data)
    if (length(unique_vals) < 2) {
      return(list(
        success = FALSE,
        data = data,
        error = error_handling$simple_error(
          message = paste0(
            "Column '", v, "' has fewer than 2 unique values",
            " (found ", length(unique_vals), ")."
          ),
          operation_name = "factor_conversion",
          context = list(column = v, n_unique = length(unique_vals))
        )
      ))
    }

    data[[v]] <- as.factor(data[[v]])
  }

  list(success = TRUE, data = data, error = NULL)
}


#' Calculate smallest group size across all factor combinations
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of grouping column(s)
#' @return Integer, smallest group size
#' @export
calculate_smallest_group <- function(df, x_axis) {
  group_counts <- dplyr$summarise(
    dplyr$group_by(df, dplyr$across(dplyr$all_of(x_axis))),
    n = dplyr$n(),
    .groups = "drop"
  )
  min(group_counts$n)
}


#' Setup bootstrap parameters
#'
#' @param df Data frame
#' @param x_axis Character vector of grouping columns
#' @param use_bootstrap Logical
#' @param boot_samples Integer, number of bootstrap samples
#' @param boot_sample_size Integer or NULL, sample size per group
#' @return List with $n_iterations and $sample_size
#' @export
setup_bootstrap_params <- function(df, x_axis,
                                   use_bootstrap,
                                   boot_samples,
                                   boot_sample_size) {
  if (use_bootstrap) {
    smallest_group <- calculate_smallest_group(df, x_axis)
    sample_size <- if (
      !is.null(boot_sample_size) && !is.na(boot_sample_size)
    ) {
      min(boot_sample_size, smallest_group)
    } else {
      smallest_group
    }
    list(n_iterations = boot_samples, sample_size = sample_size)
  } else {
    list(n_iterations = 1, sample_size = NULL)
  }
}


#' Sample data for a single bootstrap iteration
#'
#' @param df Data frame
#' @param x_axis Character vector of grouping columns
#' @param use_bootstrap Logical
#' @param sample_size Integer, sample size per group
#' @return Data frame (sampled or original)
#' @export
sample_for_iteration <- function(df, x_axis,
                                 use_bootstrap,
                                 sample_size) {
  if (use_bootstrap) {
    dplyr$ungroup(
      dplyr$slice_sample(
        dplyr$group_by(
          df,
          dplyr$across(dplyr$all_of(x_axis))
        ),
        n = sample_size,
        replace = TRUE
      )
    )
  } else {
    df
  }
}


#' Format bootstrap results with confidence intervals
#'
#' @param boot_results Data frame with bootstrap iterations as rows
#' @param digits Integer, number of significant digits (default 3)
#' @return Data frame with mean [CI lower - CI upper] format
#' @export
format_bootstrap_results <- function(boot_results, digits = 3) {
  if (is.null(boot_results) || nrow(boot_results) == 0) {
    return(error_handling$simple_error(
      message = "No bootstrap results available.",
      operation_name = "format_bootstrap"
    ))
  }

  all_na_cols <- vapply(
    boot_results,
    function(x) all(is.na(x)),
    logical(1)
  )
  if (all(all_na_cols)) {
    return(error_handling$simple_error(
      message = "All bootstrap iterations returned NA values.",
      operation_name = "format_bootstrap"
    ))
  }

  ci_bounds <- apply(boot_results, 2, function(x) {
    valid_x <- x[!is.na(x)]
    if (length(valid_x) < 2) {
      return(c(NA_real_, NA_real_))
    }
    stats$quantile(valid_x, c(0.025, 0.975), na.rm = TRUE)
  })

  formatted <- lapply(names(boot_results), function(col) {
    col_data <- boot_results[[col]]
    valid_data <- col_data[!is.na(col_data)]

    if (length(valid_data) == 0) {
      return("NA [NA - NA]")
    }

    mean_val <- mean(valid_data, na.rm = TRUE)
    lower <- ci_bounds[1, col]
    upper <- ci_bounds[2, col]

    paste0(
      signif(mean_val, digits), " [",
      signif(lower, digits), " - ",
      signif(upper, digits), "]"
    )
  })
  names(formatted) <- names(boot_results)
  as.data.frame(formatted, stringsAsFactors = FALSE)
}


#' Generic omnibus test runner
#'
#' Executes any omnibus test (robust or parametric) using a config object
#' that defines test-specific hooks for validation, formula building,
#' test execution, result extraction, and formatting.
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of grouping column(s)
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, trim proportion (0-0.5)
#' @param use_bootstrap Logical, whether to use bootstrap
#' @param boot_samples Integer, number of bootstrap samples
#' @param boot_sample_size Integer or NULL, bootstrap sample size per group
#' @param config List with test configuration hooks:
#'   $name, $result_cols, $validate, $build_context,
#'   $build_formula, $run_test, $extract_results, $format_results
#' @return Data frame with results, or structured app_error
#' @export
run_omnibus_test <- function(df, x_axis, measure_col,
                             tr_value, use_bootstrap,
                             boot_samples, boot_sample_size,
                             config) {
  # 1. Validate inputs
  validation <- config$validate(df, x_axis)
  if (error_handling$is_app_error(validation)) {
    return(validation)
  }

  # 2. Setup bootstrap parameters
  boot_params <- setup_bootstrap_params(
    df, x_axis, use_bootstrap, boot_samples, boot_sample_size
  )

  # 3. Build error context
  error_context <- config$build_context(
    df, x_axis, measure_col, tr_value, use_bootstrap
  )

  # 4. Run the test iterations inside safe_execute
  test_result <- error_handling$safe_execute(
    expr = {
      results_matrix <- as.data.frame(
        matrix(
          NA_real_,
          nrow = boot_params$n_iterations,
          ncol = length(config$result_cols)
        )
      )
      names(results_matrix) <- config$result_cols

      for (i in seq_len(boot_params$n_iterations)) {
        sample_data <- sample_for_iteration(
          df, x_axis, use_bootstrap, boot_params$sample_size
        )
        formula_obj <- config$build_formula(measure_col, x_axis)
        test_out <- config$run_test(
          formula_obj, sample_data, tr_value
        )
        results_matrix[i, ] <- config$extract_results(test_out)
      }

      results_matrix
    },
    operation_name = config$name,
    context = error_context,
    error_parser = error_handling$stat_error_parser
  )

  # 5. Handle errors
  if (!test_result$success) {
    return(test_result$error)
  }

  rhino$log$info(
    "{config$name}: test completed successfully",
    " (bootstrap={use_bootstrap},",
    " iterations={boot_params$n_iterations})"
  )

  # 6. Format results
  config$format_results(
    test_result$result, x_axis, use_bootstrap
  )
}
