#' Statistical Test Utility Functions
#'
#' Shared helpers and dispatcher for statistical tests.
#' Individual test implementations are in R/utils/statistics/:
#' - tests_robust_anova.R: t1way, t2way, t3way (Welch-Yuen family)
#' - tests_parametric_anova.R: Classical ANOVA, Tukey HSD, Cohen's d
#' - tests_contrasts.R: lincon (linear contrasts)
#' - tests_effect_size.R: Cliff's Delta
#' - tests_combined_results.R: Result formatting


# Source test implementation files
source("R/utils/statistics/tests_robust_anova.R", local = TRUE)
source("R/utils/statistics/tests_parametric_anova.R", local = TRUE)
source("R/utils/statistics/tests_contrasts.R", local = TRUE)
source("R/utils/statistics/tests_effect_size.R", local = TRUE)
source("R/utils/statistics/tests_combined_results.R", local = TRUE)


#' Create a structured error object for statistical tests
#'
#' Wrapper around create_app_error() specialized for statistical tests.
#' Uses stat_error_parser for user-friendly messages.
#'
#' @param user_msg Character, user-friendly error message
#' @param raw_msg Character, original error message from R
#' @param error_obj The error condition object
#' @param test_name Character, name of the test that failed
#' @param context List, optional context with function arguments for debugging
#' @return List with is_error=TRUE and structured error information
create_stat_error <- function(user_msg, raw_msg, error_obj, test_name, context = NULL) {
    # Delegate to global error handling with test_name as operation_name
    create_app_error(
        user_msg = user_msg,
        raw_msg = raw_msg,
        error_obj = error_obj,
        operation_name = test_name,
        context = context
    )
}


#' Safe execution wrapper for statistical tests
#'
#' Wrapper around safe_execute() specialized for statistical tests.
#' Uses stat_error_parser for user-friendly error messages.
#'
#' @param expr Expression to evaluate
#' @param test_name Character, name of the test for error messages
#' @param context List, optional context with function arguments for debugging
#' @return List with success flag and either result or structured error
safe_stat_test <- function(expr, test_name = "test", context = NULL) {
    # Delegate to global safe_execute with stat-specific error parser
    safe_execute(
        expr = expr,
        operation_name = test_name,
        context = context,
        error_parser = stat_error_parser
    )
}


#' Calculate smallest group size
#'
#' Finds the minimum sample size across all group combinations.
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of grouping column(s)
#' @return Integer, smallest group size
calculate_smallest_group <- function(df, x_axis) {
    group_counts <- df %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(x_axis))) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop")
    
    min(group_counts$n)
}


#' Format bootstrap results with confidence intervals
#'
#' @param boot_results Data frame with bootstrap iterations (rows) and statistics (columns)
#' @param digits Integer, number of significant digits
#' @return Data frame with mean [CI lower - CI upper] format
format_bootstrap_results <- function(boot_results, digits = 3) {
    ci_bounds <- apply(boot_results, 2, function(x) {
        stats::quantile(x, c(0.025, 0.975), na.rm = TRUE)
    })
    
    formatted <- lapply(names(boot_results), function(col) {
        mean_val <- mean(boot_results[[col]], na.rm = TRUE)
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


#' Compute Statistics for a Single Measurement
#'
#' Main entry point for computing all statistics for one measurement column.
#' Determines design type (one-way, two-way, three-way) and runs appropriate tests.
#'
#' @param df Data frame containing the filtered data (outliers/trimmed excluded)
#' @param x_axis Character vector of X-axis columns (determines design type)
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, trim proportion (0-0.5)
#' @param params List of statistics parameters from sidebar
#' @param level_discrepancy Character vector or NULL, level consistency issues
#' @return List with all results for this measurement
compute_measurement_statistics <- function(df, x_axis, measure_col, tr_value, params,
                                           level_discrepancy = NULL) {
    
    n_groups <- length(x_axis)
    errors <- list()
    
    # Determine test approach (default to robust if not specified)
    test_approach <- params$test_approach %||% "robust"
    
    # Determine design type and header based on test approach
    if (test_approach == "robust") {
        design_info <- switch(
            as.character(n_groups),
            "1" = list(
                type = "one-way",
                header = "Robust One-Way Trimmed Means Comparisons [ANOVA] - Heteroscedastic Welch-Yuen",
                test_fn = perform_t1way
            ),
            "2" = list(
                type = "two-way", 
                header = "Robust Two-Way Trimmed Means Comparisons [ANOVA] - Heteroscedastic Welch-Yuen",
                test_fn = perform_t2way
            ),
            "3" = list(
                type = "three-way",
                header = "Robust Three-Way Trimmed Means Comparisons [ANOVA] - Heteroscedastic Welch-Yuen",
                test_fn = perform_t3way
            ),
            list(
                type = "error",
                header = "Selection Error",
                test_fn = NULL
            )
        )
    } else if (test_approach == "parametric") {
        design_info <- switch(
            as.character(n_groups),
            "1" = list(
                type = "one-way",
                header = "Classical One-Way ANOVA - Parametric Test",
                test_fn = perform_parametric_anova
            ),
            "2" = list(
                type = "two-way", 
                header = "Classical Two-Way ANOVA - Parametric Test",
                test_fn = perform_parametric_anova
            ),
            "3" = list(
                type = "three-way",
                header = "Classical Three-Way ANOVA - Parametric Test",
                test_fn = perform_parametric_anova
            ),
            list(
                type = "error",
                header = "Selection Error",
                test_fn = NULL
            )
        )
    } else {
        design_info <- list(
            type = "error",
            header = "Selection Error",
            test_fn = NULL
        )
    }
    
    # Validate group counts for multi-way designs
    if (n_groups >= 2) {
        n_levels_first <- length(unique(df[[x_axis[1]]]))
        if (n_levels_first < 2) {
            return(list(
                measure = measure_col,
                header = design_info$header,
                result_t_way = NULL,
                result_lincon = NULL,
                result_cliff = NULL,
                result_combined = NULL,
                errors = list(
                    design_error = sprintf(
                        "A %s design requires at least 2 groups in '%s', but only %d found. Adjust X-axis selection or filtering.",
                        design_info$type, x_axis[1], n_levels_first
                    )
                )
            ))
        }
    }
    
    # Handle level discrepancy for multi-way designs (Welch-Yuen not robust)
    result_t_way <- NULL
    if (n_groups > 1 && !is.null(level_discrepancy)) {
        result_t_way <- data.frame(
            Error = c(
                "Incomplete design! It needs to be full factorial (unequal group sizes detected).",
                "--- Detailed Information ---",
                level_discrepancy
            ),
            stringsAsFactors = FALSE
        )
    } else if (!is.null(design_info$test_fn)) {
        result_t_way <- design_info$test_fn(
            df = df,
            x_axis = x_axis,
            measure_col = measure_col,
            tr_value = tr_value,
            use_bootstrap = params$use_bootstrap,
            boot_samples = params$boot_samples,
            boot_sample_size = params$boot_sample_size,
            p_adjust_method = params$p_val_cor_method
        )
    }
    
    # Post-hoc tests (based on test approach)
    if (test_approach == "robust") {
        # Linear contrasts for robust tests
        result_lincon <- perform_lincon(
            df = df,
            x_axis = x_axis,
            measure_col = measure_col,
            tr_value = tr_value,
            use_bootstrap = params$use_bootstrap,
            boot_samples = params$boot_samples,
            boot_sample_size = params$boot_sample_size,
            p_adjust_method = params$p_val_cor_method
        )
    } else {
        # Tukey HSD for parametric tests
        result_lincon <- perform_tukey_hsd(
            df = df,
            x_axis = x_axis,
            measure_col = measure_col,
            tr_value = tr_value,
            use_bootstrap = params$use_bootstrap,
            boot_samples = params$boot_samples,
            boot_sample_size = params$boot_sample_size,
            p_adjust_method = params$p_val_cor_method,
            use_scientific = params$use_scientific_notation
        )
    }
    
    # Effect size (based on test approach)
    if (test_approach == "robust") {
        # Cliff's Delta for robust tests
        result_cliff <- perform_cliff(
            df = df,
            x_axis = x_axis,
            measure_col = measure_col,
            tr_value = tr_value,
            use_bootstrap = params$use_bootstrap,
            boot_samples = params$boot_samples,
            boot_sample_size = params$boot_sample_size,
            p_adjust_method = params$p_val_cor_method
        )
    } else {
        # Cohen's d for parametric tests
        result_cliff <- perform_cohens_d(
            df = df,
            x_axis = x_axis,
            measure_col = measure_col,
            tr_value = tr_value,
            use_bootstrap = params$use_bootstrap,
            boot_samples = params$boot_samples,
            boot_sample_size = params$boot_sample_size,
            p_adjust_method = params$p_val_cor_method
        )
    }
    
    # Combined results table
    if (test_approach == "robust") {
        # Determine available columns in lincon results
        available_lincon_cols <- names(result_lincon)
        
        # Choose the best available p-value column
        p_col <- if ("p.adjusted" %in% available_lincon_cols) {
            "p.adjusted"
        } else if ("Lincon: p.value" %in% available_lincon_cols) {
            "Lincon: p.value"
        } else {
            NULL
        }
        
        # Choose the best available psihat column
        psihat_col <- if ("Lincon: psihat" %in% available_lincon_cols) {
            "Lincon: psihat"
        } else {
            NULL
        }
        
        # Build df1ColNames based on available columns
        df1ColNames <- c("Interaction")
        if (!is.null(psihat_col)) df1ColNames <- c(df1ColNames, psihat_col)
        if (!is.null(p_col)) df1ColNames <- c(df1ColNames, p_col)
        
        # Use generic combined results function for robust tests
        result_combined <- create_combined_results(
            df1 = result_lincon,  # lincon results
            df2 = result_cliff,   # cliff results
            df1ColNames = df1ColNames,
            df2ColNames = c("Interaction", "Cliff: psihat", "Cliff: p.value"),
            merge_key = "Interaction",
            x_axis = x_axis,
            filter_valid = params$valid_comparisons,
            p_adjust_method = params$p_val_cor_method,
            use_scientific = params$use_scientific_notation
        )
    } else {
        # Use generic combined results function for parametric tests
        result_combined <- create_combined_results(
            df1 = result_lincon,  # Tukey HSD results
            df2 = result_cliff,   # Cohen's d results
            df1ColNames = c("Interaction", "Difference", "P_Value_Raw"),
            df2ColNames = c("Interaction", "Cohen's d", "p.value"),
            merge_key = "Interaction",
            x_axis = x_axis,
            filter_valid = params$valid_comparisons,
            p_adjust_method = params$p_val_cor_method,
            use_scientific = params$use_scientific_notation
        )
    }
    
    list(
        measure = measure_col,
        header = design_info$header,
        design_type = design_info$type,
        result_t_way = result_t_way,
        result_lincon = result_lincon,
        result_cliff = result_cliff,
        result_combined = result_combined,
        errors = errors
    )
}
