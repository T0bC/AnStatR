#' Statistical Test Utility Functions
#'
#' Contains functions for performing robust statistical tests:
#' - One-way, two-way, three-way Welch-Yuen ANOVA
#' - Linear contrasts (lincon)
#' - Cliff's Delta effect size
#' - Combined results formatting


#' Create a structured error object for statistical tests
#'
#' Returns a standardized error structure with message, raw error,
#' and a filtered stack trace showing only application code.
#'
#' @param user_msg Character, user-friendly error message
#' @param raw_msg Character, original error message from R
#' @param error_obj The error condition object
#' @param test_name Character, name of the test that failed
#' @return List with is_error=TRUE and structured error information
create_stat_error <- function(user_msg, raw_msg, error_obj, test_name) {
    # Capture stack trace filtered to app code only (frames with file references)
    stack_trace <- tryCatch(
        {
            raw_output <- utils::capture.output(shiny::printStackTrace(error_obj), type = "message")
            if (length(raw_output) > 0) {
                # Filter to only lines containing file references [path#line]
                # These are the frames from our application code
                filtered <- raw_output[grepl("\\[.*#[0-9]+\\]", raw_output)]
                if (length(filtered) > 0) {
                    paste(cli::ansi_html(filtered), collapse = "\n")
                } else {
                    # Fallback: show top frames if no file refs found
                    paste(cli::ansi_html(head(raw_output, 20)), collapse = "\n")
                }
            } else {
                NULL
            }
        },
        error = function(e) NULL
    )
    
    list(
        is_error = TRUE,
        test_name = test_name,
        message = user_msg,
        raw_message = raw_msg,
        traces = list(
            stack_trace = stack_trace
        )
    )
}


#' Safe execution wrapper for statistical tests
#'
#' Wraps a statistical test in error handling, returning a standardized
#' result structure with either results or a structured error object.
#'
#' @param expr Expression to evaluate
#' @param test_name Character, name of the test for error messages
#' @return List with success flag and either result or structured error
safe_stat_test <- function(expr, test_name = "test") {
    # Capture the expression for evaluation with stack trace capture
    expr_quoted <- substitute(expr)
    
    tryCatch(
        {
            # Use captureStackTraces to enable stack trace capture on errors
            result <- shiny::captureStackTraces(eval(expr_quoted, envir = parent.frame()))
            list(success = TRUE, result = result, error = NULL)
        },
        error = function(e) {
            # Parse common error messages into user-friendly versions
            error_msg <- conditionMessage(e)
            
            user_msg <- if (grepl("groups", error_msg, ignore.case = TRUE)) {
                paste0(test_name, ": Insufficient groups for comparison. Need at least 2 groups with data.")
            } else if (grepl("sample size|observations", error_msg, ignore.case = TRUE)) {
                paste0(test_name, ": Insufficient sample size in one or more groups.")
            } else if (grepl("NA|missing", error_msg, ignore.case = TRUE)) {
                paste0(test_name, ": Too many missing values in the data.")
            } else if (grepl("variance|constant", error_msg, ignore.case = TRUE)) {
                paste0(test_name, ": Cannot compute - one or more groups have zero variance.")
            } else {
                paste0(test_name, " failed: ", error_msg)
            }
            
            # Create structured error with multiple trace formats
            error_struct <- create_stat_error(user_msg, error_msg, e, test_name)
            
            list(success = FALSE, result = NULL, error = error_struct)
        }
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


#' Perform One-Way Robust ANOVA (Welch-Yuen t1way)
#'
#' @param df Data frame containing the data (already filtered for outliers/trimmed)
#' @param x_axis Character, single grouping column name
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, trim proportion (0-0.5)
#' @param use_bootstrap Logical, whether to use bootstrap
#' @param boot_samples Integer, number of bootstrap samples
#' @param boot_sample_size Integer or NULL, bootstrap sample size per group
#' @param p_adjust_method Character, p-value adjustment method
#' @return Data frame with test results or error data frame
perform_t1way <- function(df, x_axis, measure_col, tr_value,
                          use_bootstrap = FALSE, boot_samples = 599,
                          boot_sample_size = NULL, p_adjust_method = "bonferroni") {
    
    # Validate inputs
    if (length(x_axis) != 1) {
        return(data.frame(Error = "t1way requires exactly one grouping variable.", 
                          stringsAsFactors = FALSE))
    }
    
    group_col <- x_axis[1]
    n_groups <- length(unique(df[[group_col]]))
    
    if (n_groups < 2) {
        return(data.frame(Error = paste0("t1way requires at least 2 groups, found ", n_groups, "."),
                          stringsAsFactors = FALSE))
    }
    
    # Determine sample size for bootstrap
    if (use_bootstrap) {
        smallest_group <- calculate_smallest_group(df, x_axis)
        sample_size <- if (!is.null(boot_sample_size) && !is.na(boot_sample_size)) {
            min(boot_sample_size, smallest_group)
        } else {
            smallest_group
        }
        n_iterations <- boot_samples
    } else {
        n_iterations <- 1
        sample_size <- NULL
    }
    
    # Run the test (with or without bootstrap)
    test_result <- safe_stat_test({
        # Storage for bootstrap iterations
        results_matrix <- data.frame(
            F_statistic = numeric(n_iterations),
            df1 = numeric(n_iterations),
            df2 = numeric(n_iterations),
            Effect_Size = numeric(n_iterations),
            p_value = numeric(n_iterations)
        )
        
        for (i in seq_len(n_iterations)) {
            # Sample data if bootstrapping
            if (use_bootstrap) {
                sample_data <- df %>%
                    dplyr::group_by(dplyr::across(dplyr::all_of(x_axis))) %>%
                    dplyr::slice_sample(n = sample_size, replace = TRUE) %>%
                    dplyr::ungroup()
            } else {
                sample_data <- df
            }
            
            # Build formula dynamically
            formula_obj <- stats::as.formula(paste0("`", measure_col, "` ~ `", group_col, "`"))
            
            # Perform t1way test
            t1way_out <- WRS2::t1way(
                formula = formula_obj,
                data = sample_data,
                tr = tr_value
            )
            
            results_matrix[i, ] <- c(
                t1way_out$test,
                t1way_out$df1,
                t1way_out$df2,
                t1way_out$effsize,
                t1way_out$p.value
            )
        }
        
        results_matrix
    }, test_name = "t1way")
    
    # Handle errors - return structured error object
    if (!test_result$success) {
        return(test_result$error)
    }
    
    # Format results
    if (use_bootstrap) {
        format_bootstrap_results(test_result$result)
    } else {
        result_df <- test_result$result
        result_df[] <- lapply(result_df, function(x) signif(x, 3))
        result_df
    }
}


#' Perform Two-Way Robust ANOVA (Welch-Yuen t2way)
#'
#' Returns main effects (A, B) and interaction (AB) with Q statistics and p-values.
#'
#' @inheritParams perform_t1way
#' @return Data frame with test results or error data frame
perform_t2way <- function(df, x_axis, measure_col, tr_value,
                          use_bootstrap = FALSE, boot_samples = 599,
                          boot_sample_size = NULL, p_adjust_method = "bonferroni") {
    
    # Validate inputs - must have exactly 2 grouping columns
    if (length(x_axis) != 2) {
        return(data.frame(Error = "t2way requires exactly two grouping variables.",
                          stringsAsFactors = FALSE))
    }
    
    factor1 <- x_axis[1]
    factor2 <- x_axis[2]
    
    # Check each factor has at least 2 levels
    n_levels_1 <- length(unique(df[[factor1]]))
    n_levels_2 <- length(unique(df[[factor2]]))
    
    if (n_levels_1 < 2) {
        return(data.frame(Error = paste0("t2way requires at least 2 levels in '", factor1, 
                                         "', found ", n_levels_1, "."),
                          stringsAsFactors = FALSE))
    }
    if (n_levels_2 < 2) {
        return(data.frame(Error = paste0("t2way requires at least 2 levels in '", factor2, 
                                         "', found ", n_levels_2, "."),
                          stringsAsFactors = FALSE))
    }
    
    # Determine sample size for bootstrap
    if (use_bootstrap) {
        smallest_group <- calculate_smallest_group(df, x_axis)
        sample_size <- if (!is.null(boot_sample_size) && !is.na(boot_sample_size)) {
            min(boot_sample_size, smallest_group)
        } else {
            smallest_group
        }
        n_iterations <- boot_samples
    } else {
        n_iterations <- 1
        sample_size <- NULL
    }
    
    # Run the test (with or without bootstrap)
    test_result <- safe_stat_test({
        # Storage for bootstrap iterations
        # Columns: Qa, Qb, Qab, A.p.value, B.p.value, AB.p.value
        results_matrix <- data.frame(
            Qa = numeric(n_iterations),
            Qb = numeric(n_iterations),
            Qab = numeric(n_iterations),
            A.p.value = numeric(n_iterations),
            B.p.value = numeric(n_iterations),
            AB.p.value = numeric(n_iterations)
        )
        
        for (i in seq_len(n_iterations)) {
            # Sample data if bootstrapping
            if (use_bootstrap) {
                sample_data <- df %>%
                    dplyr::group_by(dplyr::across(dplyr::all_of(x_axis))) %>%
                    dplyr::slice_sample(n = sample_size, replace = TRUE) %>%
                    dplyr::ungroup()
            } else {
                sample_data <- df
            }
            
            # Build formula dynamically with backtick quoting
            formula_obj <- stats::as.formula(
                paste0("`", measure_col, "` ~ `", factor1, "` * `", factor2, "`")
            )
            
            # Perform t2way test
            t2way_out <- WRS2::t2way(
                formula = formula_obj,
                data = sample_data,
                tr = tr_value
            )
            
            results_matrix[i, ] <- c(
                t2way_out$Qa,
                t2way_out$Qb,
                t2way_out$Qab,
                t2way_out$A.p.value,
                t2way_out$B.p.value,
                t2way_out$AB.p.value
            )
        }
        
        results_matrix
    }, test_name = "t2way")
    
    # Handle errors - return structured error object
    if (!test_result$success) {
        return(test_result$error)
    }
    
    # Format results
    boot_results <- test_result$result
    
    # Create effect labels
    effect_labels <- c(factor1, factor2, paste0(factor1, ":", factor2))
    
    if (use_bootstrap) {
        # Calculate CIs and format
        ci_bounds <- apply(boot_results, 2, function(x) {
            stats::quantile(x, c(0.025, 0.975), na.rm = TRUE)
        })
        
        final_results <- data.frame(
            Effect = effect_labels,
            Q.Statistic = c(
                paste0(signif(mean(boot_results$Qa, na.rm = TRUE), 3), " [",
                       signif(ci_bounds[1, "Qa"], 3), " - ", signif(ci_bounds[2, "Qa"], 3), "]"),
                paste0(signif(mean(boot_results$Qb, na.rm = TRUE), 3), " [",
                       signif(ci_bounds[1, "Qb"], 3), " - ", signif(ci_bounds[2, "Qb"], 3), "]"),
                paste0(signif(mean(boot_results$Qab, na.rm = TRUE), 3), " [",
                       signif(ci_bounds[1, "Qab"], 3), " - ", signif(ci_bounds[2, "Qab"], 3), "]")
            ),
            p.value = c(
                paste0(signif(mean(boot_results$A.p.value, na.rm = TRUE), 3), " [",
                       signif(ci_bounds[1, "A.p.value"], 3), " - ", signif(ci_bounds[2, "A.p.value"], 3), "]"),
                paste0(signif(mean(boot_results$B.p.value, na.rm = TRUE), 3), " [",
                       signif(ci_bounds[1, "B.p.value"], 3), " - ", signif(ci_bounds[2, "B.p.value"], 3), "]"),
                paste0(signif(mean(boot_results$AB.p.value, na.rm = TRUE), 3), " [",
                       signif(ci_bounds[1, "AB.p.value"], 3), " - ", signif(ci_bounds[2, "AB.p.value"], 3), "]")
            ),
            stringsAsFactors = FALSE
        )
    } else {
        final_results <- data.frame(
            Effect = effect_labels,
            Q.Statistic = signif(c(boot_results$Qa[1], boot_results$Qb[1], boot_results$Qab[1]), 3),
            p.value = signif(c(boot_results$A.p.value[1], boot_results$B.p.value[1], boot_results$AB.p.value[1]), 3),
            stringsAsFactors = FALSE
        )
    }
    
    final_results
}


#' Perform Three-Way Robust ANOVA (Welch-Yuen t3way)
#'
#' @inheritParams perform_t1way
#' @return List with test results or error
perform_t3way <- function(df, x_axis, measure_col, tr_value,
                          use_bootstrap = FALSE, boot_samples = 599,
                          boot_sample_size = NULL, p_adjust_method = "bonferroni") {
    # TODO: Implement actual t3way test
    list(
        test = "t3way",
        status = "placeholder",
        message = "Three-way Welch-Yuen ANOVA not yet implemented"
    )
}


#' Perform Linear Contrasts (lincon)
#'
#' @inheritParams perform_t1way
#' @return List with contrast results or error
perform_lincon <- function(df, x_axis, measure_col, tr_value,
                           use_bootstrap = FALSE, boot_samples = 599,
                           boot_sample_size = NULL, p_adjust_method = "bonferroni") {
    # TODO: Implement actual lincon test
    list(
        test = "lincon",
        status = "placeholder",
        message = "Linear contrasts not yet implemented"
    )
}


#' Perform Cliff's Delta Effect Size
#'
#' @inheritParams perform_t1way
#' @return List with effect size results or error
perform_cliff <- function(df, x_axis, measure_col, tr_value,
                          use_bootstrap = FALSE, boot_samples = 599,
                          boot_sample_size = NULL, p_adjust_method = "bonferroni") {
    # TODO: Implement actual Cliff's Delta
    list(
        test = "cliff_delta",
        status = "placeholder",
        message = "Cliff's Delta not yet implemented"
    )
}


#' Create Combined Results Table
#'
#' Combines lincon and cliff results into a single formatted table.
#'
#' @param result_lincon List, results from perform_lincon
#' @param result_cliff List, results from perform_cliff
#' @param measure_col Character, measurement column name
#' @param valid_comparisons Logical, filter to valid comparisons only
#' @param filter_p_values Logical, filter to significant p-values only
#' @param p_adjust_method Character, p-value adjustment method used
#' @param x_axis Character vector of grouping columns
#' @param use_scientific Logical, use scientific notation for p-values
#' @return Data frame with combined results
create_combined_results <- function(result_lincon, result_cliff, measure_col,
                                    valid_comparisons = TRUE, filter_p_values = FALSE,
                                    p_adjust_method = "bonferroni", x_axis = NULL,
                                    use_scientific = FALSE) {
    # TODO: Implement actual result combination
    data.frame(
        Comparison = "Placeholder",
        Estimate = NA_real_,
        CI_Lower = NA_real_,
        CI_Upper = NA_real_,
        p_value = NA_real_,
        Cliff_Delta = NA_real_,
        stringsAsFactors = FALSE
    )
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
    
    # Determine design type and header
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
    
    # Linear contrasts (always computed)
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
    
    # Cliff's Delta (always computed)
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
    
    # Combined results table
    result_combined <- create_combined_results(
        result_lincon = result_lincon,
        result_cliff = result_cliff,
        measure_col = measure_col,
        valid_comparisons = params$valid_comparisons,
        filter_p_values = params$filter_p_values,
        p_adjust_method = params$p_val_cor_method,
        x_axis = x_axis,
        use_scientific = params$use_scientific_notation
    )
    
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
