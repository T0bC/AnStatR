#' ANOVA Statistical Tests (Welch-Yuen Family)
#'
#' Contains robust ANOVA tests using trimmed means:
#' - One-way (t1way)
#' - Two-way (t2way)
#' - Three-way (t3way)


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
    
    # Build context for error reporting
    error_context <- list(
        measure = measure_col,
        grouping = group_col,
        n_groups = n_groups,
        n_observations = nrow(df),
        trim = tr_value,
        bootstrap = use_bootstrap
    )
    
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
    }, test_name = "t1way", context = error_context)
    
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
    
    # Build context for error reporting
    error_context <- list(
        measure = measure_col,
        factor1 = factor1,
        factor2 = factor2,
        levels_factor1 = n_levels_1,
        levels_factor2 = n_levels_2,
        n_observations = nrow(df),
        trim = tr_value,
        bootstrap = use_bootstrap
    )
    
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
    }, test_name = "t2way", context = error_context)
    
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
