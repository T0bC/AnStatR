#' Effect Size Statistical Tests
#'
#' Contains effect size calculations:
#' - Cliff's Delta
#' - Future: Cohen's d, Hedges' g, etc.


# =============================================================================
# Cliff's Delta Helper Functions
# =============================================================================

#' Validate cliff inputs
#'
#' @param df Data frame
#' @param x_axis Grouping columns
#' @return Error data frame or NULL if valid
validate_cliff <- function(df, x_axis) {
    # Create combined groups for multi-factor designs
    if (length(x_axis) > 1) {
        combined_groups <- do.call(paste, c(df[x_axis], sep = "_"))
    } else {
        combined_groups <- df[[x_axis[1]]]
    }
    
    n_groups <- length(unique(combined_groups))
    if (n_groups < 2) {
        return(data.frame(
            Error = paste0("Cliff's Delta requires at least 2 groups, found ", n_groups, "."),
            stringsAsFactors = FALSE
        ))
    }
    NULL
}

#' Build error context for cliff
#'
#' @param df Data frame
#' @param x_axis Grouping columns
#' @param measure_col Measurement column
#' @param use_bootstrap Logical
#' @return List with context information
build_cliff_context <- function(df, x_axis, measure_col, use_bootstrap) {
    if (length(x_axis) > 1) {
        combined_groups <- do.call(paste, c(df[x_axis], sep = "_"))
    } else {
        combined_groups <- df[[x_axis[1]]]
    }
    
    list(
        measure = measure_col,
        grouping = paste(x_axis, collapse = ", "),
        n_groups = length(unique(combined_groups)),
        n_observations = nrow(df),
        bootstrap = use_bootstrap
    )
}

#' Run single cliff iteration
#'
#' @param sample_data Data frame for this iteration
#' @param x_axis Grouping columns
#' @param measure_col Measurement column
#' @return Data frame with cliff results for this iteration
run_cliff_iteration <- function(sample_data, x_axis, measure_col) {
    # Combine groups for multi-factor designs
    if (length(x_axis) > 1) {
        sample_data$combinedGroups <- do.call(paste, c(sample_data[x_axis], sep = "_"))
    } else {
        sample_data$combinedGroups <- sample_data[[x_axis[1]]]
    }
    sample_data$combinedGroupsNum <- as.numeric(as.factor(sample_data$combinedGroups))
    
    # Run cidmulv2_labelled (Cliff's Delta for multiple groups)
    cliff_result <- cidmulv2_labelled(
        data = sample_data,
        gcode = "combinedGroupsNum",
        glab = "combinedGroups",
        dp = measure_col,
        alpha = 0.05,
        CI.FWE = FALSE
    )
    
    # Extract and format test results
    test_df <- cliff_result$test
    
    data.frame(
        Interaction = paste(test_df$Group.A, test_df$Group.B, sep = " vs. "),
        psihat = test_df$p.hat,
        ci.lower = test_df$p.ci.lower,
        ci.upper = test_df$p.ci.upper,
        p.value = test_df$p.value,
        p.adjusted = test_df$p.crit,
        stringsAsFactors = FALSE
    )
}

#' Format cliff bootstrap results
#'
#' Aggregates bootstrap iterations into mean [CI] format.
#'
#' @param results_list List of data frames from bootstrap iterations
#' @param p_adjust_method P-value adjustment method (not used, kept for consistency)
#' @return Data frame with formatted results
format_cliff_bootstrap <- function(results_list, p_adjust_method) {
    # Get unique interactions across all iterations
    unique_interactions <- unique(unlist(lapply(results_list, function(x) x$Interaction)))
    
    result_rows <- lapply(unique_interactions, function(interaction) {
        # Subset all iterations for this interaction
        subset_dfs <- lapply(results_list, function(x) x[x$Interaction == interaction, ])
        combined <- dplyr::bind_rows(subset_dfs)
        
        # Calculate CI bounds
        ci_lower_fn <- function(x) stats::quantile(x, probs = 0.025, na.rm = TRUE)
        ci_upper_fn <- function(x) stats::quantile(x, probs = 0.975, na.rm = TRUE)
        
        format_col <- function(col_name) {
            vals <- combined[[col_name]]
            mean_val <- signif(mean(vals, na.rm = TRUE), 3)
            lower <- signif(ci_lower_fn(vals), 3)
            upper <- signif(ci_upper_fn(vals), 3)
            paste0(mean_val, " [", lower, " - ", upper, "]")
        }
        
        data.frame(
            Interaction = interaction,
            psihat = format_col("psihat"),
            ci.lower = format_col("ci.lower"),
            ci.upper = format_col("ci.upper"),
            p.value = format_col("p.value"),
            p.adjusted = format_col("p.adjusted"),
            stringsAsFactors = FALSE
        )
    })
    
    dplyr::bind_rows(result_rows)
}

#' Format cliff single-run results
#'
#' @param result_df Data frame from single cliff run
#' @param p_adjust_method P-value adjustment method (not used, p.adjusted comes from cidmulv2)
#' @return Data frame with formatted results
format_cliff_single <- function(result_df, p_adjust_method) {
    # Round numeric columns
    numeric_cols <- c("psihat", "ci.lower", "ci.upper", "p.value", "p.adjusted")
    result_df[numeric_cols] <- lapply(result_df[numeric_cols], function(x) signif(x, 3))
    
    result_df
}


# =============================================================================
# Main Cliff Function
# =============================================================================

#' Perform Cliff's Delta Effect Size
#'
#' Performs pairwise Cliff's Delta effect size calculations for all group pairs.
#' For multi-factor designs, groups are combined into a single factor.
#' Uses cidmulv2_labelled from Rallfun-v43.R with Hochberg's method for FWE control.
#'
#' @param df Data frame containing the data (already filtered for outliers/trimmed)
#' @param x_axis Character vector of grouping column(s)
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, trim proportion (not used for Cliff's Delta, kept for API consistency)
#' @param use_bootstrap Logical, whether to use bootstrap
#' @param boot_samples Integer, number of bootstrap samples
#' @param boot_sample_size Integer or NULL, bootstrap sample size per group
#' @param p_adjust_method Character, p-value adjustment method (not used, Hochberg's method is built-in)
#' @return Data frame with effect size results or error
perform_cliff <- function(df, x_axis, measure_col, tr_value,
                          use_bootstrap = FALSE, boot_samples = 599,
                          boot_sample_size = NULL, p_adjust_method = "bonferroni") {
    
    # 1. Validate inputs
    validation_error <- validate_cliff(df, x_axis)
    if (!is.null(validation_error)) return(validation_error)
    
    # 2. Setup bootstrap parameters
    boot_params <- setup_bootstrap_params(df, x_axis, use_bootstrap, boot_samples, boot_sample_size)
    
    # 3. Build error context
    error_context <- build_cliff_context(df, x_axis, measure_col, use_bootstrap)
    
    # 4. Run the test iterations
    test_result <- safe_stat_test({
        results_list <- vector("list", boot_params$n_iterations)
        
        for (i in seq_len(boot_params$n_iterations)) {
            sample_data <- sample_for_iteration(df, x_axis, use_bootstrap, boot_params$sample_size)
            results_list[[i]] <- run_cliff_iteration(sample_data, x_axis, measure_col)
        }
        
        results_list
    }, test_name = "cliff", context = error_context)
    
    # 5. Handle errors
    if (!test_result$success) return(test_result$error)
    
    # 6. Format results
    if (use_bootstrap) {
        format_cliff_bootstrap(test_result$result, p_adjust_method)
    } else {
        format_cliff_single(test_result$result[[1]], p_adjust_method)
    }
}
