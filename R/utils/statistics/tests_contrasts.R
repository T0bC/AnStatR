#' Linear Contrast Statistical Tests
#'
#' Contains pairwise comparison tests using linear contrasts:
#' - lincon (linear contrasts)
#' - Future: mcp2atm, other contrast methods


# =============================================================================
# lincon Helper Functions
# =============================================================================

#' Validate lincon inputs
#'
#' @param df Data frame
#' @param x_axis Grouping columns
#' @return Error data frame or NULL if valid
validate_lincon <- function(df, x_axis) {
    # Create combined groups for multi-factor designs
    if (length(x_axis) > 1) {
        combined_groups <- do.call(paste, c(df[x_axis], sep = "_"))
    } else {
        combined_groups <- df[[x_axis[1]]]
    }
    
    n_groups <- length(unique(combined_groups))
    if (n_groups < 2) {
        return(data.frame(
            Error = paste0("lincon requires at least 2 groups, found ", n_groups, "."),
            stringsAsFactors = FALSE
        ))
    }
    NULL
}

#' Build error context for lincon
#'
#' @param df Data frame
#' @param x_axis Grouping columns
#' @param measure_col Measurement column
#' @param tr_value Trim proportion
#' @param use_bootstrap Logical
#' @return List with context information
build_lincon_context <- function(df, x_axis, measure_col, tr_value, use_bootstrap) {
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
        trim = tr_value,
        bootstrap = use_bootstrap
    )
}

#' Run single lincon iteration
#'
#' @param sample_data Data frame for this iteration
#' @param x_axis Grouping columns
#' @param measure_col Measurement column
#' @param tr_value Trim proportion
#' @return Data frame with lincon results for this iteration
run_lincon_iteration <- function(sample_data, x_axis, measure_col, tr_value) {
    # Combine groups for multi-factor designs
    if (length(x_axis) > 1) {
        sample_data$combinedGroups <- do.call(paste, c(sample_data[x_axis], sep = "_"))
    } else {
        sample_data$combinedGroups <- sample_data[[x_axis[1]]]
    }
    sample_data$combinedGroups <- as.factor(sample_data$combinedGroups)
    
    # Build formula
    formula_obj <- stats::as.formula(paste0("`", measure_col, "` ~ combinedGroups"))
    
    # Run lincon test
    lincon_result <- WRS2::lincon(
        formula = formula_obj,
        data = sample_data,
        tr = tr_value,
        method = "none"
    )
    
    # Extract comparison results
    comp_df <- as.data.frame(lincon_result$comp)
    
    # Build interaction labels from factor names
    interaction_labels <- paste(
        lincon_result$fnames[comp_df[, 1]],
        lincon_result$fnames[comp_df[, 2]],
        sep = " vs. "
    )
    
    data.frame(
        Interaction = interaction_labels,
        psihat = comp_df$psihat,
        ci.lower = comp_df$ci.lower,
        ci.upper = comp_df$ci.upper,
        p.value = comp_df$p.value,
        stringsAsFactors = FALSE
    )
}

#' Format lincon bootstrap results
#'
#' Aggregates bootstrap iterations into mean [CI] format.
#'
#' @param results_list List of data frames from bootstrap iterations
#' @param p_adjust_method P-value adjustment method
#' @return Data frame with formatted results
format_lincon_bootstrap <- function(results_list, p_adjust_method) {
    # Get unique interactions across all iterations
    unique_interactions <- unique(unlist(lapply(results_list, function(x) x$Interaction)))
    
    result_rows <- lapply(unique_interactions, function(interaction) {
        # Subset all iterations for this interaction
        subset_dfs <- lapply(results_list, function(x) x[x$Interaction == interaction, ])
        combined <- dplyr::bind_rows(subset_dfs)
        
        # Calculate CI bounds
        ci_lower_fn <- function(x) stats::quantile(x, probs = 0.025, na.rm = TRUE)
        ci_upper_fn <- function(x) stats::quantile(x, probs = 0.975, na.rm = TRUE)
        
        numeric_cols <- c("psihat", "ci.lower", "ci.upper", "p.value")
        
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
            stringsAsFactors = FALSE
        )
    })
    
    dplyr::bind_rows(result_rows)
}

#' Format lincon single-run results
#'
#' @param result_df Data frame from single lincon run
#' @param p_adjust_method P-value adjustment method
#' @return Data frame with formatted results
format_lincon_single <- function(result_df, p_adjust_method) {
    result_df$p.adjusted <- stats::p.adjust(result_df$p.value, method = p_adjust_method)
    
    # Round numeric columns
    numeric_cols <- c("psihat", "ci.lower", "ci.upper", "p.value", "p.adjusted")
    result_df[numeric_cols] <- lapply(result_df[numeric_cols], function(x) signif(x, 3))
    
    result_df
}


# =============================================================================
# Main lincon Function
# =============================================================================

#' Perform Linear Contrasts (lincon)
#'
#' Performs pairwise comparisons using trimmed means and linear contrasts.
#' For multi-factor designs, groups are combined into a single factor.
#'
#' @param df Data frame containing the data (already filtered for outliers/trimmed)
#' @param x_axis Character vector of grouping column(s)
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, trim proportion (0-0.5)
#' @param use_bootstrap Logical, whether to use bootstrap
#' @param boot_samples Integer, number of bootstrap samples
#' @param boot_sample_size Integer or NULL, bootstrap sample size per group
#' @param p_adjust_method Character, p-value adjustment method
#' @return Data frame with contrast results or error
perform_lincon <- function(df, x_axis, measure_col, tr_value,
                           use_bootstrap = FALSE, boot_samples = 599,
                           boot_sample_size = NULL, p_adjust_method = "bonferroni") {
    
    # 1. Validate inputs
    validation_error <- validate_lincon(df, x_axis)
    if (!is.null(validation_error)) return(validation_error)
    
    # 2. Setup bootstrap parameters
    boot_params <- setup_bootstrap_params(df, x_axis, use_bootstrap, boot_samples, boot_sample_size)
    
    # 3. Build error context
    error_context <- build_lincon_context(df, x_axis, measure_col, tr_value, use_bootstrap)
    
    # 4. Run the test iterations
    test_result <- safe_stat_test({
        results_list <- vector("list", boot_params$n_iterations)
        
        for (i in seq_len(boot_params$n_iterations)) {
            sample_data <- sample_for_iteration(df, x_axis, use_bootstrap, boot_params$sample_size)
            results_list[[i]] <- run_lincon_iteration(sample_data, x_axis, measure_col, tr_value)
        }
        
        results_list
    }, test_name = "lincon", context = error_context)
    
    # 5. Handle errors
    if (!test_result$success) return(test_result$error)
    
    # 6. Format results
    if (use_bootstrap) {
        format_lincon_bootstrap(test_result$result, p_adjust_method)
    } else {
        format_lincon_single(test_result$result[[1]], p_adjust_method)
    }
}
