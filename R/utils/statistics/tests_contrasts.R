#' Linear Contrasts Statistical Tests
#'
#' Contains post-hoc linear contrasts tests for both robust and parametric approaches:
#' - lincon: Linear contrasts for robust tests (Welch-Yuen family)
#' - Uses trimmed means for robust approach
#' - Uses standard means for parametric approach

# Import required modules
box::use(../statistics_utils)
box::use(../error_handling)
library(WRS2)
library(dplyr)
library(stats)

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
        combined_groups <- do.call(paste, c(df[x_axis], sep = "."))
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
        combined_groups <- do.call(paste, c(df[x_axis], sep = "."))
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
        sample_data$combinedGroups <- do.call(paste, c(sample_data[x_axis], sep = "."))
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
        Lincon.psihat = comp_df$psihat,
        Lincon.ci.lower = comp_df$ci.lower,
        Lincon.ci.upper = comp_df$ci.upper,
        Lincon.p.value = comp_df$p.value,
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
    # Check if results list is valid
    if (length(results_list) == 0) {
        return(data.frame(stringsAsFactors = FALSE))
    }
    
    # Get unique interactions across all iterations
    unique_interactions <- unique(unlist(lapply(results_list, function(x) {
        if ("Interaction" %in% names(x) && nrow(x) > 0) x$Interaction else character(0)
    })))
    
    if (length(unique_interactions) == 0) {
        return(data.frame(stringsAsFactors = FALSE))
    }
    
    result_rows <- lapply(unique_interactions, function(interaction) {
        # Subset all iterations for this interaction
        subset_dfs <- lapply(results_list, function(x) {
            if ("Interaction" %in% names(x) && nrow(x) > 0) {
                x[x$Interaction == interaction, ]
            } else {
                NULL
            }
        })
        subset_dfs <- subset_dfs[!sapply(subset_dfs, is.null)]
        
        if (length(subset_dfs) == 0) {
            return(NULL)
        }
        
        combined <- dplyr::bind_rows(subset_dfs)
        
        # Calculate CI bounds
        ci_lower_fn <- function(x) stats::quantile(x, probs = 0.025, na.rm = TRUE)
        ci_upper_fn <- function(x) stats::quantile(x, probs = 0.975, na.rm = TRUE)
        
        numeric_cols <- c("Lincon.psihat", "Lincon.ci.lower", "Lincon.ci.upper", "Lincon.p.value")
        available_cols <- intersect(numeric_cols, names(combined))
        
        format_col <- function(col_name) {
            if (col_name %in% names(combined)) {
                vals <- combined[[col_name]]
                if (length(vals) > 0 && any(!is.na(vals))) {
                    mean_val <- signif(mean(vals, na.rm = TRUE), 3)
                    lower <- signif(ci_lower_fn(vals), 3)
                    upper <- signif(ci_upper_fn(vals), 3)
                    paste0(mean_val, " [", lower, " - ", upper, "]")
                } else {
                    NA_character_
                }
            } else {
                NA_character_
            }
        }
        
        # For bootstrap, we don't adjust p-values individually, just show the bootstrap distribution
        data.frame(
            Interaction = interaction,
            Lincon.psihat = format_col("Lincon.psihat"),
            Lincon.ci.lower = format_col("Lincon.ci.lower"),
            Lincon.ci.upper = format_col("Lincon.ci.upper"),
            Lincon.p.value = format_col("Lincon.p.value"),
            stringsAsFactors = FALSE
        )
    })
    
    # Remove NULL results
    result_rows <- result_rows[!sapply(result_rows, is.null)]
    
    if (length(result_rows) == 0) {
        return(data.frame(stringsAsFactors = FALSE))
    }
    
    final_df <- dplyr::bind_rows(result_rows)
    
    # Add p.adjusted column for consistency with single-run results
    # For bootstrap, we'll use the same values as the original p-values
    if ("Lincon.p.value" %in% names(final_df)) {
        final_df$p.adjusted <- final_df$Lincon.p.value
    }
    
    final_df
}

#' Format lincon single-run results
#'
#' @param result_df Data frame from single lincon run
#' @param p_adjust_method P-value adjustment method
#' @return Data frame with formatted results
format_lincon_single <- function(result_df, p_adjust_method) {
    # Check if the required column exists and has data
    if (!"Lincon.p.value" %in% names(result_df) || nrow(result_df) == 0) {
        return(result_df)
    }
    
    # Apply p-value adjustment only if there are valid p-values
    p_values <- result_df$Lincon.p.value
    if (length(p_values) > 0 && any(!is.na(p_values))) {
        result_df$p.adjusted <- stats::p.adjust(p_values, method = p_adjust_method)
    } else {
        result_df$p.adjusted <- rep(NA, nrow(result_df))
    }
    
    # Round numeric columns
    numeric_cols <- c("Lincon.psihat", "Lincon.ci.lower", "Lincon.ci.upper", "Lincon.p.value", "p.adjusted")
    available_cols <- intersect(numeric_cols, names(result_df))
    result_df[available_cols] <- lapply(result_df[available_cols], function(x) signif(x, 3))
    
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
    test_result <- statistics_utils$safe_stat_test({
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
