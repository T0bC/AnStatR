#' Parametric ANOVA Statistical Tests
#'
#' Contains classical parametric ANOVA tests:
#' - Handles one-way, two-way, and three-way designs
#' - Includes Tukey HSD post-hoc comparisons
#' - Follows the same output structure as robust tests for UI compatibility


# =============================================================================
# Main Parametric ANOVA Function
# =============================================================================

#' Perform Parametric ANOVA
#'
#' Main entry point for parametric ANOVA that handles 1-way, 2-way, and 3-way designs.
#' Uses standard R aov() function with Type III sums of squares.
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of grouping columns
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, ignored for parametric tests but kept for interface consistency
#' @param use_bootstrap Logical, bootstrap not applicable to parametric tests
#' @param boot_samples Integer, ignored for parametric tests
#' @param boot_sample_size Integer, ignored for parametric tests
#' @param p_adjust_method Character, p-value adjustment method for post-hoc tests
#' @return Data frame with ANOVA results or structured error
perform_parametric_anova <- function(df, x_axis, measure_col, tr_value = 0,
                                   use_bootstrap = FALSE, boot_samples = 599,
                                   boot_sample_size = NULL, p_adjust_method = "bonferroni") {
    
    # Build error context
    error_context <- list(
        measure = measure_col,
        factors = x_axis,
        n_factors = length(x_axis),
        n_observations = nrow(df),
        test_type = "parametric_anova"
    )
    
    # Validate inputs
    if (length(x_axis) == 0 || length(x_axis) > 3) {
        return(simple_error(
            message = "Parametric ANOVA requires 1 to 3 grouping variables.",
            operation_name = "parametric_anova",
            context = error_context
        ))
    }
    
    # Check for minimum group sizes
    for (factor in x_axis) {
        n_levels <- length(unique(df[[factor]]))
        if (n_levels < 2) {
            return(simple_error(
                message = sprintf("Factor '%s' must have at least 2 levels, found %d.", 
                                factor, n_levels),
                operation_name = "parametric_anova",
                context = error_context
            ))
        }
    }
    
    # Build formula
    formula_str <- paste0("`", measure_col, "` ~ ")
    if (length(x_axis) == 1) {
        formula_str <- paste0(formula_str, "`", x_axis[1], "`")
    } else if (length(x_axis) == 2) {
        formula_str <- paste0(formula_str, "`", x_axis[1], "` * `", x_axis[2], "`")
    } else if (length(x_axis) == 3) {
        formula_str <- paste0(formula_str, "`", x_axis[1], "` * `", x_axis[2], "` * `", x_axis[3], "`")
    }
    formula_obj <- stats::as.formula(formula_str)
    
    # Convert grouping variables to factors
    for (var in x_axis) {
        df[[var]] <- as.factor(df[[var]])
    }
    
    # Run ANOVA with error handling
    anova_result <- safe_stat_test({
        # Fit the model
        model <- stats::aov(formula_obj, data = df)
        
        # Get ANOVA table with Type III SS (using car package if available)
        if (requireNamespace("car", quietly = TRUE)) {
            # Type III SS for balanced/unbalanced designs
            anova_table <- car::Anova(model, type = "III")
        } else {
            # Fall back to Type I SS
            anova_table <- summary(model)[[1]]
        }
        
        # Convert to data frame
        result_df <- as.data.frame(anova_table)
        
        # Standardize column names to match robust test output
        if ("Sum Sq" %in% names(result_df)) {
            names(result_df)[names(result_df) == "Sum Sq"] <- "SS"
        }
        if ("Mean Sq" %in% names(result_df)) {
            names(result_df)[names(result_df) == "Mean Sq"] <- "MS"
        }
        if ("F value" %in% names(result_df)) {
            names(result_df)[names(result_df) == "F value"] <- "F"
        }
        if ("Pr(>F)" %in% names(result_df)) {
            names(result_df)[names(result_df) == "Pr(>F)"] <- "p.value"
        }
        
        # Add effect names as a column
        result_df$Effect <- rownames(result_df)
        rownames(result_df) <- NULL
        
        # Reorder columns to match robust test output
        col_order <- c("Effect", "Df", "SS", "MS", "F", "p.value")
        available_cols <- intersect(col_order, names(result_df))
        result_df <- result_df[, available_cols]
        
        # Remove residuals row if present
        result_df <- result_df[result_df$Effect != "Residuals", ]
        
        # Store model for post-hoc tests
        attr(result_df, "model") <- model
        
        result_df
        
    }, test_name = "parametric_anova", context = error_context)
    
    if (!anova_result$success) {
        return(anova_result$error)
    }
    
    anova_result$result
}


# =============================================================================
# Tukey HSD Post-hoc Tests
# =============================================================================

#' Perform Tukey HSD Post-hoc Comparisons
#'
#' Conducts Tukey's Honestly Significant Difference test for pairwise comparisons.
#' Output format matches lincon from robust tests.
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of grouping columns
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, ignored for parametric tests
#' @param use_bootstrap Logical, ignored for parametric tests
#' @param boot_samples Integer, ignored for parametric tests
#' @param boot_sample_size Integer, ignored for parametric tests
#' @param p_adjust_method Character, ignored (Tukey has its own adjustment)
#' @return Data frame with pairwise comparisons or structured error
perform_tukey_hsd <- function(df, x_axis, measure_col, tr_value = 0,
                             use_bootstrap = FALSE, boot_samples = 599,
                             boot_sample_size = NULL, p_adjust_method = "bonferroni") {
    
    # Build error context
    error_context <- list(
        measure = measure_col,
        factors = x_axis,
        n_observations = nrow(df),
        test_type = "tukey_hsd"
    )
    
    # Build formula
    if (length(x_axis) == 1) {
        formula_str <- paste0("`", measure_col, "` ~ `", x_axis[1], "`")
    } else {
        # For multi-way designs, create interaction term
        interaction_term <- paste0("interaction(", 
                                 paste0("`", x_axis, "`", collapse = ", "), 
                                 ")")
        formula_str <- paste0("`", measure_col, "` ~ ", interaction_term)
    }
    formula_obj <- stats::as.formula(formula_str)
    
    # Convert grouping variables to factors
    for (var in x_axis) {
        df[[var]] <- as.factor(df[[var]])
    }
    
    # Run Tukey HSD with error handling
    tukey_result <- safe_stat_test({
        # Fit the model
        model <- stats::aov(formula_obj, data = df)
        
        # Run Tukey HSD
        tukey_out <- stats::TukeyHSD(model)
        
        # Extract results and format to match lincon output
        if (length(x_axis) == 1) {
            tukey_df <- as.data.frame(tukey_out[[x_axis[1]]])
            comparisons <- rownames(tukey_df)
        } else {
            # For interaction terms
            tukey_df <- as.data.frame(tukey_out[[1]])
            comparisons <- rownames(tukey_df)
        }
        
        # Create output matching lincon format
        result_df <- data.frame(
            Group1 = sapply(strsplit(comparisons, "-"), `[`, 1),
            Group2 = sapply(strsplit(comparisons, "-"), `[`, 2),
            psihat = tukey_df$diff,           # Difference in means
            ci.lower = tukey_df$lwr,          # Lower CI bound
            ci.upper = tukey_df$upr,          # Upper CI bound
            p.value = tukey_df$`p adj`,       # Adjusted p-value
            stringsAsFactors = FALSE
        )
        
        # Clean group names (remove spaces)
        result_df$Group1 <- trimws(result_df$Group1)
        result_df$Group2 <- trimws(result_df$Group2)
        
        # Add Interaction column to match lincon format
        result_df$Interaction <- paste(result_df$Group1, "vs.", result_df$Group2)
        
        # Add p.adjusted column (same as p.value since Tukey already adjusts)
        result_df$p.adjusted <- result_df$p.value
        
        result_df
        
    }, test_name = "tukey_hsd", context = error_context)
    
    if (!tukey_result$success) {
        return(tukey_result$error)
    }
    
    tukey_result$result
}


# =============================================================================
# Effect Size for Parametric Tests
# =============================================================================

#' Calculate Cohen's d Effect Size
#'
#' Computes Cohen's d for pairwise comparisons in parametric ANOVA.
#' Output format matches Cliff's Delta from robust tests.
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of grouping columns
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, ignored for parametric tests
#' @param use_bootstrap Logical, ignored for parametric tests
#' @param boot_samples Integer, ignored for parametric tests
#' @param boot_sample_size Integer, ignored for parametric tests
#' @param p_adjust_method Character, p-value adjustment method
#' @return Data frame with effect sizes or structured error
perform_cohens_d <- function(df, x_axis, measure_col, tr_value = 0,
                            use_bootstrap = FALSE, boot_samples = 599,
                            boot_sample_size = NULL, p_adjust_method = "bonferroni") {
    
    # Build error context
    error_context <- list(
        measure = measure_col,
        factors = x_axis,
        n_observations = nrow(df),
        test_type = "cohens_d"
    )
    
    # For multi-way designs, create interaction variable
    if (length(x_axis) > 1) {
        df$interaction_group <- interaction(df[x_axis], sep = "_")
        group_var <- "interaction_group"
    } else {
        group_var <- x_axis[1]
    }
    
    # Get unique groups
    groups <- unique(df[[group_var]])
    n_groups <- length(groups)
    
    if (n_groups < 2) {
        return(simple_error(
            message = "Cohen's d requires at least 2 groups for comparison.",
            operation_name = "cohens_d",
            context = error_context
        ))
    }
    
    # Calculate effect sizes with error handling
    effect_result <- safe_stat_test({
        # Initialize results
        results <- list()
        
        # All pairwise comparisons
        for (i in 1:(n_groups - 1)) {
            for (j in (i + 1):n_groups) {
                group1_data <- df[df[[group_var]] == groups[i], measure_col]
                group2_data <- df[df[[group_var]] == groups[j], measure_col]
                
                # Calculate means and SDs
                mean1 <- mean(group1_data, na.rm = TRUE)
                mean2 <- mean(group2_data, na.rm = TRUE)
                sd1 <- sd(group1_data, na.rm = TRUE)
                sd2 <- sd(group2_data, na.rm = TRUE)
                n1 <- length(group1_data)
                n2 <- length(group2_data)
                
                # Pooled standard deviation
                pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
                
                # Cohen's d
                d <- (mean1 - mean2) / pooled_sd
                
                # Standard error of d
                se_d <- sqrt((n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2)))
                
                # 95% CI for d
                ci_lower <- d - 1.96 * se_d
                ci_upper <- d + 1.96 * se_d
                
                # T-test for p-value
                t_test <- stats::t.test(group1_data, group2_data, var.equal = TRUE)
                
                results[[length(results) + 1]] <- data.frame(
                    Group1 = as.character(groups[i]),
                    Group2 = as.character(groups[j]),
                    n1 = n1,
                    n2 = n2,
                    d = d,
                    ci.lower = ci_lower,
                    ci.upper = ci_upper,
                    p.value = t_test$p.value,
                    stringsAsFactors = FALSE
                )
            }
        }
        
        # Combine results
        result_df <- do.call(rbind, results)
        
        # Apply p-value adjustment
        result_df$p.adjusted <- stats::p.adjust(result_df$p.value, method = p_adjust_method)
        
        # Add Interaction column to match Cliff's Delta format
        result_df$Interaction <- paste(result_df$Group1, "vs.", result_df$Group2)
        
        # Rename to match Cliff's Delta output format
        names(result_df)[names(result_df) == "d"] <- "delta"
        names(result_df)[names(result_df) == "p.adjusted"] <- "p.value"
        
        # Also rename delta to psihat to match expected format
        names(result_df)[names(result_df) == "delta"] <- "psihat"
        
        # Select columns to match Cliff's Delta output
        result_df[, c("Group1", "Group2", "n1", "n2", "psihat", "ci.lower", "ci.upper", "p.value", "Interaction")]
        
    }, test_name = "cohens_d", context = error_context)
    
    if (!effect_result$success) {
        return(effect_result$error)
    }
    
    effect_result$result
}
