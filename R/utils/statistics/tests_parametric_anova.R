#' Parametric ANOVA Statistical Tests
#'
#' Contains classical parametric ANOVA tests:
#' - Handles one-way, two-way, and three-way designs
#' - Includes Tukey HSD post-hoc comparisons
#' - Follows the same output structure as robust tests for UI compatibility

# Import required modules
box::use(../statistics_utils)
box::use(../error_handling)


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
        return(error_handling$simple_error(
            message = "Parametric ANOVA requires 1 to 3 grouping variables.",
            operation_name = "parametric_anova",
            context = error_context
        ))
    }
    
    # Check for minimum group sizes
    for (factor in x_axis) {
        n_levels <- length(unique(df[[factor]]))
        if (n_levels < 2) {
            return(error_handling$simple_error(
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
    anova_result <- statistics_utils$safe_stat_test({
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
        
        # Apply rounding to match robust ANOVA format (3 significant figures)
        numeric_cols <- sapply(result_df, is.numeric)
        result_df[numeric_cols] <- lapply(result_df[numeric_cols], function(x) signif(x, 3))
        
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
#' @details
#' For single-factor designs, returns standard Tukey HSD results with 
#' family-wise error rate controlled p-values.
#' 
#' For multi-factor designs, returns raw (unadjusted) p-values and confidence
#' intervals for downstream filtering and multiple comparison corrections.
#'
#' @return Data frame with pairwise comparisons. Structure differs by design:
#' \itemize{
#'   \item Single-factor: Tukey-adjusted p-values and CIs
#'   \item Multi-factor: Raw p-values, raw CIs, and statistical details
#' }
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of grouping columns
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, ignored for parametric tests
#' @param use_bootstrap Logical, ignored for parametric tests
#' @param boot_samples Integer, ignored for parametric tests
#' @param boot_sample_size Integer, ignored for parametric tests
#' @param p_adjust_method Character, ignored (Tukey has its own adjustment)
#' @param use_scientific Logical, use scientific notation for results
#' @return Data frame with pairwise comparisons or structured error
perform_tukey_hsd <- function(df, x_axis, measure_col, tr_value = 0,
                             use_bootstrap = FALSE, boot_samples = 599,
                             boot_sample_size = NULL, p_adjust_method = "bonferroni",
                             use_scientific = FALSE) {
    
    error_context <- list(
        measure = measure_col,
        factors = x_axis,
        n_observations = nrow(df),
        test_type = "tukey_hsd"
    )
    
    is_single_factor <- length(x_axis) == 1
    
    # Convert grouping variables to factors
    for (var in x_axis) {
        df[[var]] <- as.factor(df[[var]])
    }
    
    # Build formula and get group sizes
    formula_obj <- build_tukey_formula(measure_col, x_axis)
    group_sizes <- get_group_sizes(df, x_axis, measure_col)
    
    # Run Tukey HSD with error handling
    tukey_result <- statistics_utils$safe_stat_test({
        model <- stats::aov(formula_obj, data = df)
        tukey_out <- stats::TukeyHSD(model, conf.level = 0.95)
        
        # Extract model statistics (needed for both paths)
        model_stats <- extract_model_stats(model)
        
        # Extract and format results
        tukey_df <- extract_tukey_results(tukey_out, x_axis)
        comparisons <- rownames(tukey_df)
        
        # Build base result dataframe
        result_df <- build_base_results(comparisons, tukey_df, use_scientific)
        
        # Add path-specific columns
        if (is_single_factor) {
            result_df <- add_single_factor_columns(result_df, tukey_df, group_sizes, model_stats, use_scientific)
        } else {
            result_df <- add_multi_factor_columns(result_df, comparisons, group_sizes, model_stats, use_scientific)
        }
        
        # Add metadata attributes
        add_result_attributes(result_df, is_single_factor, x_axis, model_stats, group_sizes)
        
    }, test_name = "tukey_hsd", context = error_context)
    
    if (!tukey_result$success) {
        return(tukey_result$error)
    }
    
    tukey_result$result
}

# =============================================================================
# Helper Functions
# =============================================================================

#' Build formula for Tukey test
build_tukey_formula <- function(measure_col, x_axis) {
    if (length(x_axis) == 1) {
        formula_str <- paste0("`", measure_col, "` ~ `", x_axis[1], "`")
    } else {
        interaction_term <- paste0("interaction(", 
                                  paste0("`", x_axis, "`", collapse = ", "), 
                                  ")")
        formula_str <- paste0("`", measure_col, "` ~ ", interaction_term)
    }
    stats::as.formula(formula_str)
}

#' Get group sizes
get_group_sizes <- function(df, x_axis, measure_col) {
    if (length(x_axis) == 1) {
        tapply(df[[measure_col]], df[[x_axis[1]]], function(x) sum(!is.na(x)))
    } else {
        interaction_groups <- interaction(df[x_axis], sep = ".")
        tapply(df[[measure_col]], interaction_groups, function(x) sum(!is.na(x)))
    }
}

#' Extract model statistics
extract_model_stats <- function(model) {
    summary_table <- summary(model)[[1]]
    list(
        mse = summary_table["Residuals", "Mean Sq"],
        df_residual = summary_table["Residuals", "Df"]
    )
}

#' Extract Tukey results
extract_tukey_results <- function(tukey_out, x_axis) {
    if (length(x_axis) == 1) {
        as.data.frame(tukey_out[[x_axis[1]]])
    } else {
        as.data.frame(tukey_out[[1]])
    }
}

#' Build base result dataframe (common to both paths)
build_base_results <- function(comparisons, tukey_df, use_scientific = FALSE) {
    # Store original scipen and set based on preference
    old_scipen <- getOption("scipen")
    on.exit(options(scipen = old_scipen))
    options(scipen = if (use_scientific) 0 else 999)
    
    # Parse groups in reverse order to match Cohen's d ordering
    group_pairs <- lapply(comparisons, function(comp) {
        parts <- strsplit(comp, "-")[[1]]
        if (length(parts) > 2) {
            g1 <- paste(parts[-length(parts)], collapse = "-")
            g2 <- parts[length(parts)]
        } else {
            g1 <- parts[1]
            g2 <- parts[2]
        }
        list(g1 = trimws(g2), g2 = trimws(g1))  # Reverse to match Cohen's d
    })
    
    result_df <- data.frame(
        Interaction = paste(sapply(group_pairs, `[[`, "g1"), "vs.", sapply(group_pairs, `[[`, "g2")),
        Difference = signif(tukey_df$diff, 3),
        stringsAsFactors = FALSE
    )
    
    result_df
}

#' Calculate raw p-values from differences
#' Used by both single and multi-factor paths
calculate_raw_pvalues <- function(differences, group_pairs, group_sizes, model_stats) {
    vapply(seq_along(differences), function(i) {
        g1 <- group_pairs$Group1[i]
        g2 <- group_pairs$Group2[i]
        
        n1 <- group_sizes[g1]
        n2 <- group_sizes[g2]
        
        if (is.na(n1) || is.na(n2)) {
            return(NA_real_)
        }
        
        se_diff <- stats::sqrt(model_stats$mse * (1/n1 + 1/n2))
        t_stat <- differences[i] / se_diff
        2 * stats::pt(abs(t_stat), df = model_stats$df_residual, lower.tail = FALSE)
    }, numeric(1))
}

#' Add single-factor specific columns
add_single_factor_columns <- function(result_df, tukey_df, group_sizes, model_stats, use_scientific = FALSE) {
    # Store original scipen and set based on preference
    old_scipen <- getOption("scipen")
    on.exit(options(scipen = old_scipen))
    options(scipen = if (use_scientific) 0 else 999)
    
    result_df$ci.lower <- signif(tukey_df$lwr, 3)
    result_df$ci.upper <- signif(tukey_df$upr, 3)
    result_df$p.value <- signif(tukey_df$`p adj`, 3)
    result_df$p.value.raw <- signif(calculate_raw_pvalues(
        result_df$Difference,
        # Extract Group1 and Group2 from Interaction for p-value calculation
        data.frame(
            Group1 = sapply(strsplit(result_df$Interaction, " vs. "), `[`, 1),
            Group2 = sapply(strsplit(result_df$Interaction, " vs. "), `[`, 2)
        ),
        group_sizes,
        model_stats
    ), 3)
    result_df
}

#' Add multi-factor specific columns
add_multi_factor_columns <- function(result_df, comparisons, group_sizes, model_stats, use_scientific = FALSE) {
    # Store original scipen and set based on preference
    old_scipen <- getOption("scipen")
    on.exit(options(scipen = old_scipen))
    options(scipen = if (use_scientific) 0 else 999)
    
    # Extract Group1 and Group2 from Interaction for calculations
    group_pairs <- data.frame(
        Group1 = sapply(strsplit(result_df$Interaction, " vs. "), `[`, 1),
        Group2 = sapply(strsplit(result_df$Interaction, " vs. "), `[`, 2)
    )
    
    n_comparisons <- nrow(result_df)
    
    # Pre-allocate vectors
    se_values <- numeric(n_comparisons)
    t_values <- numeric(n_comparisons)
    raw_ci_lower <- numeric(n_comparisons)
    raw_ci_upper <- numeric(n_comparisons)
    
    for (i in seq_len(n_comparisons)) {
        g1 <- group_pairs$Group1[i]
        g2 <- group_pairs$Group2[i]
        n1 <- group_sizes[g1]
        n2 <- group_sizes[g2]
        
        if (is.na(n1) || is.na(n2)) {
            warning(paste("Could not find group sizes for:", g1, "vs", g2))
            se_values[i] <- t_values[i] <- raw_ci_lower[i] <- raw_ci_upper[i] <- NA
            next
        }
        
        se_values[i] <- stats::sqrt(model_stats$mse * (1/n1 + 1/n2))
        t_values[i] <- result_df$Difference[i] / se_values[i]
        
        margin <- stats::qt(0.975, df = model_stats$df_residual) * se_values[i]
        raw_ci_lower[i] <- result_df$Difference[i] - margin
        raw_ci_upper[i] <- result_df$Difference[i] + margin
    }
    
    result_df$SE <- signif(se_values, 3)
    result_df$t.value <- signif(t_values, 3)
    result_df$df.residual <- model_stats$df_residual
    result_df$ci.lower.raw <- signif(raw_ci_lower, 3)
    result_df$ci.upper.raw <- signif(raw_ci_upper, 3)
    result_df$p.value.raw <- signif(calculate_raw_pvalues(
        result_df$Difference,
        group_pairs,
        group_sizes,
        model_stats
    ), 3)
    
    result_df
}

#' Parse comparison string into group names
#' Handles group names that may contain hyphens
parse_comparison_groups <- function(comparison) {
    # Try regex split first
    parts <- strsplit(comparison, "\\s*-\\s*|(?<![^\\s])-(?=[^\\s])", perl = TRUE)[[1]]
    
    if (length(parts) != 2) {
        # Fallback: simple split and recombine
        parts <- strsplit(comparison, "-")[[1]]
        if (length(parts) > 2) {
            g1 <- paste(parts[-length(parts)], collapse = "-")
            g2 <- parts[length(parts)]
        } else {
            g1 <- parts[1]
            g2 <- parts[2]
        }
    } else {
        g1 <- parts[1]
        g2 <- parts[2]
    }
    
    list(g1 = trimws(g1), g2 = trimws(g2))
}

#' Add metadata attributes to result
add_result_attributes <- function(result_df, is_single_factor, x_axis, 
                                 model_stats, group_sizes) {
    attr(result_df, "design_type") <- if (is_single_factor) "single_factor" else "multi_factor"
    attr(result_df, "n_factors") <- length(x_axis)
    attr(result_df, "factors") <- x_axis
    
    if (!is_single_factor) {
        attr(result_df, "mse") <- model_stats$mse
        attr(result_df, "df_residual") <- model_stats$df_residual
        attr(result_df, "group_sizes") <- group_sizes
    }
    
    result_df
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
    
    # Convert grouping variables to factors
    for (var in x_axis) {
        df[[var]] <- as.factor(df[[var]])
    }
    
    # For multi-way designs, create interaction variable
    if (length(x_axis) > 1) {
        df$interaction_group <- interaction(df[x_axis], sep = ".")
        group_var <- "interaction_group"
    } else {
        group_var <- x_axis[1]
    }
    
    # Get unique groups - use factor levels for consistent ordering with Tukey HSD
    if (length(x_axis) > 1) {
        groups <- levels(df$interaction_group)
        # Fallback if interaction_group is not a factor
        if (is.null(groups)) {
            groups <- unique(df$interaction_group)
        }
    } else {
        groups <- levels(df[[x_axis[1]]])
        # Fallback if column is not a factor
        if (is.null(groups)) {
            groups <- unique(df[[x_axis[1]]])
        }
    }
    n_groups <- length(groups)
    
    if (n_groups < 2) {
        return(error_handling$simple_error(
            message = "Cohen's d requires at least 2 groups for comparison.",
            operation_name = "cohens_d",
            context = error_context
        ))
    }
    
    # Calculate effect sizes with error handling
    effect_result <- statistics_utils$safe_stat_test({
        # Initialize results
        results <- list()
        
        # All pairwise comparisons
        for (i in 1:(n_groups - 1)) {
            for (j in (i + 1):n_groups) {
                group1_data <- df[df[[group_var]] == groups[i], measure_col]
                group2_data <- df[df[[group_var]] == groups[j], measure_col]
                
                # Calculate means and SDs
                mean1 <- stats::mean(group1_data, na.rm = TRUE)
                mean2 <- stats::mean(group2_data, na.rm = TRUE)
                sd1 <- stats::sd(group1_data, na.rm = TRUE)
                sd2 <- stats::sd(group2_data, na.rm = TRUE)
                n1 <- length(group1_data)
                n2 <- length(group2_data)
                
                # Pooled standard deviation
                pooled_sd <- stats::sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
                
                # Cohen's d
                d <- (mean1 - mean2) / pooled_sd
                
                # Standard error of d
                se_d <- stats::sqrt((n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2)))
                
                # 95% CI for d
                ci_lower <- d - 1.96 * se_d
                ci_upper <- d + 1.96 * se_d
                
                # T-test for p-value
                t_test <- stats::t.test(group1_data, group2_data, var.equal = TRUE)
                
                results[[length(results) + 1]] <- data.frame(
                    Interaction = paste(as.character(groups[i]), "vs.", as.character(groups[j])),
                    n1 = n1,
                    n2 = n2,
                    Cohen.d = d,
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
        result_df$p.value.adjusted <- stats::p.adjust(result_df$p.value, method = p_adjust_method)
        
        # Rename to match Cliff's Delta output format
        names(result_df)[names(result_df) == "Cohen.d"] <- "Cohen.d"
        names(result_df)[names(result_df) == "p.value.adjusted"] <- "p.value"
        
        # Select columns - Interaction first, no Group1/Group2
        result_df[, c("Interaction", "n1", "n2", "Cohen.d", "ci.lower", "ci.upper", "p.value")]
        
    }, test_name = "cohens_d", context = error_context)
    
    if (!effect_result$success) {
        return(effect_result$error)
    }
    
    effect_result$result
}
