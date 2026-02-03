#' ANOVA Statistical Tests (Welch-Yuen Family)
#'
#' Contains robust ANOVA tests using trimmed means:
#' - One-way (t1way)
#' - Two-way (t2way)
#' - Three-way (t3way)

# Import dplyr for pipe operator and data manipulation functions
box::use(dplyr[...])

# Import required modules
box::use(../statistics_utils)
box::use(../error_handling)


# =============================================================================
# Helper Functions
# =============================================================================

#' Safely convert columns to factors with validation
#'
#' Validates data before factor conversion to prevent errors from invalid data.
#'
#' @param data Data frame containing the columns to convert
#' @param vars Character vector of column names to convert to factors
#' @return List with success flag, data (modified), and optional error message
safe_factor_conversion <- function(data, vars) {
    for (v in vars) {
        if (!v %in% names(data)) {
            return(list(success = FALSE, data = data, 
                        error = paste0("Column '", v, "' not found in data.")))
        }
        
        col_data <- data[[v]]
        
        # Check for all NA values
        if (all(is.na(col_data))) {
            return(list(success = FALSE, data = data,
                        error = paste0("Column '", v, "' contains only NA values.")))
        }
        
        # Check for empty strings only (after removing NA)
        non_na_data <- col_data[!is.na(col_data)]
        if (is.character(non_na_data) && all(trimws(non_na_data) == "")) {
            return(list(success = FALSE, data = data,
                        error = paste0("Column '", v, "' contains only empty strings.")))
        }
        
        # Check for sufficient unique values
        unique_vals <- unique(non_na_data)
        if (length(unique_vals) < 2) {
            return(list(success = FALSE, data = data,
                        error = paste0("Column '", v, "' has fewer than 2 unique values (found ", 
                                      length(unique_vals), ").")))
        }
        
        # Safe to convert
        data[[v]] <- as.factor(data[[v]])
    }
    
    list(success = TRUE, data = data, error = NULL)
}


# =============================================================================
# Generic Robust ANOVA Runner
# =============================================================================

#' Setup bootstrap parameters
#' @param df Data frame
#' @param x_axis Grouping columns
#' @param use_bootstrap Logical
#' @param boot_samples Number of bootstrap samples
#' @param boot_sample_size Sample size per group (or NULL)
#' @return List with n_iterations and sample_size
setup_bootstrap_params <- function(df, x_axis, use_bootstrap, boot_samples, boot_sample_size) {
    if (use_bootstrap) {
        smallest_group <- calculate_smallest_group(df, x_axis)
        sample_size <- if (!is.null(boot_sample_size) && !is.na(boot_sample_size)) {
            min(boot_sample_size, smallest_group)
        } else {
            smallest_group
        }
        list(n_iterations = boot_samples, sample_size = sample_size)
    } else {
        list(n_iterations = 1, sample_size = NULL)
    }
}

#' Sample data for a bootstrap iteration
#' @param df Data frame
#' @param x_axis Grouping columns
#' @param use_bootstrap Logical
#' @param sample_size Sample size per group
#' @return Sampled data frame
sample_for_iteration <- function(df, x_axis, use_bootstrap, sample_size) {
    if (use_bootstrap) {
        df %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(x_axis))) %>%
            dplyr::slice_sample(n = sample_size, replace = TRUE) %>%
            dplyr::ungroup()
    } else {
        df
    }
}

#' Generic robust ANOVA runner
#'
#' @param df Data frame
#' @param x_axis Grouping columns
#' @param measure_col Measurement column
#' @param tr_value Trim proportion
#' @param use_bootstrap Logical
#' @param boot_samples Number of bootstrap samples
#' @param boot_sample_size Sample size per group
#' @param config List with test configuration (validate, build_context, result_cols,
#'               build_formula, run_test, extract_results, format_results)
#' @return Data frame with results or error
run_robust_anova <- function(df, x_axis, measure_col, tr_value,
                              use_bootstrap, boot_samples, boot_sample_size,
                              config) {
    # 1. Validate inputs
    validation_error <- config$validate(df, x_axis)
    if (!is.null(validation_error)) return(validation_error)
    
    # 2. Setup bootstrap parameters
    boot_params <- setup_bootstrap_params(df, x_axis, use_bootstrap, boot_samples, boot_sample_size)
    
    # 3. Build error context
    error_context <- config$build_context(df, x_axis, measure_col, tr_value, use_bootstrap)
    
    # 4. Run the test iterations
    test_result <- statistics_utils$safe_stat_test({
        # Initialize results storage
        results_matrix <- as.data.frame(
            matrix(NA_real_, nrow = boot_params$n_iterations, ncol = length(config$result_cols))
        )
        names(results_matrix) <- config$result_cols
        
        for (i in seq_len(boot_params$n_iterations)) {
            sample_data <- sample_for_iteration(df, x_axis, use_bootstrap, boot_params$sample_size)
            formula_obj <- config$build_formula(measure_col, x_axis)
            test_out <- config$run_test(formula_obj, sample_data, tr_value)
            results_matrix[i, ] <- config$extract_results(test_out)
        }
        
        results_matrix
    }, test_name = config$name, context = error_context)
    
    # 5. Handle errors
    if (!test_result$success) return(test_result$error)
    
    # 6. Format results
    config$format_results(test_result$result, x_axis, use_bootstrap)
}


# =============================================================================
# t1way Configuration and Function
# =============================================================================

#' t1way test configuration
t1way_config <- list(
    name = "t1way",
    
    result_cols = c("F_statistic", "df1", "df2", "Effect_Size", "p_value"),
    
    validate = function(df, x_axis) {
        if (length(x_axis) != 1) {
            return(data.frame(Error = "t1way requires exactly one grouping variable.",
                              stringsAsFactors = FALSE))
        }
        n_groups <- length(unique(df[[x_axis[1]]]))
        if (n_groups < 2) {
            return(data.frame(Error = paste0("t1way requires at least 2 groups, found ", n_groups, "."),
                              stringsAsFactors = FALSE))
        }
        NULL
    },
    
    build_context = function(df, x_axis, measure_col, tr_value, use_bootstrap) {
        list(
            measure = measure_col,
            grouping = x_axis[1],
            n_groups = length(unique(df[[x_axis[1]]])),
            n_observations = nrow(df),
            trim = tr_value,
            bootstrap = use_bootstrap
        )
    },
    
    build_formula = function(measure_col, x_axis) {
        stats::as.formula(paste0("`", measure_col, "` ~ `", x_axis[1], "`"))
    },
    
    run_test = function(formula_obj, data, tr_value) {
        # t1way requires factors - convert grouping variable with validation
        vars <- all.vars(formula_obj)[-1]  # exclude response variable
        conversion <- safe_factor_conversion(data, vars)
        if (!conversion$success) {
            stop(conversion$error)
        }
        WRS2::t1way(formula = formula_obj, data = conversion$data, tr = tr_value)
    },
    
    extract_results = function(out) {
        c(out$test, out$df1, out$df2, out$effsize, out$p.value)
    },
    
    format_results = function(results, x_axis, use_bootstrap) {
        if (use_bootstrap) {
            format_bootstrap_results(results)
        } else {
            results[] <- lapply(results, function(x) signif(x, 3))
            results
        }
    }
)

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
    run_robust_anova(df, x_axis, measure_col, tr_value,
                     use_bootstrap, boot_samples, boot_sample_size,
                     t1way_config)
}


# =============================================================================
# t2way Configuration and Function
# =============================================================================

#' t2way test configuration
t2way_config <- list(
    name = "t2way",
    
    result_cols = c("Qa", "Qb", "Qab", "A.p.value", "B.p.value", "AB.p.value"),
    
    validate = function(df, x_axis) {
        if (length(x_axis) != 2) {
            return(data.frame(Error = "t2way requires exactly two grouping variables.",
                              stringsAsFactors = FALSE))
        }
        n_levels_1 <- length(unique(df[[x_axis[1]]]))
        n_levels_2 <- length(unique(df[[x_axis[2]]]))
        if (n_levels_1 < 2) {
            return(data.frame(Error = paste0("t2way requires at least 2 levels in '", x_axis[1],
                                             "', found ", n_levels_1, "."),
                              stringsAsFactors = FALSE))
        }
        if (n_levels_2 < 2) {
            return(data.frame(Error = paste0("t2way requires at least 2 levels in '", x_axis[2],
                                             "', found ", n_levels_2, "."),
                              stringsAsFactors = FALSE))
        }
        NULL
    },
    
    build_context = function(df, x_axis, measure_col, tr_value, use_bootstrap) {
        list(
            measure = measure_col,
            factor1 = x_axis[1],
            factor2 = x_axis[2],
            levels_factor1 = length(unique(df[[x_axis[1]]])),
            levels_factor2 = length(unique(df[[x_axis[2]]])),
            n_observations = nrow(df),
            trim = tr_value,
            bootstrap = use_bootstrap
        )
    },
    
    build_formula = function(measure_col, x_axis) {
        stats::as.formula(paste0("`", measure_col, "` ~ `", x_axis[1], "` * `", x_axis[2], "`"))
    },
    
    run_test = function(formula_obj, data, tr_value) {
        # t2way requires factors - convert grouping variables with validation
        vars <- all.vars(formula_obj)[-1]  # exclude response variable
        conversion <- safe_factor_conversion(data, vars)
        if (!conversion$success) {
            stop(conversion$error)
        }
        WRS2::t2way(formula = formula_obj, data = conversion$data, tr = tr_value)
    },
    
    extract_results = function(out) {
        c(out$Qa, out$Qb, out$Qab, out$A.p.value, out$B.p.value, out$AB.p.value)
    },
    
    format_results = function(results, x_axis, use_bootstrap) {
        effect_labels <- c(x_axis[1], x_axis[2], paste0(x_axis[1], ":", x_axis[2]))
        
        if (use_bootstrap) {
            ci_bounds <- apply(results, 2, function(x) {
                stats::quantile(x, c(0.025, 0.975), na.rm = TRUE)
            })
            
            data.frame(
                Effect = effect_labels,
                Q.Statistic = c(
                    paste0(signif(stats::mean(results$Qa, na.rm = TRUE), 3), " [",
                           signif(ci_bounds[1, "Qa"], 3), " - ", signif(ci_bounds[2, "Qa"], 3), "]"),
                    paste0(signif(stats::mean(results$Qb, na.rm = TRUE), 3), " [",
                           signif(ci_bounds[1, "Qb"], 3), " - ", signif(ci_bounds[2, "Qb"], 3), "]"),
                    paste0(signif(stats::mean(results$Qab, na.rm = TRUE), 3), " [",
                           signif(ci_bounds[1, "Qab"], 3), " - ", signif(ci_bounds[2, "Qab"], 3), "]")
                ),
                p.value = c(
                    paste0(signif(stats::mean(results$A.p.value, na.rm = TRUE), 3), " [",
                           signif(ci_bounds[1, "A.p.value"], 3), " - ", signif(ci_bounds[2, "A.p.value"], 3), "]"),
                    paste0(signif(stats::mean(results$B.p.value, na.rm = TRUE), 3), " [",
                           signif(ci_bounds[1, "B.p.value"], 3), " - ", signif(ci_bounds[2, "B.p.value"], 3), "]"),
                    paste0(signif(stats::mean(results$AB.p.value, na.rm = TRUE), 3), " [",
                           signif(ci_bounds[1, "AB.p.value"], 3), " - ", signif(ci_bounds[2, "AB.p.value"], 3), "]")
                ),
                stringsAsFactors = FALSE
            )
        } else {
            data.frame(
                Effect = effect_labels,
                Q.Statistic = signif(c(results$Qa[1], results$Qb[1], results$Qab[1]), 3),
                p.value = signif(c(results$A.p.value[1], results$B.p.value[1], results$AB.p.value[1]), 3),
                stringsAsFactors = FALSE
            )
        }
    }
)

#' Perform Two-Way Robust ANOVA (Welch-Yuen t2way)
#'
#' Returns main effects (A, B) and interaction (AB) with Q statistics and p-values.
#'
#' @inheritParams perform_t1way
#' @return Data frame with test results or error data frame
perform_t2way <- function(df, x_axis, measure_col, tr_value,
                          use_bootstrap = FALSE, boot_samples = 599,
                          boot_sample_size = NULL, p_adjust_method = "bonferroni") {
    run_robust_anova(df, x_axis, measure_col, tr_value,
                     use_bootstrap, boot_samples, boot_sample_size,
                     t2way_config)
}


# =============================================================================
# t3way Configuration and Function
# =============================================================================

#' t3way test configuration
t3way_config <- list(
    name = "t3way",
    
    result_cols = c("Qa", "Qb", "Qc", "Qab", "Qac", "Qbc", "Qabc",
                    "A.p.value", "B.p.value", "C.p.value", 
                    "AB.p.value", "AC.p.value", "BC.p.value", "ABC.p.value"),
    
    validate = function(df, x_axis) {
        if (length(x_axis) != 3) {
            return(data.frame(Error = "t3way requires exactly three grouping variables.",
                              stringsAsFactors = FALSE))
        }
        for (i in 1:3) {
            n_levels <- length(unique(df[[x_axis[i]]]))
            if (n_levels < 2) {
                return(data.frame(Error = paste0("t3way requires at least 2 levels in '", x_axis[i],
                                                 "', found ", n_levels, "."),
                                  stringsAsFactors = FALSE))
            }
        }
        NULL
    },
    
    build_context = function(df, x_axis, measure_col, tr_value, use_bootstrap) {
        list(
            measure = measure_col,
            factor1 = x_axis[1],
            factor2 = x_axis[2],
            factor3 = x_axis[3],
            levels_factor1 = length(unique(df[[x_axis[1]]])),
            levels_factor2 = length(unique(df[[x_axis[2]]])),
            levels_factor3 = length(unique(df[[x_axis[3]]])),
            n_observations = nrow(df),
            trim = tr_value,
            bootstrap = use_bootstrap
        )
    },
    
    build_formula = function(measure_col, x_axis) {
        stats::as.formula(paste0("`", measure_col, "` ~ `", x_axis[1], "` * `", x_axis[2], "` * `", x_axis[3], "`"))
    },
    
    run_test = function(formula_obj, data, tr_value) {
        # t3way requires factors - convert grouping variables with validation
        vars <- all.vars(formula_obj)[-1]  # exclude response variable
        conversion <- safe_factor_conversion(data, vars)
        if (!conversion$success) {
            stop(conversion$error)
        }
        WRS2::t3way(formula = formula_obj, data = conversion$data, tr = tr_value)
    },
    
    extract_results = function(out) {
        c(out$Qa, out$Qb, out$Qc, out$Qab, out$Qac, out$Qbc, out$Qabc,
          out$A.p.value, out$B.p.value, out$C.p.value,
          out$AB.p.value, out$AC.p.value, out$BC.p.value, out$ABC.p.value)
    },
    
    format_results = function(results, x_axis, use_bootstrap) {
        # Generate effect labels: A, B, C, A:B, A:C, B:C, A:B:C
        effect_labels <- c(
            x_axis[1], x_axis[2], x_axis[3],
            paste0(x_axis[1], ":", x_axis[2]),
            paste0(x_axis[1], ":", x_axis[3]),
            paste0(x_axis[2], ":", x_axis[3]),
            paste0(x_axis[1], ":", x_axis[2], ":", x_axis[3])
        )
        
        q_cols <- c("Qa", "Qb", "Qc", "Qab", "Qac", "Qbc", "Qabc")
        p_cols <- c("A.p.value", "B.p.value", "C.p.value", 
                    "AB.p.value", "AC.p.value", "BC.p.value", "ABC.p.value")
        
        if (use_bootstrap) {
            ci_bounds <- apply(results, 2, function(x) {
                stats::quantile(x, c(0.025, 0.975), na.rm = TRUE)
            })
            
            q_stats <- sapply(q_cols, function(col) {
                paste0(signif(stats::mean(results[[col]], na.rm = TRUE), 3), " [",
                       signif(ci_bounds[1, col], 3), " - ", signif(ci_bounds[2, col], 3), "]")
            })
            
            p_vals <- sapply(p_cols, function(col) {
                paste0(signif(stats::mean(results[[col]], na.rm = TRUE), 3), " [",
                       signif(ci_bounds[1, col], 3), " - ", signif(ci_bounds[2, col], 3), "]")
            })
            
            data.frame(
                Effect = effect_labels,
                Q.Statistic = q_stats,
                p.value = p_vals,
                stringsAsFactors = FALSE
            )
        } else {
            data.frame(
                Effect = effect_labels,
                Q.Statistic = signif(c(results$Qa[1], results$Qb[1], results$Qc[1],
                                       results$Qab[1], results$Qac[1], results$Qbc[1],
                                       results$Qabc[1]), 3),
                p.value = signif(c(results$A.p.value[1], results$B.p.value[1], results$C.p.value[1],
                                   results$AB.p.value[1], results$AC.p.value[1], results$BC.p.value[1],
                                   results$ABC.p.value[1]), 3),
                stringsAsFactors = FALSE
            )
        }
    }
)

#' Perform Three-Way Robust ANOVA (Welch-Yuen t3way)
#'
#' Returns main effects (A, B, C), two-way interactions (AB, AC, BC), 
#' and three-way interaction (ABC) with Q statistics and p-values.
#'
#' @inheritParams perform_t1way
#' @return Data frame with test results or error data frame
perform_t3way <- function(df, x_axis, measure_col, tr_value,
                          use_bootstrap = FALSE, boot_samples = 599,
                          boot_sample_size = NULL, p_adjust_method = "bonferroni") {
    run_robust_anova(df, x_axis, measure_col, tr_value,
                     use_bootstrap, boot_samples, boot_sample_size,
                     t3way_config)
}
