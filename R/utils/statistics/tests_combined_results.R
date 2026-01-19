#' Combined Results Tables for Statistical Tests
#'
#' This module provides functions to create combined results tables for both
#' robust and parametric statistical test approaches. It handles the different
#' column naming conventions and p-value adjustment logic for each approach.


# =============================================================================
# Helper Functions
# =============================================================================

#' Normalize interaction strings for consistent matching
#'
#' Extracts group names from "GroupA vs. GroupB" format and creates
#' an alphabetized key for consistent matching between tests.
#'
#' @param df Data frame with Interaction column
#' @return Data frame with added InteractionKey column
normalize_interaction <- function(df) {
    if (!"Interaction" %in% names(df)) {
        return(df)
    }
    
    df %>%
        dplyr::mutate(
            GroupA = stringr::str_trim(stringr::str_extract(.data$Interaction, "^(.*?)\\s+vs\\.\\s+(.*)$", group = 1)),
            GroupB = stringr::str_trim(stringr::str_extract(.data$Interaction, "^(.*?)\\s+vs\\.\\s+(.*)$", group = 2))
        ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            InteractionKey = paste(sort(c(.data$GroupA, .data$GroupB)), collapse = " vs. ")
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-"GroupA", -"GroupB")
}

#' Filter for valid comparisons in multi-factor designs
#'
#' For multi-factor designs, filters to comparisons where groups differ
#' by only one factor level (e.g., "A_X vs. A_Y" but not "A_X vs. B_Y").
#'
#' @param df Data frame with Interaction column
#' @param x_axis Character vector of grouping columns
#' @return Filtered data frame
filter_valid_comparisons <- function(df, x_axis) {
    if (is.null(x_axis) || length(x_axis) <= 1) {
        return(df)
    }
    
    # For multi-factor designs, keep only comparisons where groups differ by one factor
    df %>%
        dplyr::mutate(
            GroupA = stringr::str_trim(stringr::str_extract(.data$Interaction, "^(.*?)\\s+vs\\.\\s+(.*)$", group = 1)),
            GroupB = stringr::str_trim(stringr::str_extract(.data$Interaction, "^(.*?)\\s+vs\\.\\s+(.*)$", group = 2))
        ) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            # Split by dot and count differences
            parts_a = list(strsplit(.data$GroupA, "\\.")[[1]]),
            parts_b = list(strsplit(.data$GroupB, "\\.")[[1]]),
            n_diffs = sum(.data$parts_a != .data$parts_b)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(.data$n_diffs == 1) %>%
        dplyr::select(-"GroupA", -"GroupB", -"parts_a", -"parts_b", -"n_diffs")
}


# =============================================================================
# Main Dispatcher Function
# =============================================================================

#' Create Combined Results Table (Unified)
#'
#' Main dispatcher that detects the test approach and calls the appropriate
#' combined results function.
#'
#' @param test_approach Character, either "robust" or "parametric"
#' @param result_posthoc Data frame, post-hoc test results (lincon or tukey)
#' @param result_effect Data frame, effect size results (cliff or cohen)
#' @param measure_col Character, measurement column name
#' @param valid_comparisons Logical, filter to valid comparisons only
#' @param filter_p_values Logical, filter to significant p-values only
#' @param p_adjust_method Character, p-value adjustment method
#' @param x_axis Character vector of grouping columns
#' @param use_scientific Logical, use scientific notation for p-values
#' @return Data frame with combined results or error
create_unified_combined_results <- function(test_approach, result_posthoc, result_effect,
                                          measure_col, valid_comparisons = TRUE,
                                          filter_p_values = FALSE, p_adjust_method = "bonferroni",
                                          x_axis = NULL, use_scientific = FALSE) {
    
    if (test_approach == "robust") {
        create_robust_combined_results(
            result_lincon = result_posthoc,
            result_cliff = result_effect,
            measure_col = measure_col,
            valid_comparisons = valid_comparisons,
            filter_p_values = filter_p_values,
            p_adjust_method = p_adjust_method,
            x_axis = x_axis,
            use_scientific = use_scientific
        )
    } else if (test_approach == "parametric") {
        create_parametric_combined_results(
            result_tukey = result_posthoc,
            result_cohen = result_effect,
            measure_col = measure_col,
            valid_comparisons = valid_comparisons,
            filter_p_values = filter_p_values,
            p_adjust_method = p_adjust_method,
            x_axis = x_axis,
            use_scientific = use_scientific
        )
    } else {
        simple_error(
            message = sprintf("Unknown test approach: '%s'. Expected 'robust' or 'parametric'.", test_approach),
            operation_name = "unified_combined_results",
            context = list(test_approach = test_approach)
        )
    }
}


# =============================================================================
# Robust Combined Results
# =============================================================================

#' Create Combined Results Table for Robust Tests
#'
#' Combines lincon and cliff results with specific column naming for robust tests.
#' Column naming: Lincon: p.hat, Lincon: p.raw, Lincon: adj.p.value, 
#'                Cliff: p.hat, Cliff: p.raw, Cliff: adj.p.value
#'
#' @inheritParams create_unified_combined_results
#' @param result_lincon Data frame, results from perform_lincon
#' @param result_cliff Data frame, results from perform_cliff
#' @return Data frame with combined results or error
create_robust_combined_results <- function(result_lincon, result_cliff, measure_col,
                                         valid_comparisons = TRUE, filter_p_values = FALSE,
                                         p_adjust_method = "bonferroni", x_axis = NULL,
                                         use_scientific = FALSE) {
    
    # Check for errors in input results
    if (is_app_error(result_lincon) || is_app_error(result_cliff)) {
        return(simple_error(
            message = "Cannot create combined results: one or more tests failed.",
            operation_name = "robust_combined_results",
            context = list(
                lincon_error = is_app_error(result_lincon),
                cliff_error = is_app_error(result_cliff)
            )
        ))
    }
    
    # Check for error data frames
    if (is.data.frame(result_lincon) && "Error" %in% names(result_lincon)) {
        return(result_lincon)
    }
    if (is.data.frame(result_cliff) && "Error" %in% names(result_cliff)) {
        return(result_cliff)
    }
    
    # Handle empty results
    if (!is.data.frame(result_lincon) || nrow(result_lincon) == 0 ||
        !is.data.frame(result_cliff) || nrow(result_cliff) == 0) {
        return(data.frame(
            Error = "No pairwise comparison results available.",
            stringsAsFactors = FALSE
        ))
    }
    
    # Build error context
    error_context <- list(
        measure = measure_col,
        n_lincon = nrow(result_lincon),
        n_cliff = nrow(result_cliff),
        valid_comparisons = valid_comparisons,
        filter_p_values = filter_p_values,
        test_approach = "robust"
    )
    
    # Wrap in safe_execute for error handling
    result <- safe_execute({
        # Normalize interaction strings for consistent matching
        lincon_norm <- normalize_interaction(result_lincon) %>%
            dplyr::select("InteractionKey", "psihat", "p.value", "p.adjusted") %>%
            dplyr::rename(
                `Lincon: p.hat` = "psihat",
                `Lincon: p.raw` = "p.value",
                `Lincon: adj.p.value` = "p.adjusted"
            )
        
        cliff_norm <- normalize_interaction(result_cliff) %>%
            dplyr::select("InteractionKey", "psihat", "p.value") %>%
            dplyr::rename(
                `Cliff: p.hat` = "psihat",
                `Cliff: p.raw` = "p.value"
            )
        
        # Merge by InteractionKey
        combined <- dplyr::full_join(lincon_norm, cliff_norm, by = "InteractionKey") %>%
            dplyr::rename(Interaction = "InteractionKey") %>%
            dplyr::select(
                "Interaction",
                "Lincon: p.hat",
                "Lincon: p.raw",
                "Lincon: adj.p.value",
                "Cliff: p.hat",
                "Cliff: p.raw"
            )
        
        # Filter for valid comparisons if requested (multi-factor designs)
        if (valid_comparisons && !is.null(x_axis) && length(x_axis) > 1) {
            combined <- filter_valid_comparisons(combined, x_axis)
        }
        
        # Apply p-value adjustment for Cliff (Lincon already has adjusted p-values)
        cliff_p_raw <- combined$`Cliff: p.raw`
        
        # Check if values are numeric (not bootstrap CI strings)
        if (is.numeric(cliff_p_raw)) {
            combined$`Cliff: adj.p.value` <- stats::p.adjust(cliff_p_raw, method = p_adjust_method)
        } else {
            # For bootstrap results, p-values might be strings
            combined$`Cliff: adj.p.value` <- cliff_p_raw
        }
        
        # Reorder columns to have adj.p.value at the end for each test
        combined <- combined %>%
            dplyr::select(
                "Interaction",
                "Lincon: p.hat",
                "Lincon: p.raw",
                "Lincon: adj.p.value",
                "Cliff: p.hat",
                "Cliff: p.raw",
                "Cliff: adj.p.value"
            )
        
        # Filter for significance if requested
        if (filter_p_values) {
            if (is.numeric(combined$`Lincon: adj.p.value`) && is.numeric(combined$`Cliff: adj.p.value`)) {
                combined <- combined %>%
                    dplyr::filter(
                        .data$`Lincon: adj.p.value` < 0.07 | .data$`Cliff: adj.p.value` < 0.07
                    )
            }
        }
        
        # Format output
        if (use_scientific && is.numeric(combined$`Lincon: p.raw`)) {
            combined <- combined %>%
                dplyr::mutate(
                    `Lincon: p.raw` = formatC(.data$`Lincon: p.raw`, format = "e", digits = 2),
                    `Lincon: adj.p.value` = formatC(.data$`Lincon: adj.p.value`, format = "e", digits = 2),
                    `Cliff: p.raw` = formatC(.data$`Cliff: p.raw`, format = "e", digits = 2),
                    `Cliff: adj.p.value` = formatC(.data$`Cliff: adj.p.value`, format = "e", digits = 2),
                    `Lincon: p.hat` = round(.data$`Lincon: p.hat`, 4),
                    `Cliff: p.hat` = round(.data$`Cliff: p.hat`, 4)
                )
        } else if (is.numeric(combined$`Lincon: p.raw`)) {
            combined <- combined %>%
                dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))
        }
        
        combined
    }, operation_name = "robust_combined_results", context = error_context)
    
    if (!result$success) {
        return(result$error)
    }
    
    result$result
}


# =============================================================================
# Parametric Combined Results
# =============================================================================

#' Create Combined Results Table for Parametric Tests
#'
#' Combines Tukey HSD and Cohen's d results with specific column naming for parametric tests.
#' Column naming: Tukey: posthoc, Tukey: p.raw, Tukey: adj.p.value,
#'                Cohen: effect, Cohen: p.raw, Cohen: adj.p.value
#'
#' @inheritParams create_unified_combined_results
#' @param result_tukey Data frame, results from perform_tukey_hsd
#' @param result_cohen Data frame, results from perform_cohens_d
#' @return Data frame with combined results or error
create_parametric_combined_results <- function(result_tukey, result_cohen, measure_col,
                                             valid_comparisons = TRUE, filter_p_values = FALSE,
                                             p_adjust_method = "bonferroni", x_axis = NULL,
                                             use_scientific = FALSE) {
    
    # Check for errors in input results
    if (is_app_error(result_tukey) || is_app_error(result_cohen)) {
        return(simple_error(
            message = "Cannot create combined results: one or more tests failed.",
            operation_name = "parametric_combined_results",
            context = list(
                tukey_error = is_app_error(result_tukey),
                cohen_error = is_app_error(result_cohen)
            )
        ))
    }
    
    # Check for error data frames
    if (is.data.frame(result_tukey) && "Error" %in% names(result_tukey)) {
        return(result_tukey)
    }
    if (is.data.frame(result_cohen) && "Error" %in% names(result_cohen)) {
        return(result_cohen)
    }
    
    # Handle empty results
    if (!is.data.frame(result_tukey) || nrow(result_tukey) == 0 ||
        !is.data.frame(result_cohen) || nrow(result_cohen) == 0) {
        return(data.frame(
            Error = "No pairwise comparison results available.",
            stringsAsFactors = FALSE
        ))
    }
    
    # Build error context
    error_context <- list(
        measure = measure_col,
        n_tukey = nrow(result_tukey),
        n_cohen = nrow(result_cohen),
        valid_comparisons = valid_comparisons,
        filter_p_values = filter_p_values,
        test_approach = "parametric"
    )
    
    # Wrap in safe_execute for error handling
    result <- safe_execute({
        # Normalize interaction strings for consistent matching
        tukey_norm <- normalize_interaction(result_tukey) %>%
            dplyr::select("InteractionKey", "psihat", "p.value") %>%
            dplyr::rename(
                `Tukey: posthoc` = "psihat",  # Mean difference from Tukey
                `Tukey: p.raw` = "p.value"     # Already adjusted by Tukey
            )
        
        cohen_norm <- normalize_interaction(result_cohen) %>%
            dplyr::select("InteractionKey", "psihat", "p.value") %>%
            dplyr::rename(
                `Cohen: effect` = "psihat",    # Cohen's d effect size
                `Cohen: p.raw` = "p.value"
            )
        
        # Merge by InteractionKey
        combined <- dplyr::full_join(tukey_norm, cohen_norm, by = "InteractionKey") %>%
            dplyr::rename(Interaction = "InteractionKey")
        
        # Filter for valid comparisons if requested (multi-factor designs)
        if (valid_comparisons && !is.null(x_axis) && length(x_axis) > 1) {
            combined <- filter_valid_comparisons(combined, x_axis)
        }
        
        # Add adjusted p-values
        # Tukey p-values are already adjusted, so adj = raw
        combined$`Tukey: adj.p.value` <- combined$`Tukey: p.raw`
        
        # Apply p-value adjustment for Cohen's d
        cohen_p_raw <- combined$`Cohen: p.raw`
        if (is.numeric(cohen_p_raw)) {
            combined$`Cohen: adj.p.value` <- stats::p.adjust(cohen_p_raw, method = p_adjust_method)
        } else {
            combined$`Cohen: adj.p.value` <- cohen_p_raw
        }
        
        # Select and order columns
        combined <- combined %>%
            dplyr::select(
                "Interaction",
                "Tukey: posthoc",
                "Tukey: p.raw",
                "Tukey: adj.p.value",
                "Cohen: effect",
                "Cohen: p.raw",
                "Cohen: adj.p.value"
            )
        
        # Filter for significance if requested
        if (filter_p_values) {
            if (is.numeric(combined$`Tukey: adj.p.value`) && is.numeric(combined$`Cohen: adj.p.value`)) {
                combined <- combined %>%
                    dplyr::filter(
                        .data$`Tukey: adj.p.value` < 0.07 | .data$`Cohen: adj.p.value` < 0.07
                    )
            }
        }
        
        # Format output
        if (use_scientific && is.numeric(combined$`Tukey: p.raw`)) {
            combined <- combined %>%
                dplyr::mutate(
                    `Tukey: p.raw` = formatC(.data$`Tukey: p.raw`, format = "e", digits = 2),
                    `Tukey: adj.p.value` = formatC(.data$`Tukey: adj.p.value`, format = "e", digits = 2),
                    `Cohen: p.raw` = formatC(.data$`Cohen: p.raw`, format = "e", digits = 2),
                    `Cohen: adj.p.value` = formatC(.data$`Cohen: adj.p.value`, format = "e", digits = 2),
                    `Tukey: posthoc` = round(.data$`Tukey: posthoc`, 4),
                    `Cohen: effect` = round(.data$`Cohen: effect`, 4)
                )
        } else if (is.numeric(combined$`Tukey: p.raw`)) {
            combined <- combined %>%
                dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))
        }
        
        combined
    }, operation_name = "parametric_combined_results", context = error_context)
    
    if (!result$success) {
        return(result$error)
    }
    
    result$result
}
