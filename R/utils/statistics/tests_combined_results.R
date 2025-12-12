#' Combined Results Formatting
#'
#' Functions for combining and formatting statistical test results.


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
            # Split by underscore and count differences
            parts_a = list(strsplit(.data$GroupA, "_")[[1]]),
            parts_b = list(strsplit(.data$GroupB, "_")[[1]]),
            n_diffs = sum(.data$parts_a != .data$parts_b)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(.data$n_diffs == 1) %>%
        dplyr::select(-"GroupA", -"GroupB", -"parts_a", -"parts_b", -"n_diffs")
}


# =============================================================================
# Main Function
# =============================================================================

#' Create Combined Results Table
#'
#' Combines lincon and cliff results into a single formatted table.
#' Normalizes interaction strings for consistent matching, applies
#' p-value adjustments, and optionally filters results.
#'
#' @param result_lincon Data frame, results from perform_lincon
#' @param result_cliff Data frame, results from perform_cliff
#' @param measure_col Character, measurement column name
#' @param valid_comparisons Logical, filter to valid comparisons only (multi-factor)
#' @param filter_p_values Logical, filter to significant p-values only (p < 0.07)
#' @param p_adjust_method Character, p-value adjustment method
#' @param x_axis Character vector of grouping columns
#' @param use_scientific Logical, use scientific notation for p-values
#' @return Data frame with combined results or error
create_combined_results <- function(result_lincon, result_cliff, measure_col,
                                    valid_comparisons = TRUE, filter_p_values = FALSE,
                                    p_adjust_method = "bonferroni", x_axis = NULL,
                                    use_scientific = FALSE) {
    
    # Check for errors in input results
    if (is_app_error(result_lincon) || is_app_error(result_cliff)) {
        return(simple_error(
            message = "Cannot create combined results: one or more tests failed.",
            operation_name = "combined_results",
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
        filter_p_values = filter_p_values
    )
    
    # Wrap in safe_execute for error handling
    result <- safe_execute({
        # Normalize interaction strings for consistent matching
        lincon_norm <- normalize_interaction(result_lincon) %>%
            dplyr::select("InteractionKey", "psihat", "p.value") %>%
            dplyr::rename(
                `Lincon: p.hat` = "psihat",
                `Lincon: p.value` = "p.value"
            )
        
        cliff_norm <- normalize_interaction(result_cliff) %>%
            dplyr::select("InteractionKey", "psihat", "p.value") %>%
            dplyr::rename(
                `Cliff: p.hat` = "psihat",
                `Cliff: p.value` = "p.value"
            )
        
        # Merge by InteractionKey
        combined <- dplyr::full_join(lincon_norm, cliff_norm, by = "InteractionKey") %>%
            dplyr::rename(Interaction = "InteractionKey") %>%
            dplyr::select(
                "Interaction",
                "Lincon: p.hat",
                "Lincon: p.value",
                "Cliff: p.hat",
                "Cliff: p.value"
            )
        
        # Filter for valid comparisons if requested (multi-factor designs)
        if (valid_comparisons && !is.null(x_axis) && length(x_axis) > 1) {
            combined <- filter_valid_comparisons(combined, x_axis)
        }
        
        # Apply p-value adjustment (only for non-bootstrap numeric values)
        lincon_p <- combined$`Lincon: p.value`
        cliff_p <- combined$`Cliff: p.value`
        
        # Check if values are numeric (not bootstrap CI strings)
        if (is.numeric(lincon_p)) {
            combined$`Lincon: p.value` <- stats::p.adjust(lincon_p, method = p_adjust_method)
        }
        if (is.numeric(cliff_p)) {
            combined$`Cliff: p.value` <- stats::p.adjust(cliff_p, method = p_adjust_method)
        }
        
        # Filter for significance if requested
        if (filter_p_values) {
            if (is.numeric(combined$`Lincon: p.value`) && is.numeric(combined$`Cliff: p.value`)) {
                combined <- combined %>%
                    dplyr::filter(
                        .data$`Lincon: p.value` < 0.07 | .data$`Cliff: p.value` < 0.07
                    )
            }
        }
        
        # Format output
        if (use_scientific && is.numeric(combined$`Lincon: p.value`)) {
            combined <- combined %>%
                dplyr::mutate(
                    `Lincon: p.value` = formatC(.data$`Lincon: p.value`, format = "e", digits = 2),
                    `Cliff: p.value` = formatC(.data$`Cliff: p.value`, format = "e", digits = 2),
                    `Lincon: p.hat` = round(.data$`Lincon: p.hat`, 4),
                    `Cliff: p.hat` = round(.data$`Cliff: p.hat`, 4)
                )
        } else if (is.numeric(combined$`Lincon: p.value`)) {
            combined <- combined %>%
                dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))
        }
        
        combined
    }, operation_name = "combined_results", context = error_context)
    
    if (!result$success) {
        return(result$error)
    }
    
    result$result
}
