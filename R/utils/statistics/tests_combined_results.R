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
# Combined Results Functions
# =============================================================================

#' Create Combined Results Table
#'
#' Generic function to merge two statistical results tables by a common key column.
#' Designed to work with TukeyHSD and Cohen's d results, but flexible enough for
#' other statistical table combinations.
#'
#' @param df1 First data frame (e.g., TukeyHSD results)
#' @param df2 Second data frame (e.g., Cohen's d results)
#' @param df1ColNames Character vector of column names to select from df1
#' @param df2ColNames Character vector of column names to select from df2
#' @param merge_key Character, column name to merge on (default: "Interaction")
#' @param x_axis Character vector of grouping columns (for valid comparisons filtering)
#' @param filter_valid Logical, whether to filter for valid comparisons in multi-factor designs
#' @param p_adjust_method Character, p-value adjustment method (default: "bonferroni")
#' @param use_scientific Logical, use scientific notation for numeric output (default: FALSE)
#' @return Merged data frame with selected columns from both tables
create_combined_results <- function(df1, df2, df1ColNames, df2ColNames, 
                                   merge_key = "Interaction", x_axis = NULL,
                                   filter_valid = FALSE, p_adjust_method = "bonferroni",
                                   use_scientific = FALSE) {
    
    # Validate inputs
    if (!merge_key %in% names(df1) || !merge_key %in% names(df2)) {
        stop("Merge key '", merge_key, "' not found in both data frames")
    }
    
    if (!all(df1ColNames %in% names(df1))) {
        missing_cols <- setdiff(df1ColNames, names(df1))
        stop("Columns not found in df1: ", paste(missing_cols, collapse = ", "))
    }
    
    if (!all(df2ColNames %in% names(df2))) {
        missing_cols <- setdiff(df2ColNames, names(df2))
        stop("Columns not found in df2: ", paste(missing_cols, collapse = ", "))
    }
    
    # Create copies to avoid modifying originals
    df1_processed <- df1
    df2_processed <- df2
    
    # Normalize interaction keys for consistent matching
    if (merge_key == "Interaction") {
        df1_processed <- normalize_interaction(df1_processed)
        df2_processed <- normalize_interaction(df2_processed)
    }
    
    # Filter for valid comparisons if requested (multi-factor designs)
    if (filter_valid && !is.null(x_axis) && length(x_axis) > 1) {
        df1_processed <- filter_valid_comparisons(df1_processed, x_axis)
        df2_processed <- filter_valid_comparisons(df2_processed, x_axis)
    }
    
    # Select specified columns (exclude merge_key from df2ColNames to avoid duplicates)
    df1_selected <- df1_processed[, c(merge_key, df1ColNames), drop = FALSE]
    df2_cols_for_merge <- setdiff(df2ColNames, merge_key)  # Remove merge_key to avoid duplicates
    df2_selected <- df2_processed[, c(merge_key, df2_cols_for_merge), drop = FALSE]
    
    # Determine merge key based on whether we normalized interactions
    actual_merge_key <- if (merge_key == "Interaction") "InteractionKey" else merge_key
    
    # Merge the data frames
    merged_df <- dplyr::inner_join(df1_selected, df2_selected, 
                                 by = actual_merge_key, 
                                 suffix = c("_df1", "_df2"))
    
    # Remove the merge key column if it's InteractionKey (keep original Interaction)
    if (actual_merge_key == "InteractionKey") {
        # Keep the original Interaction column from df1
        if ("Interaction_df1" %in% names(merged_df)) {
            merged_df$Interaction <- merged_df$Interaction_df1
            merged_df <- merged_df[, !names(merged_df) %in% c("InteractionKey", "Interaction_df1", "Interaction_df2")]
        }
    }
    
    # Compute adjusted p-values if P_Value_Raw is present
    if ("P_Value_Raw" %in% names(merged_df)) {
        merged_df$P_Value_Adjusted <- stats::p.adjust(merged_df$P_Value_Raw, method = p_adjust_method)
    }
    
    # Reorder columns to put Interaction first, then df1 columns, then df2 columns
    interaction_col <- if ("Interaction" %in% names(merged_df)) "Interaction" else actual_merge_key
    other_cols <- setdiff(names(merged_df), interaction_col)
    
    # Put df1 columns first, then df2 columns (excluding the merge key)
    df1_other_cols <- setdiff(df1ColNames, merge_key)
    df2_other_cols <- df2_cols_for_merge  # Already excludes merge_key
    
    final_order <- c(interaction_col, 
                    intersect(names(merged_df), df1_other_cols),
                    intersect(names(merged_df), df2_other_cols),
                    setdiff(other_cols, c(df1_other_cols, df2_other_cols)))
    
    merged_df <- merged_df[, final_order, drop = FALSE]
    
    # Apply scientific notation formatting if requested
    if (use_scientific) {
        old_scipen <- getOption("scipen")
        on.exit(options(scipen = old_scipen))
        options(scipen = 0)
        
        # Apply scientific notation to numeric columns
        numeric_cols <- sapply(merged_df, is.numeric)
        merged_df[numeric_cols] <- lapply(merged_df[numeric_cols], function(x) signif(x, 3))
    }
    
    return(merged_df)
}

