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
    
    # Check if either input is a structured error object
    if (is_app_error(df1)) {
        return(df1)
    }
    if (is_app_error(df2)) {
        return(df2)
    }
    
    # Validate that inputs are data frames
    if (!is.data.frame(df1) || !is.data.frame(df2)) {
        return(simple_error(
            message = "Invalid input: both df1 and df2 must be data frames.",
            operation_name = "create_combined_results",
            context = list(
                df1_type = class(df1),
                df2_type = class(df2)
            )
        ))
    }
    
    # Check if either dataframe is an error dataframe (has 'Error' column)
    if ("Error" %in% names(df1) || "Error" %in% names(df2)) {
        # Return the error from whichever dataframe has it, prioritizing df1
        if ("Error" %in% names(df1)) {
            return(df1)
        } else {
            return(df2)
        }
    }
    
    # Check if either dataframe is empty or has no rows
    if (nrow(df1) == 0 || nrow(df2) == 0) {
        # Return a meaningful error or empty result
        return(data.frame(
            Error = "Unable to create combined results: one or both input tables are empty.",
            stringsAsFactors = FALSE
        ))
    }
    
    # Validate inputs
    if (!merge_key %in% names(df1) || !merge_key %in% names(df2)) {
        return(data.frame(
            Error = paste("Merge key '", merge_key, "' not found in both data frames"),
            stringsAsFactors = FALSE
        ))
    }
    
    # Check for missing columns and adjust expectations
    missing_df1_cols <- setdiff(df1ColNames, names(df1))
    missing_df2_cols <- setdiff(df2ColNames, names(df2))
    
    if (length(missing_df1_cols) > 0) {
        # Adjust df1ColNames to only include existing columns
        df1ColNames <- intersect(df1ColNames, names(df1))
        if (length(df1ColNames) <= 1) {  # Only merge_key might remain
            return(data.frame(
                Error = paste("Insufficient valid columns in df1. Missing:", paste(missing_df1_cols, collapse = ", ")),
                stringsAsFactors = FALSE
            ))
        }
    }
    
    if (length(missing_df2_cols) > 0) {
        # Adjust df2ColNames to only include existing columns
        df2ColNames <- intersect(df2ColNames, names(df2))
        if (length(df2ColNames) <= 1) {  # Only merge_key might remain
            return(data.frame(
                Error = paste("Insufficient valid columns in df2. Missing:", paste(missing_df2_cols, collapse = ", ")),
                stringsAsFactors = FALSE
            ))
        }
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
    
    # Determine merge key based on whether we normalized interactions
    actual_merge_key <- if (merge_key == "Interaction") "InteractionKey" else merge_key
    
    # Select specified columns (handle merge_key specially to avoid duplicates)
    df1_cols_for_select <- df1ColNames
    df2_cols_for_select <- setdiff(df2ColNames, merge_key)  # Remove merge_key from df2
    
    # Select columns
    df1_selected <- df1_processed[, df1_cols_for_select, drop = FALSE]
    df2_selected <- df2_processed[, df2_cols_for_select, drop = FALSE]
    
    # Add the actual merge key column to both dataframes
    df1_selected[[actual_merge_key]] <- df1_processed[[actual_merge_key]]
    df2_selected[[actual_merge_key]] <- df2_processed[[actual_merge_key]]
    
    # If using InteractionKey, also preserve original Interaction for display
    if (actual_merge_key == "InteractionKey" && merge_key == "Interaction") {
        df1_selected[["Interaction"]] <- df1_processed[["Interaction"]]
        df2_selected[["Interaction"]] <- df2_processed[["Interaction"]]
    }
    
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
    
    # Compute adjusted p-values if p.value.raw is present
    if ("p.value.raw" %in% names(merged_df)) {
        merged_df$p.value.adjusted <- stats::p.adjust(merged_df$p.value.raw, method = p_adjust_method)
    }
    
    # Reorder columns to put Interaction first, then df1 columns, then df2 columns
    interaction_col <- if ("Interaction" %in% names(merged_df)) "Interaction" else actual_merge_key
    other_cols <- setdiff(names(merged_df), interaction_col)
    
    # Put df1 columns first, then df2 columns (excluding the merge key)
    df1_other_cols <- setdiff(df1ColNames, merge_key)
    df2_other_cols <- df2_cols_for_select  # Already excludes merge_key
    
    final_order <- c(interaction_col, 
                    intersect(names(merged_df), df1_other_cols),
                    intersect(names(merged_df), df2_other_cols),
                    setdiff(other_cols, c(df1_other_cols, df2_other_cols)))
    
    merged_df <- merged_df[, final_order, drop = FALSE]
    
    # Apply consistent formatting to all numeric columns (always apply signif)
    numeric_cols <- sapply(merged_df, is.numeric)
    merged_df[numeric_cols] <- lapply(merged_df[numeric_cols], function(x) signif(x, 3))
    
    # Apply scientific notation formatting if requested
    if (use_scientific) {
        old_scipen <- getOption("scipen")
        on.exit(options(scipen = old_scipen))
        options(scipen = 0)
    }
    
    return(merged_df)
}

