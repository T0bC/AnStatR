#' Summarize data with various statistics
#'
#' Calculates summary statistics (n, mean, median, var, sd, sem, cv) for measurement
#' columns, optionally including Shapiro-Wilk normality test results.
#' 
#' Uses {col}_outlier and {col}_trimmed columns from the data to exclude
#' outliers and trimmed values from statistics calculation.
#'
#' @param data Data frame containing the data with {col}_outlier and {col}_trimmed columns
#' @param grouping_vars Character vector of column names to group by
#' @param measure_vars Character vector of measurement column names
#' @param exclude_vars Character vector of columns to exclude from results
#' @param shapiro_test Logical, whether to include Shapiro-Wilk test
#' @return Data frame in long format with summary statistics
summarize_data <- function(data,
                           grouping_vars,
                           measure_vars,
                           exclude_vars = NULL,
                           shapiro_test = FALSE) {
    
    # Filter measure_vars to exclude helper columns
    measure_vars <- measure_vars[!grepl("_outlier|_trimmed", measure_vars)]
    
    # Helper to get filtered data (excluding outliers and trimmed values)
    get_filtered_data <- function(values, outliers, trimmed) {
        # Exclude both outliers and trimmed values
        keep_idx <- which(!outliers & !trimmed & !is.na(values))
        values[keep_idx]
    }
    
    # Calculate summary statistics for specified measurement columns
    summary_stats <- data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) %>%
        dplyr::summarize(dplyr::across(
            dplyr::all_of(measure_vars),
            list(
                n = ~{
                    # Get outlier and trimmed columns for this measurement
                    col_name <- dplyr::cur_column()
                    outlier_col <- paste0(col_name, "_outlier")
                    trimmed_col <- paste0(col_name, "_trimmed")
                    all_cols <- dplyr::pick(dplyr::everything())
                    
                    outliers <- if (outlier_col %in% names(all_cols)) all_cols[[outlier_col]] else rep(FALSE, length(.))
                    trimmed <- if (trimmed_col %in% names(all_cols)) all_cols[[trimmed_col]] else rep(FALSE, length(.))
                    
                    length(get_filtered_data(., outliers, trimmed))
                },
                mean = ~{
                    col_name <- dplyr::cur_column()
                    outlier_col <- paste0(col_name, "_outlier")
                    trimmed_col <- paste0(col_name, "_trimmed")
                    all_cols <- dplyr::pick(dplyr::everything())
                    
                    outliers <- if (outlier_col %in% names(all_cols)) all_cols[[outlier_col]] else rep(FALSE, length(.))
                    trimmed <- if (trimmed_col %in% names(all_cols)) all_cols[[trimmed_col]] else rep(FALSE, length(.))
                    
                    mean(get_filtered_data(., outliers, trimmed), na.rm = TRUE)
                },
                median = ~{
                    col_name <- dplyr::cur_column()
                    outlier_col <- paste0(col_name, "_outlier")
                    trimmed_col <- paste0(col_name, "_trimmed")
                    all_cols <- dplyr::pick(dplyr::everything())
                    
                    outliers <- if (outlier_col %in% names(all_cols)) all_cols[[outlier_col]] else rep(FALSE, length(.))
                    trimmed <- if (trimmed_col %in% names(all_cols)) all_cols[[trimmed_col]] else rep(FALSE, length(.))
                    
                    stats::median(get_filtered_data(., outliers, trimmed), na.rm = TRUE)
                },
                var = ~{
                    col_name <- dplyr::cur_column()
                    outlier_col <- paste0(col_name, "_outlier")
                    trimmed_col <- paste0(col_name, "_trimmed")
                    all_cols <- dplyr::pick(dplyr::everything())
                    
                    outliers <- if (outlier_col %in% names(all_cols)) all_cols[[outlier_col]] else rep(FALSE, length(.))
                    trimmed <- if (trimmed_col %in% names(all_cols)) all_cols[[trimmed_col]] else rep(FALSE, length(.))
                    
                    stats::var(get_filtered_data(., outliers, trimmed), na.rm = TRUE)
                },
                sd = ~{
                    col_name <- dplyr::cur_column()
                    outlier_col <- paste0(col_name, "_outlier")
                    trimmed_col <- paste0(col_name, "_trimmed")
                    all_cols <- dplyr::pick(dplyr::everything())
                    
                    outliers <- if (outlier_col %in% names(all_cols)) all_cols[[outlier_col]] else rep(FALSE, length(.))
                    trimmed <- if (trimmed_col %in% names(all_cols)) all_cols[[trimmed_col]] else rep(FALSE, length(.))
                    
                    stats::sd(get_filtered_data(., outliers, trimmed), na.rm = TRUE)
                },
                sem = ~{
                    col_name <- dplyr::cur_column()
                    outlier_col <- paste0(col_name, "_outlier")
                    trimmed_col <- paste0(col_name, "_trimmed")
                    all_cols <- dplyr::pick(dplyr::everything())
                    
                    outliers <- if (outlier_col %in% names(all_cols)) all_cols[[outlier_col]] else rep(FALSE, length(.))
                    trimmed <- if (trimmed_col %in% names(all_cols)) all_cols[[trimmed_col]] else rep(FALSE, length(.))
                    
                    filtered <- get_filtered_data(., outliers, trimmed)
                    n_valid <- length(filtered)
                    if (n_valid > 1) {
                        stats::sd(filtered, na.rm = TRUE) / sqrt(n_valid)
                    } else {
                        NA_real_
                    }
                },
                cv = ~{
                    col_name <- dplyr::cur_column()
                    outlier_col <- paste0(col_name, "_outlier")
                    trimmed_col <- paste0(col_name, "_trimmed")
                    all_cols <- dplyr::pick(dplyr::everything())
                    
                    outliers <- if (outlier_col %in% names(all_cols)) all_cols[[outlier_col]] else rep(FALSE, length(.))
                    trimmed <- if (trimmed_col %in% names(all_cols)) all_cols[[trimmed_col]] else rep(FALSE, length(.))
                    
                    filtered <- get_filtered_data(., outliers, trimmed)
                    n_valid <- length(filtered)
                    if (n_valid > 1) {
                        stats::sd(filtered, na.rm = TRUE) / mean(filtered, na.rm = TRUE)
                    } else {
                        NA_real_
                    }
                },
                n_outliers = ~{
                    # Count of outliers excluded (at end of table)
                    col_name <- dplyr::cur_column()
                    outlier_col <- paste0(col_name, "_outlier")
                    all_cols <- dplyr::pick(dplyr::everything())
                    
                    outliers <- if (outlier_col %in% names(all_cols)) all_cols[[outlier_col]] else rep(FALSE, length(.))
                    sum(outliers & !is.na(.), na.rm = TRUE)
                },
                n_trimmed = ~{
                    # Count of trimmed values excluded (at end of table)
                    col_name <- dplyr::cur_column()
                    trimmed_col <- paste0(col_name, "_trimmed")
                    outlier_col <- paste0(col_name, "_outlier")
                    all_cols <- dplyr::pick(dplyr::everything())
                    
                    # Only count trimmed that are not also outliers
                    outliers <- if (outlier_col %in% names(all_cols)) all_cols[[outlier_col]] else rep(FALSE, length(.))
                    trimmed <- if (trimmed_col %in% names(all_cols)) all_cols[[trimmed_col]] else rep(FALSE, length(.))
                    sum(trimmed & !outliers & !is.na(.), na.rm = TRUE)
                }
            )
        ), .groups = "drop")
    
    # Helper to check if all values are identical
    all_identical <- function(x) {
        length(unique(x)) == 1
    }
    
    if (shapiro_test) {
        shapiro_stats <- data %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) %>%
            dplyr::summarize(dplyr::across(
                dplyr::all_of(measure_vars),
                list(
                    shapiro_p = ~{
                        col_name <- dplyr::cur_column()
                        outlier_col <- paste0(col_name, "_outlier")
                        trimmed_col <- paste0(col_name, "_trimmed")
                        all_cols <- dplyr::pick(dplyr::everything())
                        
                        outliers <- if (outlier_col %in% names(all_cols)) all_cols[[outlier_col]] else rep(FALSE, length(.))
                        trimmed <- if (trimmed_col %in% names(all_cols)) all_cols[[trimmed_col]] else rep(FALSE, length(.))
                        
                        filtered <- get_filtered_data(., outliers, trimmed)
                        if (length(filtered) >= 3 && length(filtered) <= 5000) {
                            if (all_identical(filtered)) {
                                NA_real_
                            } else {
                                stats::shapiro.test(filtered)$p.value
                            }
                        } else {
                            NA_real_
                        }
                    },
                    shapiro_W = ~{
                        col_name <- dplyr::cur_column()
                        outlier_col <- paste0(col_name, "_outlier")
                        trimmed_col <- paste0(col_name, "_trimmed")
                        all_cols <- dplyr::pick(dplyr::everything())
                        
                        outliers <- if (outlier_col %in% names(all_cols)) all_cols[[outlier_col]] else rep(FALSE, length(.))
                        trimmed <- if (trimmed_col %in% names(all_cols)) all_cols[[trimmed_col]] else rep(FALSE, length(.))
                        
                        filtered <- get_filtered_data(., outliers, trimmed)
                        if (length(filtered) >= 3 && length(filtered) <= 5000) {
                            if (all_identical(filtered)) {
                                NA_real_
                            } else {
                                stats::shapiro.test(filtered)$statistic
                            }
                        } else {
                            NA_real_
                        }
                    },
                    normal = ~{
                        col_name <- dplyr::cur_column()
                        outlier_col <- paste0(col_name, "_outlier")
                        trimmed_col <- paste0(col_name, "_trimmed")
                        all_cols <- dplyr::pick(dplyr::everything())
                        
                        outliers <- if (outlier_col %in% names(all_cols)) all_cols[[outlier_col]] else rep(FALSE, length(.))
                        trimmed <- if (trimmed_col %in% names(all_cols)) all_cols[[trimmed_col]] else rep(FALSE, length(.))
                        
                        filtered <- get_filtered_data(., outliers, trimmed)
                        if (length(filtered) >= 3 && length(filtered) <= 5000) {
                            if (all_identical(filtered)) {
                                "identical values"
                            } else {
                                ifelse(stats::shapiro.test(filtered)$p.value > 0.05, "yes", "no")
                            }
                        } else {
                            NA_character_
                        }
                    }
                )
            ), .groups = "drop")
        
        summary_stats <- dplyr::left_join(summary_stats, shapiro_stats, 
                                          by = grouping_vars)
    }
    
    # Reshape data to long format
    summary_long <- dplyr::bind_rows(lapply(measure_vars, function(x) {
        summary_stats %>%
            dplyr::select(dplyr::all_of(grouping_vars), dplyr::starts_with(paste0(x, "_"))) %>%
            dplyr::rename_with(~gsub(paste0(x, "_"), "", .x), dplyr::starts_with(paste0(x, "_"))) %>%
            dplyr::mutate(Measurement = x)
    }))
    
    # Reorder columns: Measurement first, then grouping, then stats, then shapiro (if present), then n_outliers/n_trimmed last
    base_cols <- c("Measurement", grouping_vars, "n", "mean", "median", "var", "sd", "sem", "cv")
    shapiro_cols <- if (shapiro_test) c("shapiro_p", "shapiro_W", "normal") else character(0)
    exclusion_cols <- c("n_outliers", "n_trimmed")
    
    col_order <- c(base_cols, shapiro_cols, exclusion_cols)
    col_order <- col_order[col_order %in% names(summary_long)]
    
    summary_long <- summary_long %>%
        dplyr::select(dplyr::all_of(col_order))
    
    # Remove rows which may not hold measurements
    if (!is.null(exclude_vars)) {
        summary_long <- summary_long %>% dplyr::filter(!Measurement %in% exclude_vars)
    }
    
    summary_long
}


#' Filter dataframe by multiple columns and return list of filtered dataframes
#'
#' Generates all combinations of factor levels for specified columns and
#' returns a list of filtered dataframes, one for each combination.
#'
#' @param df Data frame to filter
#' @param columns_to_filter Character vector of column names to filter by
#' @return List of lists, each containing:
#'   - col: Character string identifying the combination
#'   - df: Filtered data frame for that combination
filter_by_columns <- function(df, columns_to_filter) {
    # Get the levels for each column
    levels_list <- purrr::map(columns_to_filter, ~levels(as.factor(df[[.]])))
    names(levels_list) <- columns_to_filter
    
    # Generate all combinations of levels
    combinations <- expand.grid(levels_list, stringsAsFactors = FALSE)
    
    # Filter the dataframe by each combination and return a list of lists
    filtered_dfs <- purrr::map(seq_len(nrow(combinations)), function(i) {
        filter_conditions <- purrr::map2(columns_to_filter, seq_along(columns_to_filter), 
                                         ~rlang::quo(!!rlang::sym(.x) == combinations[i, .y]))
        filtered_df <- df %>% dplyr::filter(!!!filter_conditions)
        
        # Round numeric columns
        filtered_df <- filtered_df %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 3)))
        
        # Map combinations as character and create a concatenated string of the column levels
        if (ncol(combinations) == 1) {
            col_string <- as.character(combinations[i, ])
        } else {
            col_string <- paste(purrr::map_df(combinations[i, ], as.character), collapse = " | ")
        }
        list(
            col = col_string, 
            df = filtered_df
        )
    })
    
    # Filter out empty dataframes
    filtered_dfs <- purrr::keep(filtered_dfs, ~nrow(.$df) > 0)
    
    return(filtered_dfs)
}
