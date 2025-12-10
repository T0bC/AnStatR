#' Summarize data with various statistics
#'
#' Calculates summary statistics (n, mean, median, var, sd, sm, sr) for measurement
#' columns, optionally including Shapiro-Wilk normality test results.
#'
#' @param data Data frame containing the data
#' @param grouping_vars Character vector of column names to group by
#' @param measure_vars Character vector of measurement column names
#' @param exclude_vars Character vector of columns to exclude from results
#' @param shapiro_test Logical, whether to include Shapiro-Wilk test
#' @param trim_value Numeric, trim proportion (0-1) for trimmed statistics
#' @return Data frame in long format with summary statistics
summarize_data <- function(data,
                           grouping_vars,
                           measure_vars,
                           exclude_vars = NULL,
                           shapiro_test = FALSE,
                           trim_value = 0) {
    
    # Filter measure_vars to exclude those containing "_outlier"
    measure_vars <- measure_vars[!grepl("_outlier", measure_vars)]
    
    # Function to get trimmed indices
    get_trimmed_indices <- function(x, trim) {
        n <- length(x)
        k <- floor(n * trim)
        if (k == 0 || n <= 2 * k) return(seq_along(x))
        order_x <- order(x)
        trimmed_indices <- order_x[(k + 1):(n - k)]
        return(trimmed_indices)
    }
    
    # Function to filter outliers and get trimmed data
    get_filtered_trimmed_data <- function(x, outliers, trim) {
        # First filter out outliers
        valid_indices <- which(!outliers)
        filtered_data <- x[valid_indices]
        # Then apply trimming
        if (length(filtered_data) > 0) {
            trimmed_indices <- get_trimmed_indices(filtered_data, trim)
            return(filtered_data[trimmed_indices])
        }
        return(filtered_data)
    }
    
    # Helper to safely get outlier column
    safe_get_outlier <- function(col_name, env) {
        outlier_col <- paste0(col_name, "_outlier")
        if (outlier_col %in% names(env)) {
            return(env[[outlier_col]])
        }
        # Return FALSE vector if no outlier column exists
        return(rep(FALSE, nrow(env)))
    }
    
    # Helper to count trimmed samples (excluding outliers first)
    count_trimmed <- function(x, outliers, trim) {
        # First filter out outliers
        valid_indices <- which(!outliers)
        filtered_data <- x[valid_indices]
        n <- length(filtered_data)
        k <- floor(n * trim)
        # Trimmed = 2*k (k from each end)
        if (k == 0 || n <= 2 * k) return(0L)
        return(as.integer(2 * k))
    }
    
    # Calculate summary statistics for specified measurement columns
    summary_stats <- data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_vars))) %>%
        dplyr::summarize(dplyr::across(
            dplyr::all_of(measure_vars),
            list(
                n_total = ~{
                    # Total non-NA values before any exclusion
                    sum(!is.na(.))
                },
                n_outliers = ~{
                    # Count of outliers excluded
                    outlier_col <- paste0(dplyr::cur_column(), "_outlier")
                    outliers <- if (outlier_col %in% names(dplyr::pick(dplyr::everything()))) {
                        dplyr::pick(dplyr::everything())[[outlier_col]]
                    } else {
                        rep(FALSE, length(.))
                    }
                    sum(outliers & !is.na(.), na.rm = TRUE)
                },
                n_trimmed = ~{
                    # Count of trimmed values (after outlier removal)
                    outlier_col <- paste0(dplyr::cur_column(), "_outlier")
                    outliers <- if (outlier_col %in% names(dplyr::pick(dplyr::everything()))) {
                        dplyr::pick(dplyr::everything())[[outlier_col]]
                    } else {
                        rep(FALSE, length(.))
                    }
                    count_trimmed(., outliers, trim_value)
                },
                n = ~{
                    # Final count used for statistics (after outlier + trim exclusion)
                    outlier_col <- paste0(dplyr::cur_column(), "_outlier")
                    outliers <- if (outlier_col %in% names(dplyr::pick(dplyr::everything()))) {
                        dplyr::pick(dplyr::everything())[[outlier_col]]
                    } else {
                        rep(FALSE, length(.))
                    }
                    filtered_trimmed_data <- get_filtered_trimmed_data(., outliers, trim_value)
                    sum(!is.na(filtered_trimmed_data))
                },
                mean = ~{
                    outlier_col <- paste0(dplyr::cur_column(), "_outlier")
                    outliers <- if (outlier_col %in% names(dplyr::pick(dplyr::everything()))) {
                        dplyr::pick(dplyr::everything())[[outlier_col]]
                    } else {
                        rep(FALSE, length(.))
                    }
                    filtered_trimmed_data <- get_filtered_trimmed_data(., outliers, trim_value)
                    mean(filtered_trimmed_data, na.rm = TRUE)
                },
                median = ~{
                    outlier_col <- paste0(dplyr::cur_column(), "_outlier")
                    outliers <- if (outlier_col %in% names(dplyr::pick(dplyr::everything()))) {
                        dplyr::pick(dplyr::everything())[[outlier_col]]
                    } else {
                        rep(FALSE, length(.))
                    }
                    filtered_trimmed_data <- get_filtered_trimmed_data(., outliers, trim_value)
                    stats::median(filtered_trimmed_data, na.rm = TRUE)
                },
                var = ~{
                    outlier_col <- paste0(dplyr::cur_column(), "_outlier")
                    outliers <- if (outlier_col %in% names(dplyr::pick(dplyr::everything()))) {
                        dplyr::pick(dplyr::everything())[[outlier_col]]
                    } else {
                        rep(FALSE, length(.))
                    }
                    filtered_trimmed_data <- get_filtered_trimmed_data(., outliers, trim_value)
                    stats::var(filtered_trimmed_data, na.rm = TRUE)
                },
                sd = ~{
                    outlier_col <- paste0(dplyr::cur_column(), "_outlier")
                    outliers <- if (outlier_col %in% names(dplyr::pick(dplyr::everything()))) {
                        dplyr::pick(dplyr::everything())[[outlier_col]]
                    } else {
                        rep(FALSE, length(.))
                    }
                    filtered_trimmed_data <- get_filtered_trimmed_data(., outliers, trim_value)
                    stats::sd(filtered_trimmed_data, na.rm = TRUE)
                },
                sem = ~{
                    outlier_col <- paste0(dplyr::cur_column(), "_outlier")
                    outliers <- if (outlier_col %in% names(dplyr::pick(dplyr::everything()))) {
                        dplyr::pick(dplyr::everything())[[outlier_col]]
                    } else {
                        rep(FALSE, length(.))
                    }
                    filtered_trimmed_data <- get_filtered_trimmed_data(., outliers, trim_value)
                    n_valid <- sum(!is.na(filtered_trimmed_data))
                    if (n_valid > 1) {
                        stats::sd(filtered_trimmed_data, na.rm = TRUE) / sqrt(n_valid)
                    } else {
                        NA_real_
                    }
                },
                cv = ~{
                    outlier_col <- paste0(dplyr::cur_column(), "_outlier")
                    outliers <- if (outlier_col %in% names(dplyr::pick(dplyr::everything()))) {
                        dplyr::pick(dplyr::everything())[[outlier_col]]
                    } else {
                        rep(FALSE, length(.))
                    }
                    filtered_trimmed_data <- get_filtered_trimmed_data(., outliers, trim_value)
                    n_valid <- sum(!is.na(filtered_trimmed_data))
                    if (n_valid > 1) {
                        stats::sd(filtered_trimmed_data, na.rm = TRUE) / mean(filtered_trimmed_data, na.rm = TRUE)
                    } else {
                        NA_real_
                    }
                }
            )
        ), .groups = "drop")
    
    # New function to check if all values are identical
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
                        outlier_col <- paste0(dplyr::cur_column(), "_outlier")
                        outliers <- if (outlier_col %in% names(dplyr::pick(dplyr::everything()))) {
                            dplyr::pick(dplyr::everything())[[outlier_col]]
                        } else {
                            rep(FALSE, length(.))
                        }
                        filtered_trimmed_data <- stats::na.omit(get_filtered_trimmed_data(., outliers, trim_value))
                        if (length(filtered_trimmed_data) >= 3 && length(filtered_trimmed_data) <= 5000) {
                            if (all_identical(filtered_trimmed_data)) {
                                NA_real_
                            } else {
                                stats::shapiro.test(filtered_trimmed_data)$p.value
                            }
                        } else {
                            NA_real_
                        }
                    },
                    shapiro_W = ~{
                        outlier_col <- paste0(dplyr::cur_column(), "_outlier")
                        outliers <- if (outlier_col %in% names(dplyr::pick(dplyr::everything()))) {
                            dplyr::pick(dplyr::everything())[[outlier_col]]
                        } else {
                            rep(FALSE, length(.))
                        }
                        filtered_trimmed_data <- stats::na.omit(get_filtered_trimmed_data(., outliers, trim_value))
                        if (length(filtered_trimmed_data) >= 3 && length(filtered_trimmed_data) <= 5000) {
                            if (all_identical(filtered_trimmed_data)) {
                                NA_real_
                            } else {
                                stats::shapiro.test(filtered_trimmed_data)$statistic
                            }
                        } else {
                            NA_real_
                        }
                    },
                    normal = ~{
                        outlier_col <- paste0(dplyr::cur_column(), "_outlier")
                        outliers <- if (outlier_col %in% names(dplyr::pick(dplyr::everything()))) {
                            dplyr::pick(dplyr::everything())[[outlier_col]]
                        } else {
                            rep(FALSE, length(.))
                        }
                        filtered_trimmed_data <- stats::na.omit(get_filtered_trimmed_data(., outliers, trim_value))
                        if (length(filtered_trimmed_data) >= 3 && length(filtered_trimmed_data) <= 5000) {
                            if (all_identical(filtered_trimmed_data)) {
                                "identical values"
                            } else {
                                ifelse(stats::shapiro.test(filtered_trimmed_data)$p.value > 0.05, "yes", "no")
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
    })) %>%
        dplyr::select(Measurement, dplyr::everything())
    
    # Remove rows (basically columns in the orig df) which may not hold measurements
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
