box::use(
  rhino,
  stats,
)

box::use(
  app/logic/shared/error_handling,
  app/logic/statistics/omnibus,
  app/logic/statistics/validation_utils,
)

# =============================================================================
# Parametric post-hoc pairwise comparisons: Tukey HSD + Cohen's d.
# Returns raw p-values only — adjustment is deferred to the combined table.
# No Shiny dependencies allowed in this file.
# =============================================================================


# =============================================================================
# Private helpers
# =============================================================================

#' Parse Tukey HSD comparison string into group pair
#'
#' TukeyHSD uses "GroupB-GroupA" format. For multi-way with interaction(),
#' groups contain "." separators, so we split on the last "-" that
#' separates two groups.
#'
#' @param comp Character, comparison string from TukeyHSD rownames
#' @return List with g1 and g2 (reversed to match Cohen's d ordering)
parse_tukey_comparison <- function(comp) {
  parts <- strsplit(comp, "-")[[1]]
  if (length(parts) > 2) {
    # Group names may contain "." but not "-" in our convention.
    # TukeyHSD format: "GroupB-GroupA" — try all split points
    # and pick the one where both sides are valid group names.
    # Fallback: first part vs rest.
    g1 <- paste(parts[-length(parts)], collapse = "-")
    g2 <- parts[length(parts)]
  } else {
    g1 <- parts[1]
    g2 <- parts[2]
  }
  # Reverse order to match Cohen's d (group_i vs group_j, i < j)
  list(g1 = trimws(g2), g2 = trimws(g1))
}

#' Calculate raw (unadjusted) p-values from mean differences
#'
#' Uses the pooled MSE from the ANOVA model to compute t-statistics
#' and two-sided p-values for each pairwise comparison.
#'
#' @param differences Numeric vector of mean differences
#' @param group1 Character vector of first group names
#' @param group2 Character vector of second group names
#' @param group_sizes Named numeric vector of group sizes
#' @param mse Numeric, mean squared error from ANOVA residuals
#' @param df_residual Numeric, residual degrees of freedom
#' @return Numeric vector of raw p-values
calculate_raw_pvalues <- function(differences, group1, group2,
                                  group_sizes, mse, df_residual) {
  vapply(seq_along(differences), function(i) {
    n1 <- group_sizes[group1[i]]
    n2 <- group_sizes[group2[i]]

    if (is.na(n1) || is.na(n2)) {
      return(NA_real_)
    }

    se_diff <- sqrt(mse * (1 / n1 + 1 / n2))
    t_stat <- differences[i] / se_diff
    2 * stats$pt(abs(t_stat), df = df_residual, lower.tail = FALSE)
  }, numeric(1))
}


# =============================================================================
# Tukey HSD
# =============================================================================

#' Perform Tukey HSD Post-hoc Comparisons
#'
#' Conducts Tukey's Honestly Significant Difference test for pairwise
#' comparisons. For multi-way designs, groups are combined into a single
#' interaction factor using "." separator to produce pairwise comparisons
#' compatible with Cohen's d output.
#'
#' Returns raw (unadjusted) p-values so downstream filtering and
#' p-value correction can be applied.
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of grouping columns
#' @param measure_col Character, measurement column name
#' @return Data frame with pairwise comparisons or structured error
#' @export
perform_tukey_hsd <- function(df, x_axis, measure_col) {
  rhino$log$info(
    "tukey_hsd: starting for measure='{measure_col}',",
    " factors='{paste(x_axis, collapse=\", \")}'"
  )

  validation <- validation_utils$validate_posthoc(df, x_axis)
  if (error_handling$is_app_error(validation)) {
    return(validation)
  }

  error_context <- validation_utils$build_posthoc_context(df, x_axis, measure_col)

  test_result <- error_handling$safe_execute(
    expr = {
      # Convert grouping variables to factors
      for (var in x_axis) {
        df[[var]] <- as.factor(df[[var]])
      }

      # For multi-way designs, create combined interaction group
      if (length(x_axis) > 1) {
        df$interaction_group <- interaction(
          df[x_axis], sep = "."
        )
        formula_obj <- stats$as.formula(
          paste0("`", measure_col, "` ~ interaction_group")
        )
        group_sizes <- tapply(
          df[[measure_col]], df$interaction_group,
          function(x) sum(!is.na(x))
        )
      } else {
        formula_obj <- stats$as.formula(
          paste0("`", measure_col, "` ~ `", x_axis[1], "`")
        )
        group_sizes <- tapply(
          df[[measure_col]], df[[x_axis[1]]],
          function(x) sum(!is.na(x))
        )
      }

      # Fit ANOVA model and run Tukey HSD
      model <- stats$aov(formula_obj, data = df)
      tukey_out <- stats$TukeyHSD(model, conf.level = 0.95)

      # Extract model statistics for raw p-value calculation
      summary_table <- summary(model)[[1]]
      mse <- summary_table["Residuals", "Mean Sq"]
      df_residual <- summary_table["Residuals", "Df"]

      # Extract Tukey results
      if (length(x_axis) == 1) {
        tukey_df <- as.data.frame(tukey_out[[x_axis[1]]])
      } else {
        tukey_df <- as.data.frame(tukey_out[[1]])
      }
      comparisons <- rownames(tukey_df)

      # Parse comparison strings into group pairs
      group_pairs <- lapply(comparisons, parse_tukey_comparison)
      g1_vec <- vapply(group_pairs, `[[`, character(1), "g1")
      g2_vec <- vapply(group_pairs, `[[`, character(1), "g2")

      # Calculate raw p-values
      raw_pvals <- calculate_raw_pvalues(
        differences = tukey_df$diff,
        group1 = g1_vec,
        group2 = g2_vec,
        group_sizes = group_sizes,
        mse = mse,
        df_residual = df_residual
      )

      # Build result data frame
      data.frame(
        Interaction = paste(g1_vec, "vs.", g2_vec),
        Tukey.diff = signif(tukey_df$diff, 3),
        Tukey.ci.lower = signif(tukey_df$lwr, 3),
        Tukey.ci.upper = signif(tukey_df$upr, 3),
        Tukey.p.value = signif(raw_pvals, 3),
        stringsAsFactors = FALSE
      )
    },
    operation_name = "tukey_hsd",
    context = error_context,
    error_parser = error_handling$stat_error_parser
  )

  if (!test_result$success) return(test_result$error)

  test_result$result
}


# =============================================================================
# Cohen's d
# =============================================================================

#' Calculate Cohen's d Effect Size
#'
#' Computes Cohen's d for pairwise comparisons. For multi-way designs,
#' groups are combined into a single factor using "." separator.
#' Returns raw (unadjusted) t-test p-values.
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of grouping columns
#' @param measure_col Character, measurement column name
#' @return Data frame with effect sizes or structured error
#' @export
perform_cohens_d <- function(df, x_axis, measure_col) {
  rhino$log$info(
    "cohens_d: starting for measure='{measure_col}',",
    " factors='{paste(x_axis, collapse=\", \")}'"
  )

  validation <- validation_utils$validate_posthoc(df, x_axis)
  if (error_handling$is_app_error(validation)) {
    return(validation)
  }

  error_context <- validation_utils$build_posthoc_context(df, x_axis, measure_col)

  test_result <- error_handling$safe_execute(
    expr = {
      # Convert grouping variables to factors
      for (var in x_axis) {
        df[[var]] <- as.factor(df[[var]])
      }

      # For multi-way designs, create interaction variable
      if (length(x_axis) > 1) {
        df$interaction_group <- interaction(
          df[x_axis], sep = "."
        )
        group_var <- "interaction_group"
      } else {
        group_var <- x_axis[1]
      }

      # Get unique groups using factor levels for consistent ordering
      groups <- levels(df[[group_var]])
      if (is.null(groups)) {
        groups <- sort(unique(as.character(df[[group_var]])))
      }
      n_groups <- length(groups)

      if (n_groups < 2) {
        stop("Cohen's d requires at least 2 groups for comparison.")
      }

      # All pairwise comparisons
      results <- list()
      for (i in 1:(n_groups - 1)) {
        for (j in (i + 1):n_groups) {
          group1_data <- df[
            df[[group_var]] == groups[i], measure_col
          ]
          group2_data <- df[
            df[[group_var]] == groups[j], measure_col
          ]

          # Calculate means and SDs
          mean1 <- mean(group1_data, na.rm = TRUE)
          mean2 <- mean(group2_data, na.rm = TRUE)
          sd1 <- stats$sd(group1_data, na.rm = TRUE)
          sd2 <- stats$sd(group2_data, na.rm = TRUE)
          n1 <- sum(!is.na(group1_data))
          n2 <- sum(!is.na(group2_data))

          # Pooled standard deviation
          pooled_sd <- sqrt(
            ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) /
              (n1 + n2 - 2)
          )

          # Cohen's d
          d <- (mean1 - mean2) / pooled_sd

          # Standard error of d
          se_d <- sqrt(
            (n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2))
          )

          # 95% CI for d
          ci_lower <- d - 1.96 * se_d
          ci_upper <- d + 1.96 * se_d

          # Raw t-test p-value (two-sample, equal variance)
          t_test <- stats$t.test(
            group1_data, group2_data, var.equal = TRUE
          )

          results[[length(results) + 1]] <- data.frame(
            Interaction = paste(
              as.character(groups[i]), "vs.",
              as.character(groups[j])
            ),
            Cohen.d = signif(d, 3),
            Cohen.ci.lower = signif(ci_lower, 3),
            Cohen.ci.upper = signif(ci_upper, 3),
            Cohen.p.value = signif(t_test$p.value, 3),
            stringsAsFactors = FALSE
          )
        }
      }

      do.call(rbind, results)
    },
    operation_name = "cohens_d",
    context = error_context,
    error_parser = error_handling$stat_error_parser
  )

  if (!test_result$success) return(test_result$error)

  test_result$result
}


# =============================================================================
# Combined parametric post-hoc
# =============================================================================

#' Perform Combined Parametric Post-Hoc Tests
#'
#' Runs both Tukey HSD and Cohen's d, merges results by interaction key,
#' optionally filters valid comparisons, then applies p-value adjustment.
#'
#' @param df Data frame
#' @param x_axis Character vector of grouping column(s)
#' @param measure_col Character, measurement column name
#' @param p_adjust_method Character, p-value adjustment method
#' @param filter_valid Logical, filter to valid comparisons for multi-way
#' @return Data frame with combined results or app_error
#' @export
perform_combined_parametric_posthoc <- function(
    df, x_axis, measure_col,
    p_adjust_method = "bonferroni",
    filter_valid = FALSE,
    is_rm = FALSE,
    id_col = NULL,
    within_col = NULL) {
  rhino$log$info(
    "combined_parametric_posthoc: starting for",
    " measure='{measure_col}', rm={is_rm}"
  )

  if (isTRUE(is_rm) && !is.null(id_col) && !is.null(within_col)) {
    return(perform_rm_parametric_posthoc(
      df = df, x_axis = x_axis, measure_col = measure_col,
      id_col = id_col, within_col = within_col,
      p_adjust_method = p_adjust_method
    ))
  }

  tukey_result <- perform_tukey_hsd(
    df = df, x_axis = x_axis, measure_col = measure_col
  )

  cohen_result <- perform_cohens_d(
    df = df, x_axis = x_axis, measure_col = measure_col
  )

  tukey_err <- error_handling$is_app_error(tukey_result)
  cohen_err <- error_handling$is_app_error(cohen_result)

  if (tukey_err && cohen_err) {
    return(error_handling$simple_error(
      message = paste0(
        "Both Tukey HSD and Cohen's d failed. ",
        "Tukey: ", tukey_result$message, ". ",
        "Cohen: ", cohen_result$message
      ),
      operation_name = "combined_parametric_posthoc"
    ))
  }

  if (tukey_err) return(tukey_result)
  if (cohen_err) return(cohen_result)

  if (!is.data.frame(tukey_result) ||
      !is.data.frame(cohen_result)) {
    return(error_handling$simple_error(
      message = "Unexpected result type from post-hoc tests.",
      operation_name = "combined_parametric_posthoc"
    ))
  }

  if (nrow(tukey_result) == 0 || nrow(cohen_result) == 0) {
    return(error_handling$simple_error(
      message = paste0(
        "One or both post-hoc tests returned ",
        "empty results."
      ),
      operation_name = "combined_parametric_posthoc"
    ))
  }

  # Normalize interaction keys for matching
  tukey_norm <- validation_utils$normalize_interaction(tukey_result)
  cohen_norm <- validation_utils$normalize_interaction(cohen_result)

  # Merge by InteractionKey
  cohen_cols <- setdiff(
    names(cohen_norm), c("Interaction", "InteractionKey")
  )
  tukey_selected <- tukey_norm
  cohen_selected <- cohen_norm[
    , c("InteractionKey", cohen_cols), drop = FALSE
  ]

  merged <- merge(
    tukey_selected, cohen_selected,
    by = "InteractionKey", all = FALSE
  )

  if (nrow(merged) == 0) {
    return(error_handling$simple_error(
      message = paste0(
        "No matching interactions between ",
        "Tukey HSD and Cohen's d results."
      ),
      operation_name = "combined_parametric_posthoc"
    ))
  }

  merged$InteractionKey <- NULL

  # Filter valid comparisons for multi-way designs
  if (filter_valid && length(x_axis) > 1) {
    merged <- validation_utils$filter_valid_comparisons(merged, x_axis)
    if (nrow(merged) == 0) {
      return(error_handling$simple_error(
        message = paste0(
          "No valid comparisons remain after filtering."
        ),
        operation_name = "combined_parametric_posthoc"
      ))
    }
  }

  # Apply p-value adjustment
  if ("Tukey.p.value" %in% names(merged) &&
      is.numeric(merged$Tukey.p.value)) {
    merged$Tukey.p.adjusted <- stats$p.adjust(
      merged$Tukey.p.value, method = p_adjust_method
    )
  }
  if ("Cohen.p.value" %in% names(merged) &&
      is.numeric(merged$Cohen.p.value)) {
    merged$Cohen.p.adjusted <- stats$p.adjust(
      merged$Cohen.p.value, method = p_adjust_method
    )
  }

  # Reorder columns
  desired_order <- c(
    "Interaction",
    "Tukey.diff", "Tukey.ci.lower", "Tukey.ci.upper",
    "Tukey.p.value", "Tukey.p.adjusted",
    "Cohen.d", "Cohen.ci.lower", "Cohen.ci.upper",
    "Cohen.p.value", "Cohen.p.adjusted"
  )
  final_cols <- intersect(desired_order, names(merged))
  extra_cols <- setdiff(names(merged), desired_order)
  merged <- merged[, c(final_cols, extra_cols), drop = FALSE]

  # Round numeric columns
  numeric_cols <- vapply(merged, is.numeric, logical(1))
  merged[numeric_cols] <- lapply(
    merged[numeric_cols], function(x) signif(x, 3)
  )

  merged
}


# =============================================================================
# Repeated Measures Parametric Post-Hoc
# =============================================================================

#' Perform RM Parametric Post-Hoc (Paired t-tests + Paired Cohen's d)
#'
#' Computes pairwise paired t-tests and paired Cohen's d.
#' For multi-way designs (e.g., TREATMENT × TIME), creates full
#' interaction groups and performs paired comparisons within
#' each between-subject group.
#'
#' @param df Data frame (long format)
#' @param x_axis Character vector of grouping columns
#' @param measure_col Character, measurement column name
#' @param id_col Character, subject ID column name
#' @param within_col Character, within-subject factor column name
#' @param p_adjust_method Character, p-value adjustment method
#' @return Data frame with paired posthoc results or app_error
#' @export
perform_rm_parametric_posthoc <- function(
    df, x_axis, measure_col,
    id_col, within_col,
    p_adjust_method = "bonferroni") {
  rhino$log$info(
    "rm_parametric_posthoc: starting for",
    " measure='{measure_col}'"
  )

  error_context <- list(
    measure = measure_col,
    id_col = id_col,
    within_col = within_col,
    x_axis = x_axis,
    test_type = "rm_parametric_posthoc"
  )

  test_result <- error_handling$safe_execute(
    expr = {
      df[[id_col]] <- as.factor(df[[id_col]])
      df[[within_col]] <- as.factor(df[[within_col]])

      # Identify between-subject factors (x_axis minus within_col)
      between_factors <- setdiff(x_axis, within_col)

      # Create interaction group column combining all x_axis factors
      if (length(x_axis) > 1) {
        df$interaction_group <- do.call(
          paste, c(df[x_axis], sep = ".")
        )
      } else {
        df$interaction_group <- as.character(df[[x_axis[1]]])
      }
      df$interaction_group <- as.factor(df$interaction_group)

      # Get all unique interaction groups
      all_groups <- levels(df$interaction_group)
      n_groups <- length(all_groups)

      if (n_groups < 2) {
        stop("Paired post-hoc requires at least 2 groups.")
      }

      results <- list()

      # All pairwise comparisons between interaction groups
      for (i in 1:(n_groups - 1)) {
        for (j in (i + 1):n_groups) {
          g1_label <- all_groups[i]
          g2_label <- all_groups[j]

          # Parse group labels to extract factor levels
          g1_parts <- strsplit(g1_label, ".", fixed = TRUE)[[1]]
          g2_parts <- strsplit(g2_label, ".", fixed = TRUE)[[1]]

          # Find which factor differs
          # x_axis order determines part positions
          within_idx <- which(x_axis == within_col)
          between_idx <- which(x_axis != within_col)

          # Check if between-subject factors match
          # (only compare within same between-subject group)
          between_match <- TRUE
          if (length(between_idx) > 0) {
            for (bi in between_idx) {
              if (g1_parts[bi] != g2_parts[bi]) {
                between_match <- FALSE
                break
              }
            }
          }

          if (!between_match) {
            # Skip: different between-subject groups
            # (not a valid paired comparison)
            next
          }

          # Get data for each group
          g1_data <- df[
            df$interaction_group == g1_label,
            c(id_col, measure_col)
          ]
          g2_data <- df[
            df$interaction_group == g2_label,
            c(id_col, measure_col)
          ]

          # Merge by ID to align pairs
          paired <- merge(
            g1_data, g2_data,
            by = id_col, suffixes = c(".1", ".2")
          )

          if (nrow(paired) < 2) {
            # Skip if not enough paired observations
            next
          }

          vals1 <- paired[[paste0(measure_col, ".1")]]
          vals2 <- paired[[paste0(measure_col, ".2")]]

          # Paired t-test
          t_res <- stats$t.test(
            vals1, vals2, paired = TRUE
          )

          # Paired Cohen's d (dz = mean_diff / sd_diff)
          diffs <- vals1 - vals2
          mean_diff <- mean(diffs, na.rm = TRUE)
          sd_diff <- stats$sd(diffs, na.rm = TRUE)
          d_z <- mean_diff / sd_diff
          n_pairs <- sum(!is.na(diffs))
          se_d <- sqrt(1 / n_pairs + d_z^2 / (2 * n_pairs))
          d_ci_lower <- d_z - 1.96 * se_d
          d_ci_upper <- d_z + 1.96 * se_d

          results[[length(results) + 1]] <- data.frame(
            Interaction = paste(g1_label, "vs.", g2_label),
            Paired.t.statistic = signif(
              t_res$statistic[["t"]], 3
            ),
            Paired.t.p.value = signif(t_res$p.value, 3),
            Paired.d = signif(d_z, 3),
            Paired.d.ci.lower = signif(d_ci_lower, 3),
            Paired.d.ci.upper = signif(d_ci_upper, 3),
            stringsAsFactors = FALSE
          )
        }
      }

      if (length(results) == 0) {
        stop("No valid paired comparisons found.")
      }

      merged <- do.call(rbind, results)

      # Apply p-value adjustment
      if ("Paired.t.p.value" %in% names(merged) &&
          is.numeric(merged$Paired.t.p.value)) {
        merged$Paired.t.p.adjusted <- stats$p.adjust(
          merged$Paired.t.p.value, method = p_adjust_method
        )
      }

      # Round numeric columns
      numeric_cols <- vapply(merged, is.numeric, logical(1))
      merged[numeric_cols] <- lapply(
        merged[numeric_cols], function(x) signif(x, 3)
      )

      merged
    },
    operation_name = "rm_parametric_posthoc",
    context = error_context,
    error_parser = error_handling$stat_error_parser
  )

  if (!test_result$success) return(test_result$error)

  test_result$result
}
