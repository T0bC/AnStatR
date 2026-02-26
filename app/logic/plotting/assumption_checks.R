box::use(
  rhino,
)

# =============================================================================
# Pure logic functions for assumption checks (normality + homogeneity)
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Check normality via Shapiro-Wilk test per group
#'
#' Runs Shapiro-Wilk on each group defined by the interaction term,
#' excluding outlier- and trimmed-flagged rows.
#'
#' @param data Data frame
#' @param measure_col Character, measurement column name
#' @param group_col Factor vector (same length as nrow(data)) defining groups
#' @param outlier_col Character, name of the logical outlier flag column
#'   (optional, defaults to "{measure_col}_outlier")
#' @param trimmed_col Character, name of the logical trimmed flag column
#'   (optional, defaults to "{measure_col}_trimmed")
#' @return Data frame with columns: group, n, W, p_value, normal
#' @export
check_normality <- function(data, measure_col, group_col,
                            outlier_col = NULL, trimmed_col = NULL) {
  if (is.null(outlier_col)) {
    outlier_col <- paste0(measure_col, "_outlier")
  }
  if (is.null(trimmed_col)) {
    trimmed_col <- paste0(measure_col, "_trimmed")
  }

  # Build exclusion mask
  excluded <- rep(FALSE, nrow(data))
  if (outlier_col %in% names(data)) {
    excluded <- excluded | data[[outlier_col]]
  }
  if (trimmed_col %in% names(data)) {
    excluded <- excluded | data[[trimmed_col]]
  }

  groups <- levels(group_col)
  if (is.null(groups)) groups <- unique(as.character(group_col))

  rows <- lapply(groups, function(grp) {
    idx <- which(group_col == grp & !excluded)
    values <- data[[measure_col]][idx]
    values <- values[!is.na(values)]
    n <- length(values)

    shapiro <- compute_shapiro(values)

    data.frame(
      group   = grp,
      n       = n,
      W       = shapiro$W,
      p_value = shapiro$p_value,
      normal  = shapiro$normal,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}


#' Check normality of residuals via Shapiro-Wilk test
#'
#' Fits a simple linear model (value ~ group) and runs Shapiro-Wilk
#' on the residuals. This is the model-based approach: it tests
#' whether the ANOVA residuals are normally distributed, which is
#' the actual assumption underlying parametric group comparisons.
#'
#' @param data Data frame
#' @param measure_col Character, measurement column name
#' @param group_col Factor vector defining groups
#' @param outlier_col Character, outlier flag column name (optional)
#' @param trimmed_col Character, trimmed flag column name (optional)
#' @return List with: n, W, p_value, normal
#' @export
check_normality_residuals <- function(data, measure_col, group_col,
                                      outlier_col = NULL,
                                      trimmed_col = NULL) {
  if (is.null(outlier_col)) {
    outlier_col <- paste0(measure_col, "_outlier")
  }
  if (is.null(trimmed_col)) {
    trimmed_col <- paste0(measure_col, "_trimmed")
  }

  # Build exclusion mask
  excluded <- rep(FALSE, nrow(data))
  if (outlier_col %in% names(data)) {
    excluded <- excluded | data[[outlier_col]]
  }
  if (trimmed_col %in% names(data)) {
    excluded <- excluded | data[[trimmed_col]]
  }

  keep <- !excluded & !is.na(data[[measure_col]])
  values <- data[[measure_col]][keep]
  grp <- factor(as.character(group_col[keep]))

  # Need at least 2 groups
  if (nlevels(grp) < 2) {
    return(list(
      n       = length(values),
      W       = NA_real_,
      p_value = NA_real_,
      normal  = NA_character_
    ))
  }

  # Fit linear model and extract residuals
  fit <- tryCatch(
    stats::lm(values ~ grp),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(list(
      n       = length(values),
      W       = NA_real_,
      p_value = NA_real_,
      normal  = NA_character_
    ))
  }

  resid <- stats::residuals(fit)
  result <- compute_shapiro(resid)

  list(
    n       = length(resid),
    W       = result$W,
    p_value = result$p_value,
    normal  = result$normal
  )
}


#' Check homogeneity of variances via manual Levene's test
#'
#' Implements Levene's test as a one-way ANOVA on absolute deviations
#' from group medians. Works for any number of grouping levels
#' (1-way, 2-way, 3-way interactions treated as a single factor).
#'
#' @param data Data frame
#' @param measure_col Character, measurement column name
#' @param group_col Factor vector defining groups
#' @param outlier_col Character, outlier flag column name (optional)
#' @param trimmed_col Character, trimmed flag column name (optional)
#' @return List with: F_statistic, df1, df2, p_value, equal_variances
#' @export
check_homogeneity <- function(data, measure_col, group_col,
                              outlier_col = NULL, trimmed_col = NULL) {
  if (is.null(outlier_col)) {
    outlier_col <- paste0(measure_col, "_outlier")
  }
  if (is.null(trimmed_col)) {
    trimmed_col <- paste0(measure_col, "_trimmed")
  }

  # Build exclusion mask
  excluded <- rep(FALSE, nrow(data))
  if (outlier_col %in% names(data)) {
    excluded <- excluded | data[[outlier_col]]
  }
  if (trimmed_col %in% names(data)) {
    excluded <- excluded | data[[trimmed_col]]
  }

  keep <- !excluded & !is.na(data[[measure_col]])
  values <- data[[measure_col]][keep]
  grp <- as.character(group_col[keep])

  unique_groups <- unique(grp)
  k <- length(unique_groups)

  # Need at least 2 groups with >= 2 observations each
  group_ns <- table(grp)
  usable <- sum(group_ns >= 2)
  if (k < 2 || usable < 2) {
    return(list(
      F_statistic     = NA_real_,
      df1             = NA_integer_,
      df2             = NA_integer_,
      p_value         = NA_real_,
      equal_variances = NA_character_
    ))
  }

  # Compute |x_ij - median_j| for each group j
  group_medians <- tapply(values, grp, stats::median, na.rm = TRUE)
  abs_dev <- abs(values - group_medians[grp])

  # One-way ANOVA on absolute deviations
  levene_result <- compute_oneway_anova(abs_dev, factor(grp))

  list(
    F_statistic     = levene_result$F_statistic,
    df1             = levene_result$df1,
    df2             = levene_result$df2,
    p_value         = levene_result$p_value,
    equal_variances = if (is.na(levene_result$p_value)) {
      NA_character_
    } else if (levene_result$p_value > 0.05) {
      "yes"
    } else {
      "no"
    }
  )
}


#' Recommend whether data transformation is needed
#'
#' Based on the proportion of non-normal groups relative to the threshold.
#'
#' @param normality_df Data frame from check_normality()
#' @param threshold Numeric 0-1, proportion threshold for recommending
#'   transformation (default 0.5)
#' @return List with: recommend (logical), n_non_normal, n_groups, proportion,
#'   message
#' @export
recommend_transformation <- function(normality_df, threshold = 0.5) {
  # Only consider groups with valid Shapiro results
  valid <- normality_df[!is.na(normality_df$normal) &
                          normality_df$normal != "identical values", ]
  n_groups <- nrow(valid)

  if (n_groups == 0) {
    return(list(
      recommend    = FALSE,
      n_non_normal = 0L,
      n_groups     = 0L,
      proportion   = 0,
      message      = "No groups with sufficient data for normality testing."
    ))
  }

  n_non_normal <- sum(valid$normal == "no")
  proportion <- n_non_normal / n_groups

  recommend <- proportion > threshold

  msg <- if (n_non_normal == 0) {
    "All groups are normally distributed. Parametric tests are appropriate."
  } else if (!recommend) {
    paste0(
      n_non_normal, "/", n_groups,
      " groups non-normal (", round(proportion * 100), "% <= ",
      round(threshold * 100), "% threshold). ",
      "Parametric tests likely OK."
    )
  } else {
    paste0(
      n_non_normal, "/", n_groups,
      " groups non-normal (", round(proportion * 100), "% > ",
      round(threshold * 100), "% threshold). ",
      "Transformation recommended. ",
      "Enable 'Normalize data' in the Data Processing tab."
    )
  }

  list(
    recommend    = recommend,
    n_non_normal = n_non_normal,
    n_groups     = n_groups,
    proportion   = proportion,
    message      = msg
  )
}


#' Build recommendation banner info (CSS class + text)
#'
#' Combines normality recommendation and Levene's result into a
#' unified banner with appropriate styling.
#'
#' @param recommendation List from recommend_transformation()
#' @param levene_result List from check_homogeneity()
#' @return List with: css_class, icon, normality_text, variance_text
#' @export
build_recommendation_banner <- function(recommendation, levene_result) {
  # Normality component
  if (recommendation$n_groups == 0) {
    norm_class <- "secondary"
    norm_icon <- "question-circle"
    norm_text <- recommendation$message
  } else if (recommendation$n_non_normal == 0) {
    norm_class <- "success"
    norm_icon <- "check-circle"
    norm_text <- recommendation$message
  } else if (!recommendation$recommend) {
    norm_class <- "warning"
    norm_icon <- "exclamation-triangle"
    norm_text <- recommendation$message
  } else {
    norm_class <- "danger"
    norm_icon <- "x-circle"
    norm_text <- recommendation$message
  }

  # Variance component
  var_text <- if (is.na(levene_result$p_value)) {
    "Levene's test: insufficient data."
  } else if (levene_result$equal_variances == "yes") {
    "Equal variances assumed (Levene's p > 0.05)."
  } else {
    paste0(
      "Unequal variances detected (Levene's p = ",
      format_p(levene_result$p_value), "). ",
      "Consider Welch's ANOVA instead of classical ANOVA."
    )
  }

  # Overall class: worst of normality and variance
  overall_class <- if (norm_class == "danger" ||
                       (!is.na(levene_result$p_value) &&
                        levene_result$equal_variances == "no")) {
    "danger"
  } else if (norm_class == "warning") {
    "warning"
  } else {
    norm_class
  }

  list(
    css_class     = overall_class,
    icon          = norm_icon,
    normality_text = norm_text,
    variance_text  = var_text
  )
}


# =============================================================================
# Internal helpers
# =============================================================================

#' Compute Shapiro-Wilk test for a vector of values
#' @param values Numeric vector (already filtered for NA/outliers/trimmed)
#' @return List with W, p_value, normal
compute_shapiro <- function(values) {
  n <- length(values)
  if (n < 3 || n > 5000) {
    return(list(
      W       = NA_real_,
      p_value = NA_real_,
      normal  = NA_character_
    ))
  }

  if (length(unique(values)) == 1) {
    return(list(
      W       = NA_real_,
      p_value = NA_real_,
      normal  = "identical values"
    ))
  }

  test <- stats::shapiro.test(values)
  list(
    W       = as.numeric(test$statistic),
    p_value = test$p.value,
    normal  = if (test$p.value > 0.05) "yes" else "no"
  )
}


#' Manual one-way ANOVA (used internally for Levene's test)
#' @param values Numeric vector
#' @param groups Factor vector
#' @return List with F_statistic, df1, df2, p_value
compute_oneway_anova <- function(values, groups) {
  grand_mean <- mean(values, na.rm = TRUE)
  group_means <- tapply(values, groups, mean, na.rm = TRUE)
  group_ns <- tapply(values, groups, length)

  k <- length(group_means)
  n_total <- sum(group_ns)

  # Between-group sum of squares
  ss_between <- sum(group_ns * (group_means - grand_mean)^2)
  df_between <- k - 1L

  # Within-group sum of squares
  ss_within <- sum(
    (values - group_means[as.character(groups)])^2,
    na.rm = TRUE
  )
  df_within <- n_total - k

  if (df_between < 1 || df_within < 1) {
    return(list(
      F_statistic = NA_real_,
      df1         = NA_integer_,
      df2         = NA_integer_,
      p_value     = NA_real_
    ))
  }

  ms_between <- ss_between / df_between
  ms_within <- ss_within / df_within

  f_stat <- if (ms_within > 0) ms_between / ms_within else NA_real_
  p_val <- if (!is.na(f_stat)) {
    stats::pf(f_stat, df_between, df_within, lower.tail = FALSE)
  } else {
    NA_real_
  }

  list(
    F_statistic = f_stat,
    df1         = df_between,
    df2         = df_within,
    p_value     = p_val
  )
}


#' Format a p-value for display
#' @param p Numeric p-value
#' @return Character string
#' @export
format_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< 0.001")
  format(round(p, 3), nsmall = 3)
}
