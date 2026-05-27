box::use(
  stats,
)

box::use(
  app/logic/shared/error_handling,
  app/logic/statistics/cliff_delta[cidmulv2_labelled],
)

# =============================================================================
# Shared validation and helper utilities for post-hoc statistical tests.
# No Shiny dependencies allowed in this file.
# =============================================================================


#' Validate n-way omnibus test inputs
#'
#' Generic validation for omnibus tests (ANOVA, Kruskal-Wallis, ART, t1way, etc.)
#' Checks that the correct number of grouping variables is provided and each
#' factor has at least 2 levels.
#'
#' @param df Data frame
#' @param x_axis Grouping columns
#' @param n_ways Expected number of grouping variables (1, 2, or 3)
#' @param test_name Human-readable test name for error messages
#' @param operation_name Operation name for error context
#' @return NULL if valid, app_error otherwise
#' @export
validate_n_way <- function(df, x_axis, n_ways, test_name, operation_name) {
  if (length(x_axis) != n_ways) {
    return(error_handling$simple_error(
      message = paste0(
        test_name, " requires exactly ", n_ways,
        " grouping variable", if (n_ways > 1) "s" else "", "."
      ),
      operation_name = operation_name,
      context = list(n_grouping_vars = length(x_axis))
    ))
  }
  for (i in seq_along(x_axis)) {
    n_levels <- length(unique(df[[x_axis[i]]]))
    if (n_levels < 2) {
      return(error_handling$simple_error(
        message = paste0(
          test_name, " requires at least 2 levels in '",
          x_axis[i], "', found ", n_levels, "."
        ),
        operation_name = operation_name,
        context = list(factor = x_axis[i], n_levels = n_levels)
      ))
    }
  }
  NULL
}


#' Validate post-hoc inputs
#'
#' Checks that there are at least 2 groups for pairwise comparisons.
#'
#' @param df Data frame
#' @param x_axis Grouping columns
#' @return NULL if valid, app_error otherwise
#' @export
validate_posthoc <- function(df, x_axis) {
  if (length(x_axis) > 1) {
    combined <- do.call(paste, c(df[x_axis], sep = "."))
  } else {
    combined <- df[[x_axis[1]]]
  }
  n_groups <- length(unique(combined))
  if (n_groups < 2) {
    return(error_handling$simple_error(
      message = paste0(
        "Post-hoc tests require at least 2 groups, found ",
        n_groups, "."
      ),
      operation_name = "posthoc_validate",
      context = list(n_groups = n_groups)
    ))
  }
  NULL
}


#' Build error context for post-hoc tests
#'
#' Creates a context list for error reporting in post-hoc tests.
#'
#' @param df Data frame
#' @param x_axis Grouping columns
#' @param measure_col Measurement column
#' @param tr_value Trim proportion (optional, for robust tests)
#' @param use_bootstrap Logical (optional, for bootstrap tests)
#' @return List with context information
#' @export
build_posthoc_context <- function(df, x_axis, measure_col,
                                  tr_value = NULL,
                                  use_bootstrap = NULL) {
  ctx <- list(
    measure = measure_col,
    grouping = paste(x_axis, collapse = ", "),
    n_observations = nrow(df)
  )
  if (!is.null(tr_value)) {
    ctx$trim <- tr_value
  }
  if (!is.null(use_bootstrap)) {
    ctx$bootstrap <- use_bootstrap
  }
  ctx
}


#' Normalize interaction strings for consistent matching
#'
#' Extracts group names from "GroupA vs. GroupB" format and creates
#' an alphabetized key for consistent matching between tests.
#'
#' @param df Data frame with Interaction column
#' @return Data frame with added InteractionKey column
#' @export
normalize_interaction <- function(df) {
  if (!"Interaction" %in% names(df)) return(df)

  df$InteractionKey <- vapply(df$Interaction, function(int) {
    parts <- trimws(strsplit(int, " vs\\. ")[[1]])
    paste(sort(parts), collapse = " vs. ")
  }, character(1))

  df
}


#' Filter for valid comparisons in multi-factor designs
#'
#' Keeps only comparisons where groups differ by exactly one factor level.
#'
#' @param df Data frame with Interaction column
#' @param x_axis Character vector of grouping columns
#' @return Filtered data frame
#' @export
filter_valid_comparisons <- function(df, x_axis) {
  if (is.null(x_axis) || length(x_axis) <= 1) return(df)

  keep <- vapply(df$Interaction, function(int) {
    parts <- trimws(strsplit(int, " vs\\. ")[[1]])
    if (length(parts) != 2) return(FALSE)
    a_parts <- strsplit(parts[1], "\\.")[[1]]
    b_parts <- strsplit(parts[2], "\\.")[[1]]
    if (length(a_parts) != length(b_parts)) return(FALSE)
    sum(a_parts != b_parts) == 1
  }, logical(1))

  df[keep, , drop = FALSE]
}


#' Run single Cliff's Delta iteration
#'
#' Computes Cliff's Delta for all pairwise group comparisons.
#' For multi-way designs, groups are combined into a single factor.
#'
#' @param sample_data Data frame
#' @param x_axis Grouping columns
#' @param measure_col Measurement column
#' @return Data frame with cliff results
#' @export
run_cliff_iteration <- function(sample_data, x_axis, measure_col) {
  if (length(x_axis) > 1) {
    sample_data$combinedGroups <- do.call(
      paste, c(sample_data[x_axis], sep = ".")
    )
  } else {
    sample_data$combinedGroups <- sample_data[[x_axis[1]]]
  }
  sample_data$combinedGroupsNum <- as.numeric(
    as.factor(sample_data$combinedGroups)
  )

  cliff_result <- cidmulv2_labelled(
    data = sample_data,
    gcode = "combinedGroupsNum",
    glab = "combinedGroups",
    dp = measure_col,
    alpha = 0.05,
    CI.FWE = FALSE
  )

  test_df <- cliff_result$test
  data.frame(
    Interaction = paste(
      test_df$Group.A, "vs.", test_df$Group.B
    ),
    Cliff.psihat = test_df$p.hat,
    Cliff.ci.lower = test_df$p.ci.lower,
    Cliff.ci.upper = test_df$p.ci.upper,
    Cliff.p.value = test_df$p.value,
    Cliff.p.crit = test_df$p.crit,
    stringsAsFactors = FALSE
  )
}


#' Classify RM comparison as paired, unpaired, or skip
#'
#' Determines the type of comparison between two groups in a repeated measures
#' design by examining which factors differ between them.
#'
#' Classification logic:
#' - "paired": Between-subject factors match, within-subject factor differs
#'   (same subjects measured at different time points/conditions)
#' - "unpaired": Between-subject factors differ, within-subject factor matches
#'   (different subjects at the same time point/condition)
#' - "skip": Both or neither factor types differ (not a valid simple comparison)
#'
#' @param g1_label First group label (e.g., "A.T1")
#' @param g2_label Second group label (e.g., "A.T2")
#' @param x_axis Character vector of factor names in order
#' @param within_col Character, the within-subject factor name
#' @return Character: "paired", "unpaired", or "skip"
#' @export
classify_rm_comparison <- function(g1_label, g2_label, x_axis, within_col) {
  g1_parts <- strsplit(as.character(g1_label), ".", fixed = TRUE)[[1]]
  g2_parts <- strsplit(as.character(g2_label), ".", fixed = TRUE)[[1]]

  within_idx <- which(x_axis == within_col)
  between_idx <- which(x_axis != within_col)

  # Check if between-subject factors all match
  between_match <- if (length(between_idx) > 0) {
    all(g1_parts[between_idx] == g2_parts[between_idx])
  } else {
    TRUE
  }

  # Check if within-subject factor matches
  within_match <- (g1_parts[within_idx] == g2_parts[within_idx])

  if (between_match && !within_match) {
    "paired"
  } else if (!between_match && within_match) {
    "unpaired"
  } else {
    "skip"
  }
}


#' Format multi-way robust ANOVA results
#'
#' Generic formatter for t2way and t3way results. Builds effect labels
#' from x_axis and formats Q statistics and p-values with optional
#' bootstrap confidence intervals.
#'
#' @param results Data frame with test results
#' @param x_axis Character vector of grouping column names
#' @param use_bootstrap Logical, whether bootstrap was used
#' @param q_cols Character vector of Q statistic column names
#' @param p_cols Character vector of p-value column names
#' @return Formatted data frame with Effect, Q.Statistic, p.value columns
#' @export
format_multiway_results <- function(results, x_axis, use_bootstrap,
                                    q_cols, p_cols) {
  n <- length(x_axis)
  effect_labels <- x_axis
  if (n >= 2) {
    effect_labels <- c(effect_labels, paste0(x_axis[1], ":", x_axis[2]))
  }
  if (n >= 3) {
    effect_labels <- c(
      effect_labels,
      paste0(x_axis[1], ":", x_axis[3]),
      paste0(x_axis[2], ":", x_axis[3]),
      paste0(x_axis[1], ":", x_axis[2], ":", x_axis[3])
    )
  }

  if (use_bootstrap) {
    ci_bounds <- apply(results, 2, function(x) {
      valid_x <- x[!is.na(x)]
      if (length(valid_x) < 2) {
        return(c(NA_real_, NA_real_))
      }
      stats$quantile(valid_x, c(0.025, 0.975), na.rm = TRUE)
    })

    q_stats <- vapply(q_cols, function(col) {
      paste0(
        signif(mean(results[[col]], na.rm = TRUE), 3),
        " [",
        signif(ci_bounds[1, col], 3), " - ",
        signif(ci_bounds[2, col], 3), "]"
      )
    }, character(1))

    p_vals <- vapply(p_cols, function(col) {
      paste0(
        signif(mean(results[[col]], na.rm = TRUE), 3),
        " [",
        signif(ci_bounds[1, col], 3), " - ",
        signif(ci_bounds[2, col], 3), "]"
      )
    }, character(1))

    data.frame(
      Effect = effect_labels,
      Q.Statistic = unname(q_stats),
      p.value = unname(p_vals),
      stringsAsFactors = FALSE
    )
  } else {
    q_vals <- vapply(q_cols, function(col) {
      signif(results[[col]][1], 3)
    }, numeric(1))

    p_vals <- vapply(p_cols, function(col) {
      signif(results[[col]][1], 3)
    }, numeric(1))

    data.frame(
      Effect = effect_labels,
      Q.Statistic = unname(q_vals),
      p.value = unname(p_vals),
      stringsAsFactors = FALSE
    )
  }
}
