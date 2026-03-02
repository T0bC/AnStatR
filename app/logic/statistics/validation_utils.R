box::use(
  stats,
)

box::use(
  app/logic/error_handling,
  app/logic/statistics/cliff_delta[cidmulv2_labelled],
)

# =============================================================================
# Shared validation and helper utilities for post-hoc statistical tests.
# No Shiny dependencies allowed in this file.
# =============================================================================


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
