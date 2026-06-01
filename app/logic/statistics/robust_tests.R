box::use(
  rhino,
  stats,
  WRS2,
)

box::use(
  app/logic/shared/error_handling,
  app/logic/statistics/omnibus,
  app/logic/statistics/validation_utils,
)

# =============================================================================
# Robust ANOVA tests using trimmed means (Welch-Yuen family).
# Currently: t1way, t2way, t3way.
# No Shiny dependencies allowed in this file.
# =============================================================================

# =============================================================================
# t1way — One-Way Robust ANOVA
# =============================================================================

#' t1way test configuration
#'
#' Config object consumed by omnibus$run_omnibus_test().
#' Defines all hooks for the one-way robust trimmed-means ANOVA.
t1way_config <- list(
  name = "t1way",

  result_cols = c(
    "F_statistic", "df1", "df2", "Effect_Size", "p_value"
  ),

  validate = function(df, x_axis) {
    validation_utils$validate_n_way(
      df, x_axis, 1, "t1way", "t1way_validate"
    )
  },

  build_context = function(df, x_axis, measure_col,
                           tr_value, use_bootstrap) {
    list(
      measure = measure_col,
      grouping = x_axis[1],
      n_groups = length(unique(df[[x_axis[1]]])),
      n_observations = nrow(df),
      trim = tr_value,
      bootstrap = use_bootstrap
    )
  },

  build_formula = function(measure_col, x_axis) {
    stats$as.formula(
      paste0("`", measure_col, "` ~ `", x_axis[1], "`")
    )
  },

  run_test = function(formula_obj, data, tr_value) {
    vars <- all.vars(formula_obj)[-1]
    conversion <- omnibus$safe_factor_conversion(data, vars)
    if (!conversion$success) {
      stop(conversion$error$message)
    }
    WRS2$t1way(
      formula = formula_obj,
      data = conversion$data,
      tr = tr_value
    )
  },

  extract_results = function(out) {
    c(out$test, out$df1, out$df2, out$effsize, out$p.value)
  },

  format_results = function(results, x_axis, use_bootstrap) {
    if (use_bootstrap) {
      omnibus$format_bootstrap_results(results)
    } else {
      results[] <- lapply(
        results,
        function(x) signif(x, 3)
      )
      results
    }
  }
)


#' Perform One-Way Robust ANOVA (Welch-Yuen t1way)
#'
#' Uses WRS2::t1way with trimmed means. Supports optional bootstrap.
#'
#' @param df Data frame containing the data
#' @param x_axis Character, single grouping column name
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, trim proportion (0-0.5)
#' @param use_bootstrap Logical, whether to use bootstrap
#' @param boot_samples Integer, number of bootstrap samples
#' @param boot_sample_size Integer or NULL, bootstrap sample size per group
#' @return Data frame with test results, or structured app_error
#' @export
perform_t1way <- function(df, x_axis, measure_col,
                          tr_value,
                          use_bootstrap = FALSE,
                          boot_samples = 599,
                          boot_sample_size = NULL,
                          is_rm = FALSE,
                          id_col = NULL,
                          within_col = NULL) {
  rhino$log$info(
    "t1way: starting for measure='{measure_col}',",
    " grouping='{x_axis[1]}',",
    " bootstrap={use_bootstrap}, rm={is_rm}"
  )

  if (isTRUE(is_rm) && !is.null(id_col) && !is.null(within_col)) {
    return(perform_rm_robust(
      df = df, x_axis = x_axis, measure_col = measure_col,
      id_col = id_col, within_col = within_col,
      tr_value = tr_value
    ))
  }

  omnibus$run_omnibus_test(
    df = df,
    x_axis = x_axis,
    measure_col = measure_col,
    tr_value = tr_value,
    use_bootstrap = use_bootstrap,
    boot_samples = boot_samples,
    boot_sample_size = boot_sample_size,
    config = t1way_config
  )
}


# =============================================================================
# t2way — Two-Way Robust ANOVA
# =============================================================================

#' t2way test configuration
#'
#' Config object consumed by omnibus$run_omnibus_test().
#' Defines all hooks for the two-way robust trimmed-means ANOVA.
#' Returns main effects (A, B) and interaction (AB).
t2way_config <- list(
  name = "t2way",

  result_cols = c(
    "Qa", "Qb", "Qab",
    "A.p.value", "B.p.value", "AB.p.value"
  ),

  validate = function(df, x_axis) {
    validation_utils$validate_n_way(
      df, x_axis, 2, "t2way", "t2way_validate"
    )
  },

  build_context = function(df, x_axis, measure_col,
                           tr_value, use_bootstrap) {
    list(
      measure = measure_col,
      factor1 = x_axis[1],
      factor2 = x_axis[2],
      levels_factor1 = length(unique(df[[x_axis[1]]])),
      levels_factor2 = length(unique(df[[x_axis[2]]])),
      n_observations = nrow(df),
      trim = tr_value,
      bootstrap = use_bootstrap
    )
  },

  build_formula = function(measure_col, x_axis) {
    stats$as.formula(
      paste0(
        "`", measure_col, "` ~ `",
        x_axis[1], "` * `", x_axis[2], "`"
      )
    )
  },

  run_test = function(formula_obj, data, tr_value) {
    vars <- all.vars(formula_obj)[-1]
    conversion <- omnibus$safe_factor_conversion(data, vars)
    if (!conversion$success) {
      stop(conversion$error$message)
    }
    WRS2$t2way(
      formula = formula_obj,
      data = conversion$data,
      tr = tr_value
    )
  },

  extract_results = function(out) {
    c(
      out$Qa, out$Qb, out$Qab,
      out$A.p.value, out$B.p.value, out$AB.p.value
    )
  },

  format_results = function(results, x_axis, use_bootstrap) {
    validation_utils$format_multiway_results(
      results, x_axis, use_bootstrap,
      q_cols = c("Qa", "Qb", "Qab"),
      p_cols = c("A.p.value", "B.p.value", "AB.p.value")
    )
  }
)


#' Perform Two-Way Robust ANOVA (Welch-Yuen t2way)
#'
#' Uses WRS2::t2way with trimmed means. Returns main effects (A, B)
#' and interaction (AB) with Q statistics and p-values.
#' Supports optional bootstrap.
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of exactly two grouping column names
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, trim proportion (0-0.5)
#' @param use_bootstrap Logical, whether to use bootstrap
#' @param boot_samples Integer, number of bootstrap samples
#' @param boot_sample_size Integer or NULL, bootstrap sample size per group
#' @return Data frame with test results, or structured app_error
#' @export
perform_t2way <- function(df, x_axis, measure_col,
                          tr_value,
                          use_bootstrap = FALSE,
                          boot_samples = 599,
                          boot_sample_size = NULL,
                          is_rm = FALSE,
                          id_col = NULL,
                          within_col = NULL) {
  rhino$log$info(
    "t2way: starting for measure='{measure_col}',",
    " factors='{x_axis[1]}' * '{x_axis[2]}',",
    " bootstrap={use_bootstrap}, rm={is_rm}"
  )

  if (isTRUE(is_rm) && !is.null(id_col) && !is.null(within_col)) {
    return(perform_rm_robust(
      df = df, x_axis = x_axis, measure_col = measure_col,
      id_col = id_col, within_col = within_col,
      tr_value = tr_value
    ))
  }

  omnibus$run_omnibus_test(
    df = df,
    x_axis = x_axis,
    measure_col = measure_col,
    tr_value = tr_value,
    use_bootstrap = use_bootstrap,
    boot_samples = boot_samples,
    boot_sample_size = boot_sample_size,
    config = t2way_config
  )
}


# =============================================================================
# t3way — Three-Way Robust ANOVA
# =============================================================================

#' t3way test configuration
#'
#' Config object consumed by omnibus$run_omnibus_test().
#' Defines all hooks for the three-way robust trimmed-means ANOVA.
#' Returns main effects (A, B, C), two-way interactions (AB, AC, BC),
#' and three-way interaction (ABC).
t3way_config <- list(
  name = "t3way",

  result_cols = c(
    "Qa", "Qb", "Qc", "Qab", "Qac", "Qbc", "Qabc",
    "A.p.value", "B.p.value", "C.p.value",
    "AB.p.value", "AC.p.value", "BC.p.value",
    "ABC.p.value"
  ),

  validate = function(df, x_axis) {
    validation_utils$validate_n_way(
      df, x_axis, 3, "t3way", "t3way_validate"
    )
  },

  build_context = function(df, x_axis, measure_col,
                           tr_value, use_bootstrap) {
    list(
      measure = measure_col,
      factor1 = x_axis[1],
      factor2 = x_axis[2],
      factor3 = x_axis[3],
      levels_factor1 = length(unique(df[[x_axis[1]]])),
      levels_factor2 = length(unique(df[[x_axis[2]]])),
      levels_factor3 = length(unique(df[[x_axis[3]]])),
      n_observations = nrow(df),
      trim = tr_value,
      bootstrap = use_bootstrap
    )
  },

  build_formula = function(measure_col, x_axis) {
    stats$as.formula(
      paste0(
        "`", measure_col, "` ~ `",
        x_axis[1], "` * `",
        x_axis[2], "` * `",
        x_axis[3], "`"
      )
    )
  },

  run_test = function(formula_obj, data, tr_value) {
    vars <- all.vars(formula_obj)[-1]
    conversion <- omnibus$safe_factor_conversion(data, vars)
    if (!conversion$success) {
      stop(conversion$error$message)
    }
    WRS2$t3way(
      formula = formula_obj,
      data = conversion$data,
      tr = tr_value
    )
  },

  extract_results = function(out) {
    c(
      out$Qa, out$Qb, out$Qc,
      out$Qab, out$Qac, out$Qbc, out$Qabc,
      out$A.p.value, out$B.p.value, out$C.p.value,
      out$AB.p.value, out$AC.p.value, out$BC.p.value,
      out$ABC.p.value
    )
  },

  format_results = function(results, x_axis, use_bootstrap) {
    validation_utils$format_multiway_results(
      results, x_axis, use_bootstrap,
      q_cols = c("Qa", "Qb", "Qc", "Qab", "Qac", "Qbc", "Qabc"),
      p_cols = c(
        "A.p.value", "B.p.value", "C.p.value",
        "AB.p.value", "AC.p.value", "BC.p.value", "ABC.p.value"
      )
    )
  }
)


#' Perform Three-Way Robust ANOVA (Welch-Yuen t3way)
#'
#' Uses WRS2::t3way with trimmed means. Returns main effects (A, B, C),
#' two-way interactions (AB, AC, BC), and three-way interaction (ABC)
#' with Q statistics and p-values. Supports optional bootstrap.
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of exactly three grouping column names
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, trim proportion (0-0.5)
#' @param use_bootstrap Logical, whether to use bootstrap
#' @param boot_samples Integer, number of bootstrap samples
#' @param boot_sample_size Integer or NULL, bootstrap sample size per group
#' @return Data frame with test results, or structured app_error
#' @export
perform_t3way <- function(df, x_axis, measure_col,
                          tr_value,
                          use_bootstrap = FALSE,
                          boot_samples = 599,
                          boot_sample_size = NULL,
                          is_rm = FALSE,
                          id_col = NULL,
                          within_col = NULL) {
  rhino$log$info(
    "t3way: starting for measure='{measure_col}',",
    " factors='{x_axis[1]}' * '{x_axis[2]}'",
    " * '{x_axis[3]}',",
    " bootstrap={use_bootstrap}, rm={is_rm}"
  )

  if (isTRUE(is_rm) && !is.null(id_col) && !is.null(within_col)) {
    return(perform_rm_robust(
      df = df, x_axis = x_axis, measure_col = measure_col,
      id_col = id_col, within_col = within_col,
      tr_value = tr_value
    ))
  }

  omnibus$run_omnibus_test(
    df = df,
    x_axis = x_axis,
    measure_col = measure_col,
    tr_value = tr_value,
    use_bootstrap = use_bootstrap,
    boot_samples = boot_samples,
    boot_sample_size = boot_sample_size,
    config = t3way_config
  )
}


# =============================================================================
# Repeated Measures Robust ANOVA
# =============================================================================

#' Perform Repeated Measures Robust ANOVA
#'
#' Supports 1-way and 2-way mixed designs with within-subject factors.
#' - 1-way within: Uses WRS2::rmanova()
#' - 2-way mixed (1B x 1W): Uses WRS2::bwtrim()
#'
#' For 3-way RM designs, robust methods are not available in WRS2.
#' Consider using nonparametric alternatives (ARTool) instead.
#'
#' @param df Data frame (long format)
#' @param x_axis Character vector of grouping columns
#' @param measure_col Character, measurement column name
#' @param id_col Character, subject ID column name
#' @param within_col Character, within-subject factor column name
#' @param tr_value Numeric, trim proportion
#' @return Data frame with RM robust ANOVA results, or structured app_error
#' @export
perform_rm_robust <- function(df, x_axis, measure_col,
                               id_col, within_col,
                               tr_value = 0.2) {
  between_cols <- setdiff(x_axis, within_col)
  n_between <- length(between_cols)
  n_ways <- length(x_axis)

  rhino$log$info(
    "rm_robust: starting for measure='{measure_col}',",
    " id='{id_col}', within='{within_col}',",
    " between='{paste(between_cols, collapse=\", \")}',",
    " design={n_between}B x 1W"
  )

  error_context <- list(
    measure = measure_col,
    id_col = id_col,
    within_col = within_col,
    between_cols = paste(between_cols, collapse = ", "),
    n_observations = nrow(df),
    design = paste0(n_between, "B x 1W"),
    test_type = "robust_rm_anova"
  )

  test_result <- error_handling$safe_execute(
    expr = {
      # Check for unsupported 3-way RM designs.
      # NOTE (intentional asymmetry): WRS2 provides no robust omnibus test
      # for 3-way RM (bwtrim only handles 1 between x 1 within), so the
      # omnibus is blocked here. The robust RM *post-hoc* path
      # (perform_rm_robust_posthoc) DOES support 3-way, because it works
      # pairwise via Yuen's dependent-samples test and does not need a
      # 3-way omnibus model. Users wanting a 3-way RM omnibus should use
      # the nonparametric (ARTool) approach.
      if (n_ways >= 3) {
        stop(paste0(
          "3-way repeated measures designs are not supported for the ",
          "robust omnibus test (WRS2::bwtrim only supports 1 between x ",
          "1 within). The robust pairwise post-hoc comparisons below are ",
          "still computed with the correct paired tests. For a 3-way RM ",
          "omnibus, use the nonparametric approach (ARTool)."
        ))
      }

      # Validate columns
      if (!id_col %in% names(df)) {
        stop(paste0("ID column '", id_col, "' not found."))
      }
      if (!within_col %in% names(df)) {
        stop(paste0(
          "Within-subject factor '", within_col, "' not found."
        ))
      }

      df[[id_col]] <- as.factor(df[[id_col]])
      df[[within_col]] <- as.factor(df[[within_col]])
      for (bc in between_cols) {
        df[[bc]] <- as.factor(df[[bc]])
      }

      # Listwise deletion for repeated measures: drop rows with a missing
      # measurement, then remove any subject that no longer has a complete
      # set of within-subject observations. This keeps the design balanced
      # so WRS2::rmanova / bwtrim can run.
      df <- df[!is.na(df[[measure_col]]), , drop = FALSE]
      n_within_levels <- length(unique(df[[within_col]]))
      obs_per_subject <- table(droplevels(df[[id_col]]))
      complete_ids <- names(obs_per_subject)[
        obs_per_subject == n_within_levels
      ]
      df <- df[as.character(df[[id_col]]) %in% complete_ids, , drop = FALSE]
      df[[id_col]] <- droplevels(as.factor(df[[id_col]]))

      if (nrow(df) == 0 || length(complete_ids) < 2) {
        stop(paste0(
          "Not enough subjects with complete repeated-measures data ",
          "after removing missing values. At least 2 subjects must have ",
          "a measurement at every level of '", within_col, "'."
        ))
      }

      # Validate balanced design
      id_within_counts <- table(df[[id_col]], df[[within_col]])
      if (any(id_within_counts != 1)) {
        stop(paste0(
          "Unbalanced repeated measures design. ",
          "Each subject must appear exactly once per ",
          "level of '", within_col, "'."
        ))
      }

      if (n_between == 0) {
        # Pure within-subject (1-way): WRS2::rmanova
        rm_result <- WRS2$rmanova(
          y = df[[measure_col]],
          groups = df[[within_col]],
          blocks = df[[id_col]],
          tr = tr_value
        )

        data.frame(
          Effect = within_col,
          Test.Statistic = signif(rm_result$test, 3),
          p.value = signif(rm_result$p.value, 3),
          stringsAsFactors = FALSE
        )
      } else if (n_between == 1) {
        # 2-way mixed (1B x 1W): WRS2::bwtrim
        formula_str <- paste0(
          "`", measure_col, "` ~ `",
          paste(c(between_cols, within_col), collapse = "` * `"),
          "`"
        )
        formula_obj <- stats$as.formula(formula_str)

        bw_result <- WRS2$bwtrim(
          formula = formula_obj,
          id = df[[id_col]],
          data = df,
          tr = tr_value
        )

        effect_labels <- c(
          between_cols[1], within_col,
          paste0(between_cols[1], ":", within_col)
        )

        data.frame(
          Effect = effect_labels,
          Q.Statistic = signif(
            c(bw_result$Qa, bw_result$Qb, bw_result$Qab), 3
          ),
          p.value = signif(
            c(
              bw_result$A.p.value,
              bw_result$B.p.value,
              bw_result$AB.p.value
            ), 3
          ),
          stringsAsFactors = FALSE
        )
      } else {
        stop(paste0(
          "Unsupported RM design: ", n_between, " between x 1 within. ",
          "Robust RM ANOVA supports only 1-way within or ",
          "2-way mixed (1 between x 1 within) designs. ",
          "For more complex designs, use nonparametric alternatives."
        ))
      }
    },
    operation_name = "rm_robust",
    context = error_context,
    error_parser = error_handling$stat_error_parser
  )

  if (!test_result$success) return(test_result$error)

  test_result$result
}
