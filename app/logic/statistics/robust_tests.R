box::use(
  rhino,
  stats,
  WRS2,
)

box::use(
  app/logic/error_handling,
  app/logic/statistics/omnibus,
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
    if (length(x_axis) != 1) {
      return(error_handling$simple_error(
        message = "t1way requires exactly one grouping variable.",
        operation_name = "t1way_validate",
        context = list(n_grouping_vars = length(x_axis))
      ))
    }
    n_groups <- length(unique(df[[x_axis[1]]]))
    if (n_groups < 2) {
      return(error_handling$simple_error(
        message = paste0(
          "t1way requires at least 2 groups, found ",
          n_groups, "."
        ),
        operation_name = "t1way_validate",
        context = list(
          grouping = x_axis[1],
          n_groups = n_groups
        )
      ))
    }
    NULL
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
                          boot_sample_size = NULL) {
  rhino$log$info(
    "t1way: starting for measure='{measure_col}',",
    " grouping='{x_axis[1]}',",
    " bootstrap={use_bootstrap}"
  )

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
    if (length(x_axis) != 2) {
      return(error_handling$simple_error(
        message = paste0(
          "t2way requires exactly two grouping variables."
        ),
        operation_name = "t2way_validate",
        context = list(n_grouping_vars = length(x_axis))
      ))
    }
    for (i in seq_along(x_axis)) {
      n_levels <- length(unique(df[[x_axis[i]]]))
      if (n_levels < 2) {
        return(error_handling$simple_error(
          message = paste0(
            "t2way requires at least 2 levels in '",
            x_axis[i], "', found ", n_levels, "."
          ),
          operation_name = "t2way_validate",
          context = list(
            factor = x_axis[i],
            n_levels = n_levels
          )
        ))
      }
    }
    NULL
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
    effect_labels <- c(
      x_axis[1], x_axis[2],
      paste0(x_axis[1], ":", x_axis[2])
    )

    q_cols <- c("Qa", "Qb", "Qab")
    p_cols <- c("A.p.value", "B.p.value", "AB.p.value")

    if (use_bootstrap) {
      ci_bounds <- apply(results, 2, function(x) {
        valid_x <- x[!is.na(x)]
        if (length(valid_x) < 2) {
          return(c(NA_real_, NA_real_))
        }
        stats$quantile(
          valid_x, c(0.025, 0.975), na.rm = TRUE
        )
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
      data.frame(
        Effect = effect_labels,
        Q.Statistic = signif(
          c(results$Qa[1], results$Qb[1], results$Qab[1]),
          3
        ),
        p.value = signif(
          c(
            results$A.p.value[1],
            results$B.p.value[1],
            results$AB.p.value[1]
          ),
          3
        ),
        stringsAsFactors = FALSE
      )
    }
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
                          boot_sample_size = NULL) {
  rhino$log$info(
    "t2way: starting for measure='{measure_col}',",
    " factors='{x_axis[1]}' * '{x_axis[2]}',",
    " bootstrap={use_bootstrap}"
  )

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
    if (length(x_axis) != 3) {
      return(error_handling$simple_error(
        message = paste0(
          "t3way requires exactly three ",
          "grouping variables."
        ),
        operation_name = "t3way_validate",
        context = list(
          n_grouping_vars = length(x_axis)
        )
      ))
    }
    for (i in seq_along(x_axis)) {
      n_levels <- length(unique(df[[x_axis[i]]]))
      if (n_levels < 2) {
        return(error_handling$simple_error(
          message = paste0(
            "t3way requires at least 2 levels in '",
            x_axis[i], "', found ", n_levels, "."
          ),
          operation_name = "t3way_validate",
          context = list(
            factor = x_axis[i],
            n_levels = n_levels
          )
        ))
      }
    }
    NULL
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
    effect_labels <- c(
      x_axis[1], x_axis[2], x_axis[3],
      paste0(x_axis[1], ":", x_axis[2]),
      paste0(x_axis[1], ":", x_axis[3]),
      paste0(x_axis[2], ":", x_axis[3]),
      paste0(
        x_axis[1], ":", x_axis[2], ":", x_axis[3]
      )
    )

    q_cols <- c(
      "Qa", "Qb", "Qc",
      "Qab", "Qac", "Qbc", "Qabc"
    )
    p_cols <- c(
      "A.p.value", "B.p.value", "C.p.value",
      "AB.p.value", "AC.p.value", "BC.p.value",
      "ABC.p.value"
    )

    if (use_bootstrap) {
      ci_bounds <- apply(results, 2, function(x) {
        valid_x <- x[!is.na(x)]
        if (length(valid_x) < 2) {
          return(c(NA_real_, NA_real_))
        }
        stats$quantile(
          valid_x, c(0.025, 0.975), na.rm = TRUE
        )
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
      data.frame(
        Effect = effect_labels,
        Q.Statistic = signif(
          c(
            results$Qa[1], results$Qb[1],
            results$Qc[1], results$Qab[1],
            results$Qac[1], results$Qbc[1],
            results$Qabc[1]
          ),
          3
        ),
        p.value = signif(
          c(
            results$A.p.value[1],
            results$B.p.value[1],
            results$C.p.value[1],
            results$AB.p.value[1],
            results$AC.p.value[1],
            results$BC.p.value[1],
            results$ABC.p.value[1]
          ),
          3
        ),
        stringsAsFactors = FALSE
      )
    }
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
                          boot_sample_size = NULL) {
  rhino$log$info(
    "t3way: starting for measure='{measure_col}',",
    " factors='{x_axis[1]}' * '{x_axis[2]}'",
    " * '{x_axis[3]}',",
    " bootstrap={use_bootstrap}"
  )

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
