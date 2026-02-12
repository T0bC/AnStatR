box::use(
  rhino,
  stats,
)

box::use(
  app/logic/error_handling,
  app/logic/statistics/omnibus,
)

# =============================================================================
# Parametric ANOVA tests using classical F-tests.
# Currently: one-way ANOVA, two-way ANOVA.
# No Shiny dependencies allowed in this file.
# =============================================================================

# =============================================================================
# One-Way Parametric ANOVA
# =============================================================================

#' One-way ANOVA configuration
#'
#' Config object consumed by omnibus$run_omnibus_test().
#' Defines all hooks for the classical one-way ANOVA (aov).
#' Returns an ANOVA table with Effect, Df, SS, MS, F, p.value.
anova1way_config <- list(
  name = "anova1way",

  result_cols = c("Df", "SS", "MS", "F_statistic", "p_value"),

  validate = function(df, x_axis) {
    if (length(x_axis) != 1) {
      return(error_handling$simple_error(
        message = paste0(
          "One-way ANOVA requires exactly one ",
          "grouping variable."
        ),
        operation_name = "anova1way_validate",
        context = list(
          n_grouping_vars = length(x_axis)
        )
      ))
    }
    n_groups <- length(unique(df[[x_axis[1]]]))
    if (n_groups < 2) {
      return(error_handling$simple_error(
        message = paste0(
          "One-way ANOVA requires at least 2 groups, ",
          "found ", n_groups, "."
        ),
        operation_name = "anova1way_validate",
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
      test_type = "parametric_anova"
    )
  },

  build_formula = function(measure_col, x_axis) {
    stats$as.formula(
      paste0("`", measure_col, "` ~ `", x_axis[1], "`")
    )
  },

  run_test = function(formula_obj, data, tr_value) {
    # tr_value is ignored for parametric tests
    vars <- all.vars(formula_obj)[-1]
    conversion <- omnibus$safe_factor_conversion(data, vars)
    if (!conversion$success) {
      stop(conversion$error$message)
    }
    model <- stats$aov(formula_obj, data = conversion$data)
    anova_table <- summary(model)[[1]]
    # Return the full summary table — extract_results picks the row
    anova_table
  },

  extract_results = function(out) {
    # out is the summary(aov()) table
    # First row is the factor effect, last row is Residuals
    # Extract the factor row only
    factor_row <- out[1, ]
    c(
      factor_row[["Df"]],
      factor_row[["Sum Sq"]],
      factor_row[["Mean Sq"]],
      factor_row[["F value"]],
      factor_row[["Pr(>F)"]]
    )
  },

  format_results = function(results, x_axis, use_bootstrap) {
    # Parametric tests do not support bootstrap —
    # always format as a single-row table
    data.frame(
      Effect = x_axis[1],
      Df = as.integer(results$Df[1]),
      SS = signif(results$SS[1], 3),
      MS = signif(results$MS[1], 3),
      F.Statistic = signif(results$F_statistic[1], 3),
      p.value = signif(results$p_value[1], 3),
      stringsAsFactors = FALSE
    )
  }
)


#' Perform One-Way Parametric ANOVA
#'
#' Uses stats::aov() for classical one-way ANOVA.
#' Bootstrap and trim parameters are accepted for interface
#' consistency but ignored.
#'
#' @param df Data frame containing the data
#' @param x_axis Character, single grouping column name
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, ignored for parametric tests
#' @param use_bootstrap Logical, ignored (always FALSE)
#' @param boot_samples Integer, ignored
#' @param boot_sample_size Integer or NULL, ignored
#' @return Data frame with ANOVA results, or structured app_error
#' @export
perform_anova1way <- function(df, x_axis, measure_col,
                               tr_value = 0,
                               use_bootstrap = FALSE,
                               boot_samples = 599,
                               boot_sample_size = NULL) {
  rhino$log$info(
    "anova1way: starting for measure='{measure_col}',",
    " grouping='{x_axis[1]}'"
  )

  # Force bootstrap off for parametric tests
  omnibus$run_omnibus_test(
    df = df,
    x_axis = x_axis,
    measure_col = measure_col,
    tr_value = tr_value,
    use_bootstrap = FALSE,
    boot_samples = 1,
    boot_sample_size = NULL,
    config = anova1way_config
  )
}


# =============================================================================
# Two-Way Parametric ANOVA
# =============================================================================

#' Two-way ANOVA configuration
#'
#' Config object consumed by omnibus$run_omnibus_test().
#' Defines all hooks for the classical two-way ANOVA (aov).
#' Returns an ANOVA table with rows for main effects (A, B)
#' and interaction (A:B).
anova2way_config <- list(
  name = "anova2way",

  result_cols = c(
    "Df_A", "SS_A", "MS_A", "F_A", "p_A",
    "Df_B", "SS_B", "MS_B", "F_B", "p_B",
    "Df_AB", "SS_AB", "MS_AB", "F_AB", "p_AB"
  ),

  validate = function(df, x_axis) {
    if (length(x_axis) != 2) {
      return(error_handling$simple_error(
        message = paste0(
          "Two-way ANOVA requires exactly two ",
          "grouping variables."
        ),
        operation_name = "anova2way_validate",
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
            "Two-way ANOVA requires at least 2 ",
            "levels in '", x_axis[i],
            "', found ", n_levels, "."
          ),
          operation_name = "anova2way_validate",
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
      test_type = "parametric_anova"
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
    model <- stats$aov(formula_obj, data = conversion$data)
    summary(model)[[1]]
  },

  extract_results = function(out) {
    # Rows: factor A, factor B, A:B, Residuals
    # Extract first 3 rows (all except Residuals)
    row_a <- out[1, ]
    row_b <- out[2, ]
    row_ab <- out[3, ]
    c(
      row_a[["Df"]], row_a[["Sum Sq"]], row_a[["Mean Sq"]],
      row_a[["F value"]], row_a[["Pr(>F)"]],
      row_b[["Df"]], row_b[["Sum Sq"]], row_b[["Mean Sq"]],
      row_b[["F value"]], row_b[["Pr(>F)"]],
      row_ab[["Df"]], row_ab[["Sum Sq"]], row_ab[["Mean Sq"]],
      row_ab[["F value"]], row_ab[["Pr(>F)"]]
    )
  },

  format_results = function(results, x_axis, use_bootstrap) {
    effect_labels <- c(
      x_axis[1], x_axis[2],
      paste0(x_axis[1], ":", x_axis[2])
    )
    data.frame(
      Effect = effect_labels,
      Df = as.integer(c(
        results$Df_A[1], results$Df_B[1],
        results$Df_AB[1]
      )),
      SS = signif(c(
        results$SS_A[1], results$SS_B[1],
        results$SS_AB[1]
      ), 3),
      MS = signif(c(
        results$MS_A[1], results$MS_B[1],
        results$MS_AB[1]
      ), 3),
      F.Statistic = signif(c(
        results$F_A[1], results$F_B[1],
        results$F_AB[1]
      ), 3),
      p.value = signif(c(
        results$p_A[1], results$p_B[1],
        results$p_AB[1]
      ), 3),
      stringsAsFactors = FALSE
    )
  }
)


#' Perform Two-Way Parametric ANOVA
#'
#' Uses stats::aov() for classical two-way ANOVA with interaction.
#' Returns main effects (A, B) and interaction (A:B).
#' Bootstrap and trim parameters are accepted for interface
#' consistency but ignored.
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of exactly two grouping column names
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, ignored for parametric tests
#' @param use_bootstrap Logical, ignored (always FALSE)
#' @param boot_samples Integer, ignored
#' @param boot_sample_size Integer or NULL, ignored
#' @return Data frame with ANOVA results, or structured app_error
#' @export
perform_anova2way <- function(df, x_axis, measure_col,
                               tr_value = 0,
                               use_bootstrap = FALSE,
                               boot_samples = 599,
                               boot_sample_size = NULL) {
  rhino$log$info(
    "anova2way: starting for measure='{measure_col}',",
    " factors='{x_axis[1]}' * '{x_axis[2]}'"
  )

  omnibus$run_omnibus_test(
    df = df,
    x_axis = x_axis,
    measure_col = measure_col,
    tr_value = tr_value,
    use_bootstrap = FALSE,
    boot_samples = 1,
    boot_sample_size = NULL,
    config = anova2way_config
  )
}
