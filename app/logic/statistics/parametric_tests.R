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
# Currently: one-way ANOVA.
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
