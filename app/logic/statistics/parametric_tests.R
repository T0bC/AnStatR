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
# Parametric ANOVA tests using classical F-tests.
# Currently: one-way ANOVA, two-way ANOVA, three-way ANOVA.
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
    validation_utils$validate_n_way(
      df, x_axis, 1, "One-way ANOVA", "anova1way_validate"
    )
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
                               boot_sample_size = NULL,
                               is_rm = FALSE,
                               id_col = NULL,
                               within_col = NULL) {
  rhino$log$info(
    "anova1way: starting for measure='{measure_col}',",
    " grouping='{x_axis[1]}', rm={is_rm}"
  )

  if (isTRUE(is_rm) && !is.null(id_col) && !is.null(within_col)) {
    return(perform_rm_anova(
      df = df, x_axis = x_axis, measure_col = measure_col,
      id_col = id_col, within_col = within_col
    ))
  }

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
    validation_utils$validate_n_way(
      df, x_axis, 2, "Two-way ANOVA", "anova2way_validate"
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
                               boot_sample_size = NULL,
                               is_rm = FALSE,
                               id_col = NULL,
                               within_col = NULL) {
  rhino$log$info(
    "anova2way: starting for measure='{measure_col}',",
    " factors='{x_axis[1]}' * '{x_axis[2]}', rm={is_rm}"
  )

  if (isTRUE(is_rm) && !is.null(id_col) && !is.null(within_col)) {
    return(perform_rm_anova(
      df = df, x_axis = x_axis, measure_col = measure_col,
      id_col = id_col, within_col = within_col
    ))
  }

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


# =============================================================================
# Three-Way Parametric ANOVA
# =============================================================================

#' Three-way ANOVA configuration
#'
#' Config object consumed by omnibus$run_omnibus_test().
#' Defines all hooks for the classical three-way ANOVA (aov).
#' Returns an ANOVA table with rows for main effects (A, B, C),
#' two-way interactions (A:B, A:C, B:C), and three-way
#' interaction (A:B:C).
anova3way_config <- list(
  name = "anova3way",

  result_cols = c(
    "Df_A", "SS_A", "MS_A", "F_A", "p_A",
    "Df_B", "SS_B", "MS_B", "F_B", "p_B",
    "Df_C", "SS_C", "MS_C", "F_C", "p_C",
    "Df_AB", "SS_AB", "MS_AB", "F_AB", "p_AB",
    "Df_AC", "SS_AC", "MS_AC", "F_AC", "p_AC",
    "Df_BC", "SS_BC", "MS_BC", "F_BC", "p_BC",
    "Df_ABC", "SS_ABC", "MS_ABC", "F_ABC", "p_ABC"
  ),

  validate = function(df, x_axis) {
    validation_utils$validate_n_way(
      df, x_axis, 3, "Three-way ANOVA", "anova3way_validate"
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
      test_type = "parametric_anova"
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
    model <- stats$aov(formula_obj, data = conversion$data)
    summary(model)[[1]]
  },

  extract_results = function(out) {
    # Rows: A, B, C, A:B, A:C, B:C, A:B:C, Residuals
    # Extract first 7 rows (all except Residuals)
    extract_row <- function(row) {
      c(
        row[["Df"]], row[["Sum Sq"]],
        row[["Mean Sq"]], row[["F value"]],
        row[["Pr(>F)"]]
      )
    }
    c(
      extract_row(out[1, ]),
      extract_row(out[2, ]),
      extract_row(out[3, ]),
      extract_row(out[4, ]),
      extract_row(out[5, ]),
      extract_row(out[6, ]),
      extract_row(out[7, ])
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

    suffixes <- c(
      "_A", "_B", "_C", "_AB", "_AC", "_BC", "_ABC"
    )

    data.frame(
      Effect = effect_labels,
      Df = as.integer(vapply(suffixes, function(s) {
        results[[paste0("Df", s)]][1]
      }, numeric(1))),
      SS = signif(vapply(suffixes, function(s) {
        results[[paste0("SS", s)]][1]
      }, numeric(1)), 3),
      MS = signif(vapply(suffixes, function(s) {
        results[[paste0("MS", s)]][1]
      }, numeric(1)), 3),
      F.Statistic = signif(vapply(suffixes, function(s) {
        results[[paste0("F", s)]][1]
      }, numeric(1)), 3),
      p.value = signif(vapply(suffixes, function(s) {
        results[[paste0("p", s)]][1]
      }, numeric(1)), 3),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }
)


#' Perform Three-Way Parametric ANOVA
#'
#' Uses stats::aov() for classical three-way ANOVA with all
#' interactions. Returns main effects (A, B, C), two-way
#' interactions (A:B, A:C, B:C), and three-way interaction
#' (A:B:C). Bootstrap and trim parameters are accepted for
#' interface consistency but ignored.
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of exactly three grouping
#'   column names
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, ignored for parametric tests
#' @param use_bootstrap Logical, ignored (always FALSE)
#' @param boot_samples Integer, ignored
#' @param boot_sample_size Integer or NULL, ignored
#' @return Data frame with ANOVA results, or structured app_error
#' @export
perform_anova3way <- function(df, x_axis, measure_col,
                               tr_value = 0,
                               use_bootstrap = FALSE,
                               boot_samples = 599,
                               boot_sample_size = NULL) {
  rhino$log$info(
    "anova3way: starting for measure='{measure_col}',",
    " factors='{x_axis[1]}' * '{x_axis[2]}'",
    " * '{x_axis[3]}'"
  )

  omnibus$run_omnibus_test(
    df = df,
    x_axis = x_axis,
    measure_col = measure_col,
    tr_value = tr_value,
    use_bootstrap = FALSE,
    boot_samples = 1,
    boot_sample_size = NULL,
    config = anova3way_config
  )
}


# =============================================================================
# Repeated Measures Parametric ANOVA
# =============================================================================

#' Perform Repeated Measures Parametric ANOVA
#'
#' Uses stats::aov() with Error(ID/within_factor) for within-subject
#' designs. For mixed designs (between x within), the between-subject
#' factors are included as fixed effects.
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of grouping columns (all factors on X-axis)
#' @param measure_col Character, measurement column name
#' @param id_col Character, subject ID column name
#' @param within_col Character, within-subject factor column name
#' @return Data frame with RM ANOVA results, or structured app_error
#' @export
perform_rm_anova <- function(df, x_axis, measure_col,
                              id_col, within_col) {
  rhino$log$info(
    "rm_anova: starting for measure='{measure_col}',",
    " id='{id_col}', within='{within_col}'"
  )

  error_context <- list(
    measure = measure_col,
    id_col = id_col,
    within_col = within_col,
    x_axis = paste(x_axis, collapse = ", "),
    n_observations = nrow(df),
    test_type = "parametric_rm_anova"
  )

  test_result <- error_handling$safe_execute(
    expr = {
      # Validate RM design
      if (!id_col %in% names(df)) {
        stop(paste0("ID column '", id_col, "' not found in data."))
      }
      if (!within_col %in% names(df)) {
        stop(paste0(
          "Within-subject factor '", within_col,
          "' not found in data."
        ))
      }

      # Convert to factors
      df[[id_col]] <- as.factor(df[[id_col]])
      df[[within_col]] <- as.factor(df[[within_col]])

      # Identify between-subject factors
      between_cols <- setdiff(x_axis, within_col)
      for (bc in between_cols) {
        df[[bc]] <- as.factor(df[[bc]])
      }

      # Validate balanced design: each ID must appear once per
      # within-factor level
      id_within_counts <- table(df[[id_col]], df[[within_col]])
      if (any(id_within_counts != 1)) {
        stop(paste0(
          "Unbalanced repeated measures design. ",
          "Each subject must appear exactly once per ",
          "level of '", within_col, "'."
        ))
      }

      # Build formula with Error() term
      # Pure within-subject: measure ~ within + Error(ID/within)
      # Mixed design: measure ~ between * within + Error(ID/within)
      if (length(between_cols) == 0) {
        formula_str <- paste0(
          "`", measure_col, "` ~ `", within_col,
          "` + Error(`", id_col, "` / `", within_col, "`)"
        )
      } else {
        fixed_terms <- paste0(
          "`", c(between_cols, within_col), "`",
          collapse = " * "
        )
        formula_str <- paste0(
          "`", measure_col, "` ~ ", fixed_terms,
          " + Error(`", id_col, "` / `", within_col, "`)"
        )
      }

      formula_obj <- stats$as.formula(formula_str)
      model <- stats$aov(formula_obj, data = df)
      model_summary <- summary(model)

      # Extract results from all strata
      results_rows <- list()
      for (stratum_name in names(model_summary)) {
        stratum_table <- model_summary[[stratum_name]][[1]]
        # Skip Residuals-only strata
        effect_rows <- rownames(stratum_table)
        for (eff in effect_rows) {
          if (grepl("^Residuals", eff)) next
          row_data <- stratum_table[eff, ]
          results_rows[[length(results_rows) + 1]] <- data.frame(
            Effect = trimws(eff),
            Df = as.integer(row_data[["Df"]]),
            SS = signif(row_data[["Sum Sq"]], 3),
            MS = signif(row_data[["Mean Sq"]], 3),
            F.Statistic = if (!is.na(row_data[["F value"]])) {
              signif(row_data[["F value"]], 3)
            } else {
              NA_real_
            },
            p.value = if (!is.na(row_data[["Pr(>F)"]])) {
              signif(row_data[["Pr(>F)"]], 3)
            } else {
              NA_real_
            },
            stringsAsFactors = FALSE
          )
        }
      }

      if (length(results_rows) == 0) {
        stop("RM ANOVA returned no effect rows.")
      }

      do.call(rbind, results_rows)
    },
    operation_name = "rm_anova",
    context = error_context,
    error_parser = error_handling$stat_error_parser
  )

  if (!test_result$success) return(test_result$error)

  test_result$result
}
