box::use(
  rhino,
  stats,
)

box::use(
  app/logic/shared/error_handling,
  app/logic/statistics/omnibus,
  app/logic/statistics/validation_utils,
)

box::use(
  ARTool[art]
)

# =============================================================================
# Non-parametric tests: Kruskal-Wallis (1-way), ART (2-way, 3-way).
# No Shiny dependencies allowed in this file.
# =============================================================================


# =============================================================================
# Kruskal-Wallis — One-Way Non-Parametric Test
# =============================================================================

#' Kruskal-Wallis one-way test configuration
#'
#' Config object consumed by omnibus$run_omnibus_test().
#' Defines all hooks for the Kruskal-Wallis rank-sum test.
kruskal1way_config <- list(
  name = "kruskal1way",

  result_cols = c("Df", "H_statistic", "p_value"),

  validate = function(df, x_axis) {
    validation_utils$validate_n_way(
      df, x_axis, 1, "Kruskal-Wallis", "kruskal1way_validate"
    )
  },

  build_context = function(df, x_axis, measure_col,
                           tr_value, use_bootstrap) {
    list(
      measure = measure_col,
      grouping = x_axis[1],
      n_groups = length(unique(df[[x_axis[1]]])),
      n_observations = nrow(df),
      test_type = "nonparametric"
    )
  },

  build_formula = function(measure_col, x_axis) {
    stats$as.formula(
      paste0("`", measure_col, "` ~ `", x_axis[1], "`")
    )
  },

  run_test = function(formula_obj, data, tr_value) {
    # tr_value is ignored for non-parametric tests
    vars <- all.vars(formula_obj)[-1]
    conversion <- omnibus$safe_factor_conversion(data, vars)
    if (!conversion$success) {
      stop(conversion$error$message)
    }
    stats$kruskal.test(
      formula = formula_obj,
      data = conversion$data
    )
  },

  extract_results = function(out) {
    c(out$parameter[["df"]], out$statistic[["Kruskal-Wallis chi-squared"]], out$p.value)
  },

  format_results = function(results, x_axis, use_bootstrap) {
    data.frame(
      Effect = x_axis[1],
      Df = as.integer(results$Df[1]),
      H.Statistic = signif(results$H_statistic[1], 3),
      p.value = signif(results$p_value[1], 3),
      stringsAsFactors = FALSE
    )
  }
)


#' Perform One-Way Kruskal-Wallis Test
#'
#' Uses stats::kruskal.test() for non-parametric one-way comparison.
#' Bootstrap and trim parameters are accepted for interface
#' consistency but ignored.
#'
#' @param df Data frame containing the data
#' @param x_axis Character, single grouping column name
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, ignored for non-parametric tests
#' @param use_bootstrap Logical, ignored (always FALSE)
#' @param boot_samples Integer, ignored
#' @param boot_sample_size Integer or NULL, ignored
#' @return Data frame with test results, or structured app_error
#' @export
perform_kruskal1way <- function(df, x_axis, measure_col,
                                tr_value = 0,
                                use_bootstrap = FALSE,
                                boot_samples = 599,
                                boot_sample_size = NULL,
                                is_rm = FALSE,
                                id_col = NULL,
                                within_col = NULL) {
  rhino$log$info(
    "kruskal1way: starting for measure='{measure_col}',",
    " grouping='{x_axis[1]}', rm={is_rm}"
  )

  if (isTRUE(is_rm) && !is.null(id_col) && !is.null(within_col)) {
    return(perform_rm_nonparametric(
      df = df, x_axis = x_axis, measure_col = measure_col,
      id_col = id_col, within_col = within_col
    ))
  }

  # Force bootstrap off for non-parametric tests
  omnibus$run_omnibus_test(
    df = df,
    x_axis = x_axis,
    measure_col = measure_col,
    tr_value = tr_value,
    use_bootstrap = FALSE,
    boot_samples = 1,
    boot_sample_size = NULL,
    config = kruskal1way_config
  )
}


# =============================================================================
# ART Two-Way — Non-Parametric Factorial Test
# =============================================================================

#' Helper: run ARTool art() + anova() in globalenv
#'
#' ARTool::art() uses formula interface internally and needs
#' access to stats::model.frame etc. The anova.art S3 method
#' also requires ARTool to be attached for dispatch to work.
#' Running both steps inside a single eval in a child of
#' globalenv() with requireNamespace ensures proper S3
#' method registration.
#'
#' @param formula_obj Formula object
#' @param data Data frame
#' @return anova data frame from ART model
run_art_anova <- function(formula_obj, data) {
  env <- new.env(parent = globalenv())
  env$formula_obj <- formula_obj
  env$data <- data
  env$art <- art
  env$anova_art <- get("anova.art", envir = asNamespace("ARTool"))
  eval(quote({
    art_model <- art(formula_obj, data = data)
    anova_art(art_model)
  }), envir = env)
}


#' Two-way ART test configuration
#'
#' Config object consumed by omnibus$run_omnibus_test().
#' Defines all hooks for the two-way Aligned Rank Transform.
#' Returns main effects (A, B) and interaction (AB).
art2way_config <- list(
  name = "art2way",

  result_cols = c(
    "Df_A", "Df.res_A", "F_A", "p_A",
    "Df_B", "Df.res_B", "F_B", "p_B",
    "Df_AB", "Df.res_AB", "F_AB", "p_AB"
  ),

  validate = function(df, x_axis) {
    validation_utils$validate_n_way(
      df, x_axis, 2, "Two-way ART", "art2way_validate"
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
      test_type = "nonparametric"
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
    # tr_value is ignored for non-parametric tests
    vars <- all.vars(formula_obj)[-1]
    conversion <- omnibus$safe_factor_conversion(data, vars)
    if (!conversion$success) {
      stop(conversion$error$message)
    }
    # Remove rows with missing values in response variable (ARTool cannot handle NAs)
    response_var <- all.vars(formula_obj)[1]
    na_mask <- is.na(conversion$data[[response_var]])
    if (any(na_mask)) {
      n_missing <- sum(na_mask)
      rhino$log$warn(
        "art2way: Dropping {n_missing} row(s) with NA in '{response_var}'",
        " ({nrow(conversion$data)} -> {nrow(conversion$data) - n_missing} rows)"
      )
      conversion$data <- conversion$data[!na_mask, , drop = FALSE]
    }
    
    # Check for balanced design (ARTool requires balanced designs)
    factor_vars <- all.vars(formula_obj)[-1]
    cell_counts <- table(conversion$data[factor_vars])
    min_cell_size <- min(cell_counts)
    max_cell_size <- max(cell_counts)
    if (min_cell_size < 3) {
      stop(error_handling$simple_error(
        message = paste0(
          "ART requires a balanced design with at least 3 observations per cell. ",
          "Found cells with ", min_cell_size, " to ", max_cell_size, " observations."
        ),
        operation_name = "art2way_validate",
        context = list(
          min_cell_size = min_cell_size,
          max_cell_size = max_cell_size,
          factor_combinations = length(cell_counts)
        )
      )$message)
    }
    
    run_art_anova(formula_obj, conversion$data)
  },

  extract_results = function(out) {
    # out is the anova table from ART
    # Rows: factor A, factor B, A:B
    c(
      out[1, "Df"], out[1, "Df.res"],
      out[1, "F value"], out[1, "Pr(>F)"],
      out[2, "Df"], out[2, "Df.res"],
      out[2, "F value"], out[2, "Pr(>F)"],
      out[3, "Df"], out[3, "Df.res"],
      out[3, "F value"], out[3, "Pr(>F)"]
    )
  },

  format_results = function(results, x_axis, use_bootstrap) {
    effect_labels <- c(
      x_axis[1], x_axis[2],
      paste0(x_axis[1], ":", x_axis[2])
    )

    suffixes <- c("_A", "_B", "_AB")

    data.frame(
      Effect = effect_labels,
      Df = as.integer(vapply(suffixes, function(s) {
        results[[paste0("Df", s)]][1]
      }, numeric(1))),
      Df.res = as.integer(vapply(suffixes, function(s) {
        results[[paste0("Df.res", s)]][1]
      }, numeric(1))),
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


#' Perform Two-Way Non-Parametric ART Test
#'
#' Uses ARTool::art() + anova() for the Aligned Rank Transform.
#' Returns main effects (A, B) and interaction (A:B) with
#' F statistics and p-values. Bootstrap and trim parameters are
#' accepted for interface consistency but ignored.
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of exactly two grouping column names
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, ignored for non-parametric tests
#' @param use_bootstrap Logical, ignored (always FALSE)
#' @param boot_samples Integer, ignored
#' @param boot_sample_size Integer or NULL, ignored
#' @return Data frame with test results, or structured app_error
#' @export
perform_art2way <- function(df, x_axis, measure_col,
                            tr_value = 0,
                            use_bootstrap = FALSE,
                            boot_samples = 599,
                            boot_sample_size = NULL,
                            is_rm = FALSE,
                            id_col = NULL,
                            within_col = NULL) {
  rhino$log$info(
    "art2way: starting for measure='{measure_col}',",
    " factors='{x_axis[1]}' * '{x_axis[2]}', rm={is_rm}"
  )

  if (isTRUE(is_rm) && !is.null(id_col) && !is.null(within_col)) {
    return(perform_rm_nonparametric(
      df = df, x_axis = x_axis, measure_col = measure_col,
      id_col = id_col, within_col = within_col
    ))
  }

  # Force bootstrap off for non-parametric tests
  omnibus$run_omnibus_test(
    df = df,
    x_axis = x_axis,
    measure_col = measure_col,
    tr_value = tr_value,
    use_bootstrap = FALSE,
    boot_samples = 1,
    boot_sample_size = NULL,
    config = art2way_config
  )
}


# =============================================================================
# ART Three-Way — Non-Parametric Factorial Test
# =============================================================================

#' Three-way ART test configuration
#'
#' Config object consumed by omnibus$run_omnibus_test().
#' Defines all hooks for the three-way Aligned Rank Transform.
#' Returns main effects (A, B, C), two-way interactions (AB, AC, BC),
#' and three-way interaction (ABC).
art3way_config <- list(
  name = "art3way",

  result_cols = c(
    "Df_A", "Df.res_A", "F_A", "p_A",
    "Df_B", "Df.res_B", "F_B", "p_B",
    "Df_C", "Df.res_C", "F_C", "p_C",
    "Df_AB", "Df.res_AB", "F_AB", "p_AB",
    "Df_AC", "Df.res_AC", "F_AC", "p_AC",
    "Df_BC", "Df.res_BC", "F_BC", "p_BC",
    "Df_ABC", "Df.res_ABC", "F_ABC", "p_ABC"
  ),

  validate = function(df, x_axis) {
    validation_utils$validate_n_way(
      df, x_axis, 3, "Three-way ART", "art3way_validate"
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
      test_type = "nonparametric"
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
    # tr_value is ignored for non-parametric tests
    vars <- all.vars(formula_obj)[-1]
    conversion <- omnibus$safe_factor_conversion(data, vars)
    if (!conversion$success) {
      stop(conversion$error$message)
    }
    # Remove rows with missing values in response variable (ARTool cannot handle NAs)
    response_var <- all.vars(formula_obj)[1]
    na_mask <- is.na(conversion$data[[response_var]])
    if (any(na_mask)) {
      n_missing <- sum(na_mask)
      rhino$log$warn(
        "art3way: Dropping {n_missing} row(s) with NA in '{response_var}'",
        " ({nrow(conversion$data)} -> {nrow(conversion$data) - n_missing} rows)"
      )
      conversion$data <- conversion$data[!na_mask, , drop = FALSE]
    }
    
    # Check for balanced design (ARTool requires balanced designs)
    factor_vars <- all.vars(formula_obj)[-1]
    cell_counts <- table(conversion$data[factor_vars])
    min_cell_size <- min(cell_counts)
    max_cell_size <- max(cell_counts)
    if (min_cell_size < 3) {
      stop(error_handling$simple_error(
        message = paste0(
          "ART requires a balanced design with at least 3 observations per cell. ",
          "Found cells with ", min_cell_size, " to ", max_cell_size, " observations."
        ),
        operation_name = "art3way_validate",
        context = list(
          min_cell_size = min_cell_size,
          max_cell_size = max_cell_size,
          factor_combinations = length(cell_counts)
        )
      )$message)
    }
    
    run_art_anova(formula_obj, conversion$data)
  },

  extract_results = function(out) {
    # out is the anova table from ART
    # Rows: A, B, C, A:B, A:C, B:C, A:B:C
    extract_row <- function(row) {
      c(
        out[row, "Df"], out[row, "Df.res"],
        out[row, "F value"], out[row, "Pr(>F)"]
      )
    }
    c(
      extract_row(1), extract_row(2), extract_row(3),
      extract_row(4), extract_row(5), extract_row(6),
      extract_row(7)
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
      Df.res = as.integer(vapply(suffixes, function(s) {
        results[[paste0("Df.res", s)]][1]
      }, numeric(1))),
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


#' Perform Three-Way Non-Parametric ART Test
#'
#' Uses ARTool::art() + anova() for the Aligned Rank Transform.
#' Returns main effects (A, B, C), two-way interactions (AB, AC, BC),
#' and three-way interaction (ABC) with F statistics and p-values.
#' Bootstrap and trim parameters are accepted for interface
#' consistency but ignored.
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of exactly three grouping
#'   column names
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, ignored for non-parametric tests
#' @param use_bootstrap Logical, ignored (always FALSE)
#' @param boot_samples Integer, ignored
#' @param boot_sample_size Integer or NULL, ignored
#' @return Data frame with test results, or structured app_error
#' @export
perform_art3way <- function(df, x_axis, measure_col,
                            tr_value = 0,
                            use_bootstrap = FALSE,
                            boot_samples = 599,
                            boot_sample_size = NULL,
                            is_rm = FALSE,
                            id_col = NULL,
                            within_col = NULL) {
  rhino$log$info(
    "art3way: starting for measure='{measure_col}',",
    " factors='{x_axis[1]}' * '{x_axis[2]}'",
    " * '{x_axis[3]}', rm={is_rm}"
  )

  if (isTRUE(is_rm) && !is.null(id_col) && !is.null(within_col)) {
    return(perform_rm_nonparametric(
      df = df, x_axis = x_axis, measure_col = measure_col,
      id_col = id_col, within_col = within_col
    ))
  }

  # Force bootstrap off for non-parametric tests
  omnibus$run_omnibus_test(
    df = df,
    x_axis = x_axis,
    measure_col = measure_col,
    tr_value = tr_value,
    use_bootstrap = FALSE,
    boot_samples = 1,
    boot_sample_size = NULL,
    config = art3way_config
  )
}


# =============================================================================
# Repeated Measures Non-Parametric Tests
# =============================================================================

#' Perform Repeated Measures Non-Parametric Test
#'
#' 1-way within: Uses Friedman rank-sum test (stats::friedman.test).
#' Mixed (between x within): Uses ARTool::art() with Error(ID) term.
#'
#' @param df Data frame (long format)
#' @param x_axis Character vector of grouping columns
#' @param measure_col Character, measurement column name
#' @param id_col Character, subject ID column name
#' @param within_col Character, within-subject factor column name
#' @return Data frame with test results, or structured app_error
#' @export
perform_rm_nonparametric <- function(df, x_axis, measure_col,
                                      id_col, within_col) {
  rhino$log$info(
    "rm_nonparametric: starting for measure='{measure_col}',",
    " id='{id_col}', within='{within_col}'"
  )

  between_cols <- setdiff(x_axis, within_col)

  error_context <- list(
    measure = measure_col,
    id_col = id_col,
    within_col = within_col,
    between_cols = paste(between_cols, collapse = ", "),
    n_observations = nrow(df),
    test_type = "nonparametric_rm"
  )

  test_result <- error_handling$safe_execute(
    expr = {
      # Validate columns
      if (!id_col %in% names(df)) {
        stop(paste0("ID column '", id_col, "' not found."))
      }
      if (!within_col %in% names(df)) {
        stop(paste0(
          "Within-subject factor '", within_col,
          "' not found."
        ))
      }

      df[[id_col]] <- as.factor(df[[id_col]])
      df[[within_col]] <- as.factor(df[[within_col]])
      for (bc in between_cols) {
        df[[bc]] <- as.factor(df[[bc]])
      }

      # Remove NAs in response
      na_mask <- is.na(df[[measure_col]])
      if (any(na_mask)) {
        rhino$log$warn(
          "rm_nonparametric: Dropping {sum(na_mask)}",
          " row(s) with NA in '{measure_col}'"
        )
        df <- df[!na_mask, , drop = FALSE]
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

      if (length(between_cols) == 0) {
        # Pure within-subject: Friedman test
        # Requires a matrix: rows = subjects, cols = conditions
        wide_df <- stats$reshape(
          df[, c(id_col, within_col, measure_col)],
          idvar = id_col,
          timevar = within_col,
          direction = "wide"
        )
        measure_cols_wide <- setdiff(names(wide_df), id_col)
        y_matrix <- as.matrix(
          wide_df[, measure_cols_wide, drop = FALSE]
        )

        friedman_result <- stats$friedman.test(y_matrix)

        data.frame(
          Effect = within_col,
          Df = as.integer(
            friedman_result$parameter[["df"]]
          ),
          Chi.Sq.Statistic = signif(
            friedman_result$statistic[["Friedman chi-squared"]], 3
          ),
          p.value = signif(friedman_result$p.value, 3),
          stringsAsFactors = FALSE
        )
      } else {
        # Mixed design: ART with Error(ID) term
        fixed_terms <- paste0(
          "`", c(between_cols, within_col), "`",
          collapse = " * "
        )
        formula_str <- paste0(
          "`", measure_col, "` ~ ", fixed_terms,
          " + Error(`", id_col, "`)"
        )
        formula_obj <- stats$as.formula(formula_str)

        art_result <- run_art_anova(formula_obj, df)

        # Reconstruct effect names from Term and Error columns.
        # ARTool splits repeated-measures ANOVA by error strata:
        # - between-subject effects appear in the subject ID stratum
        # - within-subject effects appear in the "Within" stratum
        # - interactions (between x within) also appear in the "Within"
        #   stratum but with the between-subject factor as the Term
        raw_terms <- as.character(art_result$Term)
        error_strata <- as.character(art_result$Error)
        id_stratum <- id_col

        effect_names <- vapply(seq_along(raw_terms), function(i) {
          term <- raw_terms[i]
          if (term %in% between_cols && error_strata[i] != id_stratum) {
            # Between-subject term in within stratum = interaction
            paste(term, within_col, sep = ":")
          } else {
            term
          }
        }, character(1))

        # Reorder: between effects, within effects, then interactions
        is_interaction <- grepl(":", effect_names)
        is_between <- effect_names %in% between_cols
        is_within <- effect_names %in% within_col
        ordered_idx <- order(!is_between, !is_within, is_interaction)
        art_result <- art_result[ordered_idx, , drop = FALSE]
        effect_names <- effect_names[ordered_idx]

        data.frame(
          Effect = effect_names,
          Df = as.integer(art_result[, "Df"]),
          Df.res = as.integer(art_result[, "Df.res"]),
          F.Statistic = signif(
            art_result[, "F value"], 3
          ),
          p.value = signif(art_result[, "Pr(>F)"], 3),
          stringsAsFactors = FALSE,
          row.names = NULL
        )
      }
    },
    operation_name = "rm_nonparametric",
    context = error_context,
    error_parser = error_handling$stat_error_parser
  )

  if (!test_result$success) return(test_result$error)

  test_result$result
}
