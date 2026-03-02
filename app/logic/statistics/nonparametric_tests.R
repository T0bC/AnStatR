box::use(
  rhino,
  stats,
)

box::use(
  app/logic/error_handling,
  app/logic/statistics/omnibus,
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
    if (length(x_axis) != 1) {
      return(error_handling$simple_error(
        message = paste0(
          "Kruskal-Wallis requires exactly one ",
          "grouping variable."
        ),
        operation_name = "kruskal1way_validate",
        context = list(
          n_grouping_vars = length(x_axis)
        )
      ))
    }
    n_groups <- length(unique(df[[x_axis[1]]]))
    if (n_groups < 2) {
      return(error_handling$simple_error(
        message = paste0(
          "Kruskal-Wallis requires at least 2 groups, ",
          "found ", n_groups, "."
        ),
        operation_name = "kruskal1way_validate",
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
                                boot_sample_size = NULL) {
  rhino$log$info(
    "kruskal1way: starting for measure='{measure_col}',",
    " grouping='{x_axis[1]}'"
  )

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
    if (length(x_axis) != 2) {
      return(error_handling$simple_error(
        message = paste0(
          "Two-way ART requires exactly two ",
          "grouping variables."
        ),
        operation_name = "art2way_validate",
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
            "Two-way ART requires at least 2 ",
            "levels in '", x_axis[i],
            "', found ", n_levels, "."
          ),
          operation_name = "art2way_validate",
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
    # Check for missing values in response variable (ARTool is sensitive to NAs)
    response_var <- all.vars(formula_obj)[1]
    if (any(is.na(conversion$data[[response_var]]))) {
      n_missing <- sum(is.na(conversion$data[[response_var]]))
      stop(error_handling$simple_error(
        message = paste0(
          "ART requires complete data. Found ", n_missing,
          " missing values in the response variable."
        ),
        operation_name = "art2way_validate",
        context = list(
          n_missing = n_missing,
          response_var = response_var
        )
      )$message)
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
                            boot_sample_size = NULL) {
  rhino$log$info(
    "art2way: starting for measure='{measure_col}',",
    " factors='{x_axis[1]}' * '{x_axis[2]}'"
  )

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
    if (length(x_axis) != 3) {
      return(error_handling$simple_error(
        message = paste0(
          "Three-way ART requires exactly three ",
          "grouping variables."
        ),
        operation_name = "art3way_validate",
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
            "Three-way ART requires at least 2 ",
            "levels in '", x_axis[i],
            "', found ", n_levels, "."
          ),
          operation_name = "art3way_validate",
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
    # Check for missing values in response variable (ARTool is sensitive to NAs)
    response_var <- all.vars(formula_obj)[1]
    if (any(is.na(conversion$data[[response_var]]))) {
      n_missing <- sum(is.na(conversion$data[[response_var]]))
      stop(error_handling$simple_error(
        message = paste0(
          "ART requires complete data. Found ", n_missing,
          " missing values in the response variable."
        ),
        operation_name = "art3way_validate",
        context = list(
          n_missing = n_missing,
          response_var = response_var
        )
      )$message)
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
                            boot_sample_size = NULL) {
  rhino$log$info(
    "art3way: starting for measure='{measure_col}',",
    " factors='{x_axis[1]}' * '{x_axis[2]}'",
    " * '{x_axis[3]}'"
  )

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
