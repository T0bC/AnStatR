box::use(
  dplyr,
  rhino,
  stats,
  WRS2,
)

box::use(
  app/logic/error_handling,
  app/logic/statistics/omnibus,
  app/logic/statistics/customWRS,
  app/logic/statistics/`Rallfun-v43`[cidmulv2_labelled],
)

# =============================================================================
# Robust post-hoc pairwise comparisons: lincon + Cliff's Delta.
# Returns raw p-values only — adjustment is deferred to the combined table.
# No Shiny dependencies allowed in this file.
# =============================================================================


# =============================================================================
# Private helpers
# =============================================================================

#' Validate post-hoc inputs
#'
#' @param df Data frame
#' @param x_axis Grouping columns
#' @return NULL if valid, app_error otherwise
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
#' @param df Data frame
#' @param x_axis Grouping columns
#' @param measure_col Measurement column
#' @param tr_value Trim proportion
#' @param use_bootstrap Logical
#' @return List with context information
build_posthoc_context <- function(df, x_axis, measure_col,
                                  tr_value, use_bootstrap) {
  list(
    measure = measure_col,
    grouping = paste(x_axis, collapse = ", "),
    n_observations = nrow(df),
    trim = tr_value,
    bootstrap = use_bootstrap
  )
}

#' Extract interaction labels from mcp2atm_TM / mcp3atm_TM contrast matrix
#'
#' Each contrast column has +1 and -1 entries. The row names of the
#' contrast matrix are the group labels (from dataWide column names).
#' For each contrast, we find the groups with positive vs negative
#' coefficients and build "GroupA vs. GroupB" labels.
#'
#' @param contrasts Data frame with contrast matrix (rows = groups, cols = contrasts)
#' @return Character vector of interaction labels
extract_contrast_labels <- function(contrasts) {
  group_names <- rownames(contrasts)
  vapply(seq_len(ncol(contrasts)), function(i) {
    col <- contrasts[, i]
    pos_groups <- group_names[col > 0]
    neg_groups <- group_names[col < 0]
    paste(
      paste(pos_groups, collapse = "."),
      "vs.",
      paste(neg_groups, collapse = ".")
    )
  }, character(1))
}

#' Flatten mcp result effects into a data frame with interaction labels
#'
#' @param mcp_result Result from mcp2atm_TM or mcp3atm_TM
#' @return Data frame with Interaction, Lincon.psihat, Lincon.ci.lower,
#'   Lincon.ci.upper, Lincon.p.value
flatten_mcp_effects <- function(mcp_result) {
  effects <- mcp_result$effects
  contrasts <- mcp_result$contrasts

  group_names <- rownames(contrasts)
  all_rows <- list()

  col_offset <- 0
  for (effect_name in names(effects)) {
    eff <- effects[[effect_name]]
    n_contrasts <- length(eff$psihat)

    for (k in seq_len(n_contrasts)) {
      col_idx <- col_offset + k
      col_vals <- contrasts[, col_idx]
      pos_groups <- group_names[col_vals > 0]
      neg_groups <- group_names[col_vals < 0]
      label <- paste(
        paste(pos_groups, collapse = "."),
        "vs.",
        paste(neg_groups, collapse = ".")
      )

      ci <- eff$conf.int
      ci_lower <- if (is.matrix(ci)) ci[k, 1] else ci[1]
      ci_upper <- if (is.matrix(ci)) ci[k, 2] else ci[2]

      all_rows[[length(all_rows) + 1]] <- data.frame(
        Interaction = label,
        Lincon.psihat = eff$psihat[k],
        Lincon.ci.lower = ci_lower,
        Lincon.ci.upper = ci_upper,
        Lincon.p.value = eff$p.value[k],
        stringsAsFactors = FALSE
      )
    }
    col_offset <- col_offset + n_contrasts
  }

  do.call(rbind, all_rows)
}

#' Run single lincon iteration for 1-way design
#'
#' @param sample_data Data frame
#' @param x_axis Grouping columns (length 1)
#' @param measure_col Measurement column
#' @param tr_value Trim proportion
#' @return Data frame with lincon results
run_lincon_1way <- function(sample_data, x_axis, measure_col,
                            tr_value) {
  sample_data[[x_axis[1]]] <- as.factor(sample_data[[x_axis[1]]])
  formula_obj <- stats$as.formula(
    paste0("`", measure_col, "` ~ `", x_axis[1], "`")
  )
  lincon_result <- WRS2$lincon(
    formula = formula_obj,
    data = sample_data,
    tr = tr_value,
    method = "none"
  )
  comp_df <- as.data.frame(lincon_result$comp)
  interaction_labels <- paste(
    lincon_result$fnames[comp_df[, 1]],
    "vs.",
    lincon_result$fnames[comp_df[, 2]]
  )
  data.frame(
    Interaction = interaction_labels,
    Lincon.psihat = comp_df$psihat,
    Lincon.ci.lower = comp_df$ci.lower,
    Lincon.ci.upper = comp_df$ci.upper,
    Lincon.p.value = comp_df$p.value,
    stringsAsFactors = FALSE
  )
}

#' Run single lincon iteration for 2-way design
#'
#' @param sample_data Data frame
#' @param x_axis Grouping columns (length 2)
#' @param measure_col Measurement column
#' @param tr_value Trim proportion
#' @return Data frame with lincon results
run_lincon_2way <- function(sample_data, x_axis, measure_col,
                            tr_value) {
  sample_data[[x_axis[1]]] <- as.factor(sample_data[[x_axis[1]]])
  sample_data[[x_axis[2]]] <- as.factor(sample_data[[x_axis[2]]])
  formula_obj <- stats$as.formula(
    paste0(
      "`", measure_col, "` ~ `",
      x_axis[1], "` * `", x_axis[2], "`"
    )
  )
  mcp_result <- customWRS$mcp2atm_TM(
    formula = formula_obj,
    data = sample_data,
    tr = tr_value
  )
  flatten_mcp_effects(mcp_result)
}

#' Run single lincon iteration for 3-way design
#'
#' @param sample_data Data frame
#' @param x_axis Grouping columns (length 3)
#' @param measure_col Measurement column
#' @param tr_value Trim proportion
#' @return Data frame with lincon results
run_lincon_3way <- function(sample_data, x_axis, measure_col,
                            tr_value) {
  sample_data[[x_axis[1]]] <- as.factor(sample_data[[x_axis[1]]])
  sample_data[[x_axis[2]]] <- as.factor(sample_data[[x_axis[2]]])
  sample_data[[x_axis[3]]] <- as.factor(sample_data[[x_axis[3]]])
  formula_obj <- stats$as.formula(
    paste0(
      "`", measure_col, "` ~ `",
      x_axis[1], "` * `", x_axis[2], "` * `", x_axis[3], "`"
    )
  )
  mcp_result <- customWRS$mcp3atm_TM(
    formula = formula_obj,
    data = sample_data,
    tr = tr_value
  )
  flatten_mcp_effects(mcp_result)
}

#' Run single cliff iteration
#'
#' @param sample_data Data frame
#' @param x_axis Grouping columns
#' @param measure_col Measurement column
#' @return Data frame with cliff results
run_cliff_iteration <- function(sample_data, x_axis,
                                measure_col) {
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

#' Aggregate bootstrap iterations into mean [CI] format
#'
#' @param results_list List of data frames from bootstrap iterations
#' @param value_cols Character vector of numeric column names to aggregate
#' @return Data frame with formatted results
format_posthoc_bootstrap <- function(results_list, value_cols) {
  if (length(results_list) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  unique_interactions <- unique(unlist(lapply(
    results_list,
    function(x) {
      if ("Interaction" %in% names(x) && nrow(x) > 0) {
        x$Interaction
      } else {
        character(0)
      }
    }
  )))

  if (length(unique_interactions) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  result_rows <- lapply(unique_interactions, function(interaction) {
    subset_dfs <- lapply(results_list, function(x) {
      if ("Interaction" %in% names(x) && nrow(x) > 0) {
        x[x$Interaction == interaction, , drop = FALSE]
      } else {
        NULL
      }
    })
    subset_dfs <- subset_dfs[!vapply(
      subset_dfs, is.null, logical(1)
    )]
    if (length(subset_dfs) == 0) return(NULL)

    combined <- do.call(rbind, subset_dfs)

    row <- data.frame(
      Interaction = interaction,
      stringsAsFactors = FALSE
    )
    for (col_name in value_cols) {
      if (col_name %in% names(combined)) {
        vals <- combined[[col_name]]
        valid <- vals[!is.na(vals)]
        if (length(valid) > 0) {
          mean_val <- signif(mean(valid), 3)
          lower <- signif(
            stats$quantile(valid, 0.025, na.rm = TRUE), 3
          )
          upper <- signif(
            stats$quantile(valid, 0.975, na.rm = TRUE), 3
          )
          row[[col_name]] <- paste0(
            mean_val, " [", lower, " - ", upper, "]"
          )
        } else {
          row[[col_name]] <- NA_character_
        }
      } else {
        row[[col_name]] <- NA_character_
      }
    }
    row
  })

  result_rows <- result_rows[!vapply(
    result_rows, is.null, logical(1)
  )]
  if (length(result_rows) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  do.call(rbind, result_rows)
}

#' Normalize interaction strings for consistent matching
#'
#' Extracts group names from "GroupA vs. GroupB" format and creates
#' an alphabetized key for consistent matching between tests.
#'
#' @param df Data frame with Interaction column
#' @return Data frame with added InteractionKey column
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


#' Run lincon on combined groups (1-way style) for multi-way designs
#'
#' Combines multi-factor groups into a single factor using "." separator
#' (matching Cliff's Delta convention) and runs WRS2::lincon.
#' This produces pairwise comparisons compatible with Cliff's output.
#'
#' @param sample_data Data frame
#' @param x_axis Grouping columns
#' @param measure_col Measurement column
#' @param tr_value Trim proportion
#' @return Data frame with lincon results
run_lincon_combined <- function(sample_data, x_axis, measure_col,
                                 tr_value) {
  if (length(x_axis) > 1) {
    sample_data$combinedGroups <- do.call(
      paste, c(sample_data[x_axis], sep = ".")
    )
  } else {
    sample_data$combinedGroups <- sample_data[[x_axis[1]]]
  }
  sample_data$combinedGroups <- as.factor(
    sample_data$combinedGroups
  )
  formula_obj <- stats$as.formula(
    paste0("`", measure_col, "` ~ combinedGroups")
  )
  lincon_result <- WRS2$lincon(
    formula = formula_obj,
    data = sample_data,
    tr = tr_value,
    method = "none"
  )
  comp_df <- as.data.frame(lincon_result$comp)
  interaction_labels <- paste(
    lincon_result$fnames[comp_df[, 1]],
    "vs.",
    lincon_result$fnames[comp_df[, 2]]
  )
  data.frame(
    Interaction = interaction_labels,
    Lincon.psihat = comp_df$psihat,
    Lincon.ci.lower = comp_df$ci.lower,
    Lincon.ci.upper = comp_df$ci.upper,
    Lincon.p.value = comp_df$p.value,
    stringsAsFactors = FALSE
  )
}

#' Perform lincon on combined groups for the combined posthoc table
#'
#' For multi-way designs, combines factors into a single group using "."
#' separator and runs WRS2::lincon (1-way style). This produces pairwise
#' comparisons that match Cliff's Delta output for merging.
#'
#' @param df Data frame
#' @param x_axis Character vector of grouping column(s)
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, trim proportion (0-0.5)
#' @param use_bootstrap Logical
#' @param boot_samples Integer, number of bootstrap samples
#' @param boot_sample_size Integer or NULL
#' @return Data frame with lincon results or app_error
perform_lincon_combined <- function(df, x_axis, measure_col,
                                     tr_value,
                                     use_bootstrap = FALSE,
                                     boot_samples = 599,
                                     boot_sample_size = NULL) {
  rhino$log$info(
    "lincon_combined: starting for measure='{measure_col}',",
    " factors='{paste(x_axis, collapse=\", \")}'"
  )

  validation <- validate_posthoc(df, x_axis)
  if (error_handling$is_app_error(validation)) {
    return(validation)
  }

  boot_params <- omnibus$setup_bootstrap_params(
    df, x_axis, use_bootstrap, boot_samples, boot_sample_size
  )

  error_context <- build_posthoc_context(
    df, x_axis, measure_col, tr_value, use_bootstrap
  )

  test_result <- error_handling$safe_execute(
    expr = {
      results_list <- vector("list", boot_params$n_iterations)
      for (i in seq_len(boot_params$n_iterations)) {
        sample_data <- omnibus$sample_for_iteration(
          df, x_axis, use_bootstrap, boot_params$sample_size
        )
        results_list[[i]] <- run_lincon_combined(
          sample_data, x_axis, measure_col, tr_value
        )
      }
      results_list
    },
    operation_name = "lincon_combined",
    context = error_context,
    error_parser = error_handling$stat_error_parser
  )

  if (!test_result$success) return(test_result$error)

  if (use_bootstrap) {
    format_posthoc_bootstrap(
      test_result$result,
      c(
        "Lincon.psihat", "Lincon.ci.lower",
        "Lincon.ci.upper", "Lincon.p.value"
      )
    )
  } else {
    result_df <- test_result$result[[1]]
    numeric_cols <- c(
      "Lincon.psihat", "Lincon.ci.lower",
      "Lincon.ci.upper", "Lincon.p.value"
    )
    avail <- intersect(numeric_cols, names(result_df))
    result_df[avail] <- lapply(
      result_df[avail], function(x) signif(x, 3)
    )
    result_df
  }
}


# =============================================================================
# Exported functions
# =============================================================================

#' Perform Linear Contrasts (lincon)
#'
#' Pairwise comparisons using trimmed means.
#' 1-way: WRS2::lincon with method="none".
#' 2-way: mcp2atm_TM (structured Factor.A, Factor.B, Factor.AB).
#' 3-way: mcp3atm_TM (structured 7 effect groups).
#' Returns raw p-values only — no adjustment applied.
#'
#' @param df Data frame
#' @param x_axis Character vector of grouping column(s)
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, trim proportion (0-0.5)
#' @param use_bootstrap Logical
#' @param boot_samples Integer, number of bootstrap samples
#' @param boot_sample_size Integer or NULL
#' @return Data frame with contrast results or app_error
#' @export
perform_lincon <- function(df, x_axis, measure_col, tr_value,
                           use_bootstrap = FALSE,
                           boot_samples = 599,
                           boot_sample_size = NULL) {
  rhino$log$info(
    "lincon: starting for measure='{measure_col}',",
    " factors='{paste(x_axis, collapse=\", \")}'"
  )

  validation <- validate_posthoc(df, x_axis)
  if (error_handling$is_app_error(validation)) {
    return(validation)
  }

  boot_params <- omnibus$setup_bootstrap_params(
    df, x_axis, use_bootstrap, boot_samples, boot_sample_size
  )

  error_context <- build_posthoc_context(
    df, x_axis, measure_col, tr_value, use_bootstrap
  )

  n_ways <- length(x_axis)
  run_fn <- if (n_ways == 1) {
    run_lincon_1way
  } else if (n_ways == 2) {
    run_lincon_2way
  } else if (n_ways == 3) {
    run_lincon_3way
  } else {
    return(error_handling$simple_error(
      message = paste0(
        n_ways, "-way lincon is not supported."
      ),
      operation_name = "lincon"
    ))
  }

  test_result <- error_handling$safe_execute(
    expr = {
      results_list <- vector("list", boot_params$n_iterations)
      for (i in seq_len(boot_params$n_iterations)) {
        sample_data <- omnibus$sample_for_iteration(
          df, x_axis, use_bootstrap, boot_params$sample_size
        )
        results_list[[i]] <- run_fn(
          sample_data, x_axis, measure_col, tr_value
        )
      }
      results_list
    },
    operation_name = "lincon",
    context = error_context,
    error_parser = error_handling$stat_error_parser
  )

  if (!test_result$success) return(test_result$error)

  if (use_bootstrap) {
    format_posthoc_bootstrap(
      test_result$result,
      c(
        "Lincon.psihat", "Lincon.ci.lower",
        "Lincon.ci.upper", "Lincon.p.value"
      )
    )
  } else {
    result_df <- test_result$result[[1]]
    numeric_cols <- c(
      "Lincon.psihat", "Lincon.ci.lower",
      "Lincon.ci.upper", "Lincon.p.value"
    )
    avail <- intersect(numeric_cols, names(result_df))
    result_df[avail] <- lapply(
      result_df[avail], function(x) signif(x, 3)
    )
    result_df
  }
}


#' Perform Cliff's Delta Effect Size
#'
#' Pairwise Cliff's Delta for all group pairs.
#' For multi-way designs, groups are combined into a single factor.
#' Returns raw p-values only — no adjustment applied.
#'
#' @param df Data frame
#' @param x_axis Character vector of grouping column(s)
#' @param measure_col Character, measurement column name
#' @param use_bootstrap Logical
#' @param boot_samples Integer, number of bootstrap samples
#' @param boot_sample_size Integer or NULL
#' @return Data frame with effect size results or app_error
#' @export
perform_cliff <- function(df, x_axis, measure_col,
                          use_bootstrap = FALSE,
                          boot_samples = 599,
                          boot_sample_size = NULL) {
  rhino$log$info(
    "cliff: starting for measure='{measure_col}',",
    " factors='{paste(x_axis, collapse=\", \")}'"
  )

  validation <- validate_posthoc(df, x_axis)
  if (error_handling$is_app_error(validation)) {
    return(validation)
  }

  boot_params <- omnibus$setup_bootstrap_params(
    df, x_axis, use_bootstrap, boot_samples, boot_sample_size
  )

  error_context <- build_posthoc_context(
    df, x_axis, measure_col, 0, use_bootstrap
  )

  test_result <- error_handling$safe_execute(
    expr = {
      results_list <- vector("list", boot_params$n_iterations)
      for (i in seq_len(boot_params$n_iterations)) {
        sample_data <- omnibus$sample_for_iteration(
          df, x_axis, use_bootstrap, boot_params$sample_size
        )
        results_list[[i]] <- run_cliff_iteration(
          sample_data, x_axis, measure_col
        )
      }
      results_list
    },
    operation_name = "cliff",
    context = error_context,
    error_parser = error_handling$stat_error_parser
  )

  if (!test_result$success) return(test_result$error)

  if (use_bootstrap) {
    format_posthoc_bootstrap(
      test_result$result,
      c(
        "Cliff.psihat", "Cliff.ci.lower",
        "Cliff.ci.upper", "Cliff.p.value", "Cliff.p.crit"
      )
    )
  } else {
    result_df <- test_result$result[[1]]
    numeric_cols <- c(
      "Cliff.psihat", "Cliff.ci.lower",
      "Cliff.ci.upper", "Cliff.p.value", "Cliff.p.crit"
    )
    avail <- intersect(numeric_cols, names(result_df))
    result_df[avail] <- lapply(
      result_df[avail], function(x) signif(x, 3)
    )
    result_df
  }
}


#' Perform Combined Post-Hoc Tests
#'
#' Runs both lincon and Cliff's Delta, merges results by interaction key,
#' optionally filters valid comparisons, then applies p-value adjustment.
#'
#' @param df Data frame
#' @param x_axis Character vector of grouping column(s)
#' @param measure_col Character, measurement column name
#' @param tr_value Numeric, trim proportion (0-0.5)
#' @param use_bootstrap Logical
#' @param boot_samples Integer, number of bootstrap samples
#' @param boot_sample_size Integer or NULL
#' @param p_adjust_method Character, p-value adjustment method
#' @param filter_valid Logical, filter to valid comparisons for multi-way
#' @return Data frame with combined results or app_error
#' @export
perform_combined_posthoc <- function(df, x_axis, measure_col,
                                     tr_value,
                                     use_bootstrap = FALSE,
                                     boot_samples = 599,
                                     boot_sample_size = NULL,
                                     p_adjust_method = "bonferroni",
                                     filter_valid = FALSE,
                                     debug = FALSE) {
  rhino$log$info(
    "combined_posthoc: starting for measure='{measure_col}'"
  )

  # For multi-way designs, run lincon on combined groups (1-way style)
  # to produce pairwise comparisons matching Cliff's Delta output.
  # Structured contrasts (mcp2atm/mcp3atm) test main effects/interactions
  # which are not comparable to Cliff's pairwise comparisons.
  lincon_result <- if (length(x_axis) > 1) {
    perform_lincon_combined(
      df = df, x_axis = x_axis, measure_col = measure_col,
      tr_value = tr_value, use_bootstrap = use_bootstrap,
      boot_samples = boot_samples,
      boot_sample_size = boot_sample_size
    )
  } else {
    perform_lincon(
      df = df, x_axis = x_axis, measure_col = measure_col,
      tr_value = tr_value, use_bootstrap = use_bootstrap,
      boot_samples = boot_samples,
      boot_sample_size = boot_sample_size
    )
  }

  cliff_result <- perform_cliff(
    df = df, x_axis = x_axis, measure_col = measure_col,
    use_bootstrap = use_bootstrap,
    boot_samples = boot_samples,
    boot_sample_size = boot_sample_size
  )

  if (debug) {
    cat("\n=== DEBUG: Raw Lincon Result ===", "\n")
    if (is.data.frame(lincon_result)) {
      print(lincon_result)
    } else {
      cat("Error:", lincon_result$message, "\n")
    }
    cat("\n=== DEBUG: Raw Cliff Result ===", "\n")
    if (is.data.frame(cliff_result)) {
      print(cliff_result)
    } else {
      cat("Error:", cliff_result$message, "\n")
    }
  }

  lincon_err <- error_handling$is_app_error(lincon_result)
  cliff_err <- error_handling$is_app_error(cliff_result)

  if (lincon_err && cliff_err) {
    return(error_handling$simple_error(
      message = paste0(
        "Both lincon and Cliff's Delta failed. ",
        "Lincon: ", lincon_result$message, ". ",
        "Cliff: ", cliff_result$message
      ),
      operation_name = "combined_posthoc"
    ))
  }

  if (lincon_err) return(lincon_result)
  if (cliff_err) return(cliff_result)

  if (!is.data.frame(lincon_result) ||
      !is.data.frame(cliff_result)) {
    return(error_handling$simple_error(
      message = "Unexpected result type from post-hoc tests.",
      operation_name = "combined_posthoc"
    ))
  }

  if (nrow(lincon_result) == 0 || nrow(cliff_result) == 0) {
    return(error_handling$simple_error(
      message = "One or both post-hoc tests returned empty results.",
      operation_name = "combined_posthoc"
    ))
  }

  lincon_norm <- normalize_interaction(lincon_result)
  cliff_norm <- normalize_interaction(cliff_result)

  if (debug) {
    cat("\n=== DEBUG: Lincon InteractionKeys ===", "\n")
    print(lincon_norm[, c("Interaction", "InteractionKey")])
    cat("\n=== DEBUG: Cliff InteractionKeys ===", "\n")
    print(cliff_norm[, c("Interaction", "InteractionKey")])
  }

  cliff_cols <- setdiff(names(cliff_norm), c("Interaction", "InteractionKey"))
  lincon_selected <- lincon_norm
  cliff_selected <- cliff_norm[, c("InteractionKey", cliff_cols), drop = FALSE]

  merged <- merge(
    lincon_selected, cliff_selected,
    by = "InteractionKey", all = FALSE
  )

  if (debug) {
    cat("\n=== DEBUG: Merged Result ===", "\n")
    if (nrow(merged) > 0) {
      print(merged)
    } else {
      cat("No rows matched!\n")
    }
  }

  if (nrow(merged) == 0) {
    return(error_handling$simple_error(
      message = "No matching interactions between lincon and Cliff results.",
      operation_name = "combined_posthoc"
    ))
  }

  merged$InteractionKey <- NULL

  if (filter_valid && length(x_axis) > 1) {
    merged <- filter_valid_comparisons(merged, x_axis)
    if (nrow(merged) == 0) {
      return(error_handling$simple_error(
        message = "No valid comparisons remain after filtering.",
        operation_name = "combined_posthoc"
      ))
    }
  }

  if (!use_bootstrap) {
    if ("Lincon.p.value" %in% names(merged) &&
        is.numeric(merged$Lincon.p.value)) {
      merged$Lincon.p.adjusted <- stats$p.adjust(
        merged$Lincon.p.value, method = p_adjust_method
      )
    }
    if ("Cliff.p.value" %in% names(merged) &&
        is.numeric(merged$Cliff.p.value)) {
      merged$Cliff.p.adjusted <- stats$p.adjust(
        merged$Cliff.p.value, method = p_adjust_method
      )
    }
  }

  merged$Cliff.p.crit <- NULL

  desired_order <- c(
    "Interaction",
    "Lincon.psihat", "Lincon.ci.lower", "Lincon.ci.upper",
    "Lincon.p.value", "Lincon.p.adjusted",
    "Cliff.psihat", "Cliff.ci.lower", "Cliff.ci.upper",
    "Cliff.p.value", "Cliff.p.adjusted"
  )
  final_cols <- intersect(desired_order, names(merged))
  extra_cols <- setdiff(names(merged), desired_order)
  merged <- merged[, c(final_cols, extra_cols), drop = FALSE]

  numeric_cols <- vapply(merged, is.numeric, logical(1))
  merged[numeric_cols] <- lapply(
    merged[numeric_cols], function(x) signif(x, 3)
  )

  merged
}
