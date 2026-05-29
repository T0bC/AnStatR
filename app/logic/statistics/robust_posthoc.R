box::use(
  rhino,
  stats,
  WRS2,
)

box::use(
  app/logic/shared/error_handling,
  app/logic/statistics/omnibus,
  app/logic/statistics/customWRS,
  app/logic/statistics/cliff_delta[cidmulv2_labelled],
  app/logic/statistics/validation_utils,
)

# =============================================================================
# Robust post-hoc pairwise comparisons: lincon + Cliff's Delta.
# Returns raw p-values only — adjustment is deferred to the combined table.
# No Shiny dependencies allowed in this file.
# =============================================================================


# =============================================================================
# Private helpers
# =============================================================================

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

  group_names <- gsub("_", ".", rownames(contrasts), fixed = TRUE)
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

  validation <- validation_utils$validate_posthoc(df, x_axis)
  if (error_handling$is_app_error(validation)) {
    return(validation)
  }

  boot_params <- omnibus$setup_bootstrap_params(
    df, x_axis, use_bootstrap, boot_samples, boot_sample_size
  )

  error_context <- validation_utils$build_posthoc_context(
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

  validation <- validation_utils$validate_posthoc(df, x_axis)
  if (error_handling$is_app_error(validation)) {
    return(validation)
  }

  boot_params <- omnibus$setup_bootstrap_params(
    df, x_axis, use_bootstrap, boot_samples, boot_sample_size
  )

  error_context <- validation_utils$build_posthoc_context(
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

  validation <- validation_utils$validate_posthoc(df, x_axis)
  if (error_handling$is_app_error(validation)) {
    return(validation)
  }

  boot_params <- omnibus$setup_bootstrap_params(
    df, x_axis, use_bootstrap, boot_samples, boot_sample_size
  )

  error_context <- validation_utils$build_posthoc_context(
    df, x_axis, measure_col, 0, use_bootstrap
  )

  test_result <- error_handling$safe_execute(
    expr = {
      results_list <- vector("list", boot_params$n_iterations)
      for (i in seq_len(boot_params$n_iterations)) {
        sample_data <- omnibus$sample_for_iteration(
          df, x_axis, use_bootstrap, boot_params$sample_size
        )
        results_list[[i]] <- validation_utils$run_cliff_iteration(
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
                                     debug = FALSE,
                                     is_rm = FALSE,
                                     id_col = NULL,
                                     within_col = NULL) {
  rhino$log$info(
    "combined_posthoc: starting for measure='{measure_col}',",
    " rm={is_rm}"
  )

  if (isTRUE(is_rm) && !is.null(id_col) && !is.null(within_col)) {
    return(perform_rm_robust_posthoc(
      df = df, x_axis = x_axis, measure_col = measure_col,
      id_col = id_col, within_col = within_col,
      tr_value = tr_value, p_adjust_method = p_adjust_method
    ))
  }

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

  lincon_norm <- validation_utils$normalize_interaction(lincon_result)
  cliff_norm <- validation_utils$normalize_interaction(cliff_result)

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
    merged <- validation_utils$filter_valid_comparisons(merged, x_axis)
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


# =============================================================================
# Repeated Measures Robust Post-Hoc
# =============================================================================

#' Compute paired Yuen statistics (trimmed means)
#'
#' @param df Data frame with interaction_group column
#' @param g1_label First group label
#' @param g2_label Second group label
#' @param id_col Subject ID column
#' @param measure_col Measurement column
#' @param tr_value Trim proportion
#' @return Data frame row with paired Yuen stats, or NULL if insufficient data
#' @keywords internal
compute_paired_yuen_stats <- function(df, g1_label, g2_label, id_col, measure_col, tr_value) {
  g1_data <- df[df$interaction_group == g1_label, c(id_col, measure_col), drop = FALSE]
  g2_data <- df[df$interaction_group == g2_label, c(id_col, measure_col), drop = FALSE]

  paired <- merge(g1_data, g2_data, by = id_col, suffixes = c(".1", ".2"))
  if (nrow(paired) < 2) return(NULL)

  vals1 <- paired[[paste0(measure_col, ".1")]]
  vals2 <- paired[[paste0(measure_col, ".2")]]

  # Yuen's paired test for trimmed means
  yuen_res <- WRS2$yuend(x = vals1, y = vals2, tr = tr_value)

  data.frame(
    Interaction = paste(g1_label, "vs.", g2_label),
    Lincon.psihat = yuen_res$diff,
    Lincon.ci.lower = yuen_res$conf.int[1],
    Lincon.ci.upper = yuen_res$conf.int[2],
    Lincon.p.value = yuen_res$p.value,
    stringsAsFactors = FALSE
  )
}


#' Perform RM Robust Post-Hoc (Hybrid approach)
#'
#' Uses a hybrid approach: run unpaired tests first, then replace paired comparisons.
#'
#' Why this approach over computing paired/unpaired separately:
#' - Reuses existing unpaired infrastructure (lincon + Cliff's Delta)
#' - Ensures consistent output format and column structure
#' - Avoids duplicating complex merging/formatting logic
#' - Unpaired results for between-subject comparisons remain valid
#' - Only within-subject (paired) comparisons need correction
#'
#' Strategy:
#' 1. Run standard unpaired posthoc (lincon + Cliff's Delta) with filter_valid=TRUE
#' 2. Identify which comparisons are "within-subject" (paired)
#' 3. Compute paired Yuen test for those rows
#' 4. Replace unpaired values with paired values for within-subject rows
#' 5. Apply p-value correction on final combined raw p-values
#'
#' @param df Data frame (long format)
#' @param x_axis Character vector of grouping columns
#' @param measure_col Character, measurement column name
#' @param id_col Character, subject ID column name
#' @param within_col Character, within-subject factor column name
#' @param tr_value Numeric, trim proportion
#' @param p_adjust_method Character, p-value adjustment method
#' @return Data frame with combined results or app_error
#' @export
perform_rm_robust_posthoc <- function(
    df, x_axis, measure_col,
    id_col, within_col,
    tr_value = 0.2,
    p_adjust_method = "bonferroni") {
  rhino$log$info(
    "rm_robust_posthoc: starting for",
    " measure='{measure_col}'"
  )

  # Validate within_col is in x_axis
  if (!within_col %in% x_axis) {
    return(error_handling$simple_error(
      message = paste0(
        "Within-subject factor '", within_col, "' must be in x_axis."
      ),
      operation_name = "rm_robust_posthoc"
    ))
  }

  error_context <- list(
    measure = measure_col,
    id_col = id_col,
    within_col = within_col,
    x_axis = x_axis,
    tr_value = tr_value,
    test_type = "rm_robust_posthoc"
  )

  test_result <- error_handling$safe_execute(
    expr = {
      # Prepare factors
      df[[id_col]] <- as.factor(df[[id_col]])
      df[[within_col]] <- as.factor(df[[within_col]])
      for (var in x_axis) {
        df[[var]] <- as.factor(df[[var]])
      }

      # Create interaction group
      if (length(x_axis) > 1) {
        df$interaction_group <- interaction(df[x_axis], sep = ".")
      } else {
        df$interaction_group <- df[[x_axis[1]]]
      }

      # Step 1: Get unpaired base results (no p-adjustment yet)
      unpaired_base <- perform_combined_posthoc(
        df = df,
        x_axis = x_axis,
        measure_col = measure_col,
        tr_value = tr_value,
        p_adjust_method = "none",
        filter_valid = TRUE,
        is_rm = FALSE
      )

      if (error_handling$is_app_error(unpaired_base)) {
        stop(unpaired_base$message)
      }

      # Step 2: Classify each comparison and compute paired stats where needed
      all_groups <- levels(df$interaction_group)
      paired_replacements <- list()

      for (i in 1:(length(all_groups) - 1)) {
        for (j in (i + 1):length(all_groups)) {
          g1 <- all_groups[i]
          g2 <- all_groups[j]

          comp_type <- validation_utils$classify_rm_comparison(g1, g2, x_axis, within_col)

          if (comp_type == "paired") {
            paired_row <- compute_paired_yuen_stats(
              df, g1, g2, id_col, measure_col, tr_value
            )
            if (!is.null(paired_row)) {
              paired_replacements[[length(paired_replacements) + 1]] <- paired_row
            }
          }
        }
      }

      # Step 3: Replace unpaired values with paired values
      if (length(paired_replacements) > 0) {
        paired_df <- do.call(rbind, paired_replacements)

        # Normalize interaction keys for matching
        unpaired_norm <- validation_utils$normalize_interaction(unpaired_base)
        paired_norm <- validation_utils$normalize_interaction(paired_df)

        # For each paired row, find and replace in unpaired_base
        for (pk in paired_norm$InteractionKey) {
          match_idx <- which(unpaired_norm$InteractionKey == pk)
          if (length(match_idx) == 1) {
            paired_row <- paired_norm[paired_norm$InteractionKey == pk, ]
            # Replace Lincon values (Cliff's Delta remains from unpaired)
            cols_to_replace <- c(
              "Lincon.psihat", "Lincon.ci.lower", "Lincon.ci.upper", "Lincon.p.value"
            )
            for (col in cols_to_replace) {
              if (col %in% names(unpaired_base) && col %in% names(paired_row)) {
                unpaired_base[match_idx, col] <- paired_row[[col]]
              }
            }
          }
        }
      }

      # Step 4: Apply p-value adjustment on final combined raw p-values
      # Remove any existing p.adjusted columns first
      unpaired_base$Lincon.p.adjusted <- NULL
      unpaired_base$Cliff.p.adjusted <- NULL

      if ("Lincon.p.value" %in% names(unpaired_base)) {
        unpaired_base$Lincon.p.adjusted <- stats$p.adjust(
          unpaired_base$Lincon.p.value, method = p_adjust_method
        )
      }
      if ("Cliff.p.value" %in% names(unpaired_base)) {
        unpaired_base$Cliff.p.adjusted <- stats$p.adjust(
          unpaired_base$Cliff.p.value, method = p_adjust_method
        )
      }

      # Reorder columns
      desired_order <- c(
        "Interaction",
        "Lincon.psihat", "Lincon.ci.lower", "Lincon.ci.upper",
        "Lincon.p.value", "Lincon.p.adjusted",
        "Cliff.psihat", "Cliff.ci.lower", "Cliff.ci.upper",
        "Cliff.p.value", "Cliff.p.adjusted"
      )
      final_cols <- intersect(desired_order, names(unpaired_base))
      extra_cols <- setdiff(names(unpaired_base), desired_order)
      result <- unpaired_base[, c(final_cols, extra_cols), drop = FALSE]

      # Round numeric columns
      numeric_cols <- vapply(result, is.numeric, logical(1))
      result[numeric_cols] <- lapply(
        result[numeric_cols], function(x) signif(x, 3)
      )

      # Sort by Interaction
      result <- result[order(result$Interaction), ]
      rownames(result) <- NULL

      result
    },
    operation_name = "rm_robust_posthoc",
    context = error_context,
    error_parser = error_handling$stat_error_parser
  )

  if (!test_result$success) return(test_result$error)

  test_result$result
}
