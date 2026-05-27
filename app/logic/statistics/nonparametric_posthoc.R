box::use(
  dunn.test[dunn.test],
  rhino,
  stats,
)

box::use(
  app/logic/shared/error_handling,
  app/logic/statistics/omnibus,
  app/logic/statistics/cliff_delta[cidmulv2_labelled],
  app/logic/statistics/validation_utils,
)

box::use(
  ARTool[art],
)

# =============================================================================
# Non-parametric post-hoc pairwise comparisons:
#   1-way: Dunn's test or pairwise Wilcoxon + Cliff's Delta
#   2/3-way: ART contrasts (art.con) + ART-derived Cohen's d
# Returns raw p-values only — adjustment is deferred to the combined table.
# No Shiny dependencies allowed in this file.
# =============================================================================


# =============================================================================
# Dunn's Test (1-way post-hoc for Kruskal-Wallis)
# =============================================================================

#' Perform Dunn's Test Post-hoc Comparisons
#'
#' Conducts Dunn's test for pairwise comparisons after Kruskal-Wallis.
#' For multi-way designs, groups are combined into a single interaction
#' factor using "." separator.
#' Returns raw (unadjusted) p-values.
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of grouping columns
#' @param measure_col Character, measurement column name
#' @return Data frame with pairwise comparisons or structured error
#' @export
perform_dunn_test <- function(df, x_axis, measure_col) {
  rhino$log$info(
    "dunn_test: starting for measure='{measure_col}',",
    " factors='{paste(x_axis, collapse=\", \")}'"
  )

  validation <- validation_utils$validate_posthoc(df, x_axis)
  if (error_handling$is_app_error(validation)) {
    return(validation)
  }

  error_context <- validation_utils$build_posthoc_context(df, x_axis, measure_col)

  test_result <- error_handling$safe_execute(
    expr = {
      # For multi-way designs, create combined interaction group
      if (length(x_axis) > 1) {
        df$combined_group <- do.call(
          paste, c(df[x_axis], sep = ".")
        )
      } else {
        df$combined_group <- df[[x_axis[1]]]
      }

      # Suppress dunn.test console output
      dunn_out <- utils::capture.output({
        dunn_result <- dunn.test(
          x = df[[measure_col]],
          g = df$combined_group,
          method = "none",
          kw = FALSE,
          label = TRUE,
          table = FALSE
        )
      })

      # Parse comparison labels: dunn.test uses " - " separator
      comparisons <- dunn_result$comparisons
      z_values <- dunn_result$Z
      p_values <- dunn_result$P

      # Parse "GroupA - GroupB" into separate groups
      parsed <- strsplit(comparisons, " - ", fixed = TRUE)
      g1_vec <- vapply(parsed, `[`, character(1), 1)
      g2_vec <- vapply(parsed, `[`, character(1), 2)

      data.frame(
        Interaction = paste(
          trimws(g1_vec), "vs.", trimws(g2_vec)
        ),
        Dunn.Z = signif(z_values, 3),
        Dunn.p.value = signif(p_values, 3),
        stringsAsFactors = FALSE
      )
    },
    operation_name = "dunn_test",
    context = error_context,
    error_parser = error_handling$stat_error_parser
  )

  if (!test_result$success) return(test_result$error)

  test_result$result
}


# =============================================================================
# Pairwise Wilcoxon (1-way alternative post-hoc)
# =============================================================================

#' Perform Pairwise Wilcoxon Rank-Sum Tests
#'
#' Conducts pairwise Wilcoxon rank-sum tests for pairwise comparisons.
#' For multi-way designs, groups are combined into a single interaction
#' factor using "." separator.
#' Returns raw (unadjusted) p-values.
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of grouping columns
#' @param measure_col Character, measurement column name
#' @return Data frame with pairwise comparisons or structured error
#' @export
perform_wilcox_pairwise <- function(df, x_axis, measure_col) {
  rhino$log$info(
    "wilcox_pairwise: starting for measure='{measure_col}',",
    " factors='{paste(x_axis, collapse=\", \")}'"
  )

  validation <- validation_utils$validate_posthoc(df, x_axis)
  if (error_handling$is_app_error(validation)) {
    return(validation)
  }

  error_context <- validation_utils$build_posthoc_context(df, x_axis, measure_col)

  test_result <- error_handling$safe_execute(
    expr = {
      # For multi-way designs, create combined interaction group
      if (length(x_axis) > 1) {
        df$combined_group <- as.factor(do.call(
          paste, c(df[x_axis], sep = ".")
        ))
      } else {
        df$combined_group <- as.factor(df[[x_axis[1]]])
      }

      wilcox_out <- stats$pairwise.wilcox.test(
        x = df[[measure_col]],
        g = df$combined_group,
        p.adjust.method = "none",
        exact = FALSE
      )

      # Extract p-values from matrix into pairwise rows
      p_mat <- wilcox_out$p.value
      row_names <- rownames(p_mat)
      col_names <- colnames(p_mat)

      results <- list()
      for (i in seq_along(row_names)) {
        for (j in seq_along(col_names)) {
          p_val <- p_mat[i, j]
          if (!is.na(p_val)) {
            results[[length(results) + 1]] <- data.frame(
              Interaction = paste(
                col_names[j], "vs.", row_names[i]
              ),
              Wilcox.p.value = signif(p_val, 3),
              stringsAsFactors = FALSE
            )
          }
        }
      }

      do.call(rbind, results)
    },
    operation_name = "wilcox_pairwise",
    context = error_context,
    error_parser = error_handling$stat_error_parser
  )

  if (!test_result$success) return(test_result$error)

  test_result$result
}


# =============================================================================
# ART Contrasts (2/3-way post-hoc for ART ANOVA)
# =============================================================================

#' Helper: run ART contrasts via art.con() in globalenv
#'
#' ARTool::art.con() requires proper S3 method dispatch.
#' Running in a child of globalenv() with required functions
#' ensures proper method registration.
#'
#' @param formula_obj Formula object
#' @param data Data frame
#' @param x_axis Character vector of grouping columns
#' @return List with contrasts data frame and artlm.con model
run_art_contrasts <- function(formula_obj, data, x_axis) {
  env <- new.env(parent = globalenv())
  env$formula_obj <- formula_obj
  env$data <- data
  env$x_axis <- x_axis
  env$art <- art
  env$art_con <- get("art.con", envir = asNamespace("ARTool"))
  env$artlm_con <- get("artlm.con", envir = asNamespace("ARTool"))
  env$summary_emmGrid <- get(
    "summary.emmGrid", envir = asNamespace("emmeans")
  )
  env$pairs_emmGrid <- get(
    "pairs.emmGrid", envir = asNamespace("emmeans")
  )
  env$sigma <- stats::sigma

  eval(quote({
    art_model <- art(formula_obj, data = data)

    # Build interaction term string: "X1:X2" or "X1:X2:X3"
    interaction_term <- paste(x_axis, collapse = ":")

    # Run ART-C contrasts on the interaction
    contrasts_result <- art_con(
      art_model, interaction_term, adjust = "none"
    )

    # Get the artlm.con model for sigmaHat (Cohen's d)
    art_lm <- artlm_con(art_model, interaction_term)
    sigma_hat <- sigma(art_lm)

    # Convert emmeans contrast output to data frame
    contrasts_df <- as.data.frame(
      summary_emmGrid(contrasts_result)
    )

    list(
      contrasts = contrasts_df,
      sigma_hat = sigma_hat
    )
  }), envir = env)
}

#' Perform ART Contrasts Post-hoc Comparisons
#'
#' Conducts ART-C pairwise contrasts for 2/3-way designs using
#' ARTool::art.con(). Also derives Cohen's d from ART estimates.
#' Returns raw (unadjusted) p-values.
#'
#' @param df Data frame containing the data
#' @param x_axis Character vector of grouping columns (length 2 or 3)
#' @param measure_col Character, measurement column name
#' @return Data frame with pairwise comparisons or structured error
#' @export
perform_art_contrasts <- function(df, x_axis, measure_col) {
  rhino$log$info(
    "art_contrasts: starting for measure='{measure_col}',",
    " factors='{paste(x_axis, collapse=\", \")}'"
  )

  validation <- validation_utils$validate_posthoc(df, x_axis)
  if (error_handling$is_app_error(validation)) {
    return(validation)
  }

  error_context <- validation_utils$build_posthoc_context(df, x_axis, measure_col)

  test_result <- error_handling$safe_execute(
    expr = {
      # Convert grouping variables to factors
      for (var in x_axis) {
        df[[var]] <- as.factor(df[[var]])
      }

      # Remove rows with NA in response
      na_mask <- is.na(df[[measure_col]])
      if (any(na_mask)) {
        n_missing <- sum(na_mask)
        rhino$log$warn(
          "art_contrasts: Dropping {n_missing} row(s)",
          " with NA in '{measure_col}'"
        )
        df <- df[!na_mask, , drop = FALSE]
      }

      # Build formula: measure ~ factor1 * factor2 [* factor3]
      formula_str <- paste0(
        "`", measure_col, "` ~ ",
        paste0("`", x_axis, "`", collapse = " * ")
      )
      formula_obj <- stats$as.formula(formula_str)

      # Run ART contrasts
      art_result <- run_art_contrasts(
        formula_obj, df, x_axis
      )

      contrasts_df <- art_result$contrasts
      sigma_hat <- art_result$sigma_hat

      # Parse contrast labels: art.con uses "A,C - A,D" format
      # Convert to our "A.C vs. A.D" format
      contrast_col <- if ("contrast" %in% names(contrasts_df)) {
        "contrast"
      } else {
        names(contrasts_df)[1]
      }

      parsed <- strsplit(
        as.character(contrasts_df[[contrast_col]]),
        " - ", fixed = TRUE
      )
      g1_vec <- vapply(parsed, function(p) {
        gsub(",", ".", trimws(p[1]), fixed = TRUE)
      }, character(1))
      g2_vec <- vapply(parsed, function(p) {
        gsub(",", ".", trimws(p[2]), fixed = TRUE)
      }, character(1))

      # Extract statistics
      estimates <- contrasts_df$estimate
      se_vals <- contrasts_df$SE
      df_vals <- contrasts_df$df
      t_ratios <- contrasts_df$t.ratio
      p_values <- contrasts_df$p.value

      # Calculate ART-derived Cohen's d
      d_values <- estimates / sigma_hat

      # SE of d (approximation: SE_d = SE_estimate / sigma_hat)
      se_d <- se_vals / sigma_hat
      d_ci_lower <- d_values - 1.96 * se_d
      d_ci_upper <- d_values + 1.96 * se_d

      data.frame(
        Interaction = paste(g1_vec, "vs.", g2_vec),
        ART.estimate = signif(estimates, 3),
        ART.SE = signif(se_vals, 3),
        ART.df = signif(df_vals, 3),
        ART.t.ratio = signif(t_ratios, 3),
        ART.p.value = signif(p_values, 3),
        ART.d = signif(d_values, 3),
        ART.d.ci.lower = signif(d_ci_lower, 3),
        ART.d.ci.upper = signif(d_ci_upper, 3),
        stringsAsFactors = FALSE
      )
    },
    operation_name = "art_contrasts",
    context = error_context,
    error_parser = error_handling$stat_error_parser
  )

  if (!test_result$success) return(test_result$error)

  test_result$result
}


# =============================================================================
# Combined non-parametric post-hoc
# =============================================================================

#' Perform Combined Non-Parametric Post-Hoc Tests
#'
#' 1-way: Runs Dunn or Wilcoxon + Cliff's Delta, merges by interaction key.
#' 2/3-way: Runs ART contrasts with embedded Cohen's d (no merge needed).
#' Optionally filters valid comparisons, then applies p-value adjustment.
#'
#' @param df Data frame
#' @param x_axis Character vector of grouping column(s)
#' @param measure_col Character, measurement column name
#' @param p_adjust_method Character, p-value adjustment method
#' @param filter_valid Logical, filter to valid comparisons for multi-way
#' @param posthoc_method Character, "dunn" or "wilcox" (1-way only)
#' @return Data frame with combined results or app_error
#' @export
perform_combined_nonparametric_posthoc <- function(
    df, x_axis, measure_col,
    p_adjust_method = "bonferroni",
    filter_valid = FALSE,
    posthoc_method = "dunn",
    is_rm = FALSE,
    id_col = NULL,
    within_col = NULL) {
  rhino$log$info(
    "combined_nonparametric_posthoc: starting for",
    " measure='{measure_col}',",
    " method='{posthoc_method}', rm={is_rm}"
  )

  if (isTRUE(is_rm) && !is.null(id_col) && !is.null(within_col)) {
    return(perform_rm_nonparametric_posthoc(
      df = df, x_axis = x_axis, measure_col = measure_col,
      id_col = id_col, within_col = within_col,
      p_adjust_method = p_adjust_method
    ))
  }

  n_ways <- length(x_axis)

  if (n_ways == 1) {
    combine_oneway(
      df, x_axis, measure_col,
      p_adjust_method, posthoc_method
    )
  } else if (n_ways %in% c(2, 3)) {
    combine_multiway(
      df, x_axis, measure_col,
      p_adjust_method, filter_valid
    )
  } else {
    error_handling$simple_error(
      message = paste0(
        n_ways, "-way non-parametric post-hoc ",
        "is not supported."
      ),
      operation_name = "combined_nonparametric_posthoc"
    )
  }
}


#' Combine 1-way post-hoc: Dunn/Wilcox + Cliff's Delta
#'
#' @param df Data frame
#' @param x_axis Grouping columns (length 1)
#' @param measure_col Measurement column
#' @param p_adjust_method P-value adjustment method
#' @param posthoc_method "dunn" or "wilcox"
#' @return Data frame or app_error
combine_oneway <- function(df, x_axis, measure_col,
                           p_adjust_method, posthoc_method) {
  # Run pairwise test
  pairwise_result <- if (posthoc_method == "wilcox") {
    perform_wilcox_pairwise(
      df = df, x_axis = x_axis, measure_col = measure_col
    )
  } else {
    perform_dunn_test(
      df = df, x_axis = x_axis, measure_col = measure_col
    )
  }

  # Run Cliff's Delta
  cliff_result <- error_handling$safe_execute(
    expr = {
      validation_utils$run_cliff_iteration(df, x_axis, measure_col)
    },
    operation_name = "cliff_delta",
    context = validation_utils$build_posthoc_context(df, x_axis, measure_col),
    error_parser = error_handling$stat_error_parser
  )
  if (!cliff_result$success) {
    cliff_result <- cliff_result$error
  } else {
    result_df <- cliff_result$result
    numeric_cols <- c(
      "Cliff.psihat", "Cliff.ci.lower",
      "Cliff.ci.upper", "Cliff.p.value", "Cliff.p.crit"
    )
    avail <- intersect(numeric_cols, names(result_df))
    result_df[avail] <- lapply(
      result_df[avail], function(x) signif(x, 3)
    )
    cliff_result <- result_df
  }

  pairwise_err <- error_handling$is_app_error(pairwise_result)
  cliff_err <- error_handling$is_app_error(cliff_result)

  if (pairwise_err && cliff_err) {
    return(error_handling$simple_error(
      message = paste0(
        "Both pairwise test and Cliff's Delta failed. ",
        "Pairwise: ", pairwise_result$message, ". ",
        "Cliff: ", cliff_result$message
      ),
      operation_name = "combined_nonparametric_posthoc"
    ))
  }

  if (pairwise_err) return(pairwise_result)
  if (cliff_err) return(cliff_result)

  if (!is.data.frame(pairwise_result) ||
      !is.data.frame(cliff_result)) {
    return(error_handling$simple_error(
      message = "Unexpected result type from post-hoc tests.",
      operation_name = "combined_nonparametric_posthoc"
    ))
  }

  if (nrow(pairwise_result) == 0 ||
      nrow(cliff_result) == 0) {
    return(error_handling$simple_error(
      message = "One or both post-hoc tests returned empty results.",
      operation_name = "combined_nonparametric_posthoc"
    ))
  }

  # Normalize and merge
  pairwise_norm <- validation_utils$normalize_interaction(pairwise_result)
  cliff_norm <- validation_utils$normalize_interaction(cliff_result)

  cliff_cols <- setdiff(
    names(cliff_norm), c("Interaction", "InteractionKey")
  )
  cliff_selected <- cliff_norm[
    , c("InteractionKey", cliff_cols), drop = FALSE
  ]

  merged <- merge(
    pairwise_norm, cliff_selected,
    by = "InteractionKey", all = FALSE
  )

  if (nrow(merged) == 0) {
    return(error_handling$simple_error(
      message = paste0(
        "No matching interactions between pairwise ",
        "test and Cliff's Delta results."
      ),
      operation_name = "combined_nonparametric_posthoc"
    ))
  }

  merged$InteractionKey <- NULL
  merged$Cliff.p.crit <- NULL

  # Apply p-value adjustment
  p_col <- if (posthoc_method == "wilcox") {
    "Wilcox.p.value"
  } else {
    "Dunn.p.value"
  }
  if (p_col %in% names(merged) &&
      is.numeric(merged[[p_col]])) {
    adj_col <- sub("\\.p\\.value$", ".p.adjusted", p_col)
    merged[[adj_col]] <- stats$p.adjust(
      merged[[p_col]], method = p_adjust_method
    )
  }
  if ("Cliff.p.value" %in% names(merged) &&
      is.numeric(merged$Cliff.p.value)) {
    merged$Cliff.p.adjusted <- stats$p.adjust(
      merged$Cliff.p.value, method = p_adjust_method
    )
  }

  # Reorder columns
  if (posthoc_method == "wilcox") {
    desired_order <- c(
      "Interaction",
      "Wilcox.p.value", "Wilcox.p.adjusted",
      "Cliff.psihat", "Cliff.ci.lower", "Cliff.ci.upper",
      "Cliff.p.value", "Cliff.p.adjusted"
    )
  } else {
    desired_order <- c(
      "Interaction",
      "Dunn.Z", "Dunn.p.value", "Dunn.p.adjusted",
      "Cliff.psihat", "Cliff.ci.lower", "Cliff.ci.upper",
      "Cliff.p.value", "Cliff.p.adjusted"
    )
  }
  final_cols <- intersect(desired_order, names(merged))
  extra_cols <- setdiff(names(merged), desired_order)
  merged <- merged[, c(final_cols, extra_cols), drop = FALSE]

  # Round numeric columns
  numeric_cols <- vapply(merged, is.numeric, logical(1))
  merged[numeric_cols] <- lapply(
    merged[numeric_cols], function(x) signif(x, 3)
  )

  merged
}


#' Combine multi-way post-hoc: ART contrasts + ART Cohen's d
#'
#' For 2/3-way designs, ART contrasts include both pairwise
#' comparisons and effect sizes in one result set.
#'
#' @param df Data frame
#' @param x_axis Grouping columns (length 2 or 3)
#' @param measure_col Measurement column
#' @param p_adjust_method P-value adjustment method
#' @param filter_valid Logical, filter valid comparisons
#' @return Data frame or app_error
combine_multiway <- function(df, x_axis, measure_col,
                             p_adjust_method, filter_valid) {
  art_result <- perform_art_contrasts(
    df = df, x_axis = x_axis, measure_col = measure_col
  )

  if (error_handling$is_app_error(art_result)) {
    return(art_result)
  }

  if (!is.data.frame(art_result) || nrow(art_result) == 0) {
    return(error_handling$simple_error(
      message = "ART contrasts returned empty results.",
      operation_name = "combined_nonparametric_posthoc"
    ))
  }

  # Filter valid comparisons for multi-way designs
  if (filter_valid && length(x_axis) > 1) {
    art_result <- validation_utils$filter_valid_comparisons(art_result, x_axis)
    if (nrow(art_result) == 0) {
      return(error_handling$simple_error(
        message = "No valid comparisons remain after filtering.",
        operation_name = "combined_nonparametric_posthoc"
      ))
    }
  }

  # Apply p-value adjustment
  if ("ART.p.value" %in% names(art_result) &&
      is.numeric(art_result$ART.p.value)) {
    art_result$ART.p.adjusted <- stats$p.adjust(
      art_result$ART.p.value, method = p_adjust_method
    )
  }

  # Reorder columns
  desired_order <- c(
    "Interaction",
    "ART.estimate", "ART.SE", "ART.df",
    "ART.t.ratio", "ART.p.value", "ART.p.adjusted",
    "ART.d", "ART.d.ci.lower", "ART.d.ci.upper"
  )
  final_cols <- intersect(desired_order, names(art_result))
  extra_cols <- setdiff(names(art_result), desired_order)
  art_result <- art_result[
    , c(final_cols, extra_cols), drop = FALSE
  ]

  # Round numeric columns
  numeric_cols <- vapply(art_result, is.numeric, logical(1))
  art_result[numeric_cols] <- lapply(
    art_result[numeric_cols], function(x) signif(x, 3)
  )

  art_result
}


# =============================================================================
# Repeated Measures Non-Parametric Post-Hoc
# =============================================================================

#' Perform RM Non-Parametric Post-Hoc (Paired + Unpaired comparisons)
#'
#' For multi-way designs (e.g., TREATMENT × TIME), returns both:
#' - Paired comparisons (within-subject): same subjects across time
#' - Unpaired comparisons (between-subject): different subjects at same time
#'
#' @param df Data frame (long format)
#' @param x_axis Character vector of grouping columns
#' @param measure_col Character, measurement column name
#' @param id_col Character, subject ID column name
#' @param within_col Character, within-subject factor column name
#' @param p_adjust_method Character, p-value adjustment method
#' @return Data frame with paired and unpaired Wilcoxon results or app_error
#' @export
perform_rm_nonparametric_posthoc <- function(
    df, x_axis, measure_col,
    id_col, within_col,
    p_adjust_method = "bonferroni") {
  rhino$log$info(
    "rm_nonparametric_posthoc: starting for",
    " measure='{measure_col}'"
  )

  error_context <- list(
    measure = measure_col,
    id_col = id_col,
    within_col = within_col,
    x_axis = x_axis,
    test_type = "rm_nonparametric_posthoc"
  )

  test_result <- error_handling$safe_execute(
    expr = {
      df[[id_col]] <- as.factor(df[[id_col]])
      df[[within_col]] <- as.factor(df[[within_col]])

      # Identify between-subject factors (x_axis minus within_col)
      between_factors <- setdiff(x_axis, within_col)

      # Create interaction group column combining all x_axis factors
      if (length(x_axis) > 1) {
        df$interaction_group <- do.call(
          paste, c(df[x_axis], sep = ".")
        )
      } else {
        df$interaction_group <- as.character(df[[x_axis[1]]])
      }
      df$interaction_group <- as.factor(df$interaction_group)

      # Get all unique interaction groups
      all_groups <- levels(df$interaction_group)
      n_groups <- length(all_groups)

      if (n_groups < 2) {
        stop("Post-hoc requires at least 2 groups.")
      }

      paired_results <- list()
      unpaired_results <- list()

      # All pairwise comparisons between interaction groups
      for (i in 1:(n_groups - 1)) {
        for (j in (i + 1):n_groups) {
          g1_label <- all_groups[i]
          g2_label <- all_groups[j]

          # Parse group labels to extract factor levels
          g1_parts <- strsplit(g1_label, ".", fixed = TRUE)[[1]]
          g2_parts <- strsplit(g2_label, ".", fixed = TRUE)[[1]]

          # Find which factor differs
          within_idx <- which(x_axis == within_col)
          between_idx <- which(x_axis != within_col)

          # Check if between-subject factors match
          between_match <- TRUE
          if (length(between_idx) > 0) {
            for (bi in between_idx) {
              if (g1_parts[bi] != g2_parts[bi]) {
                between_match <- FALSE
                break
              }
            }
          }

          # Check if within-subject factor matches
          within_match <- (g1_parts[within_idx] == g2_parts[within_idx])

          # Get data for each group
          g1_data <- df[
            df$interaction_group == g1_label,
            c(id_col, measure_col),
            drop = FALSE
          ]
          g2_data <- df[
            df$interaction_group == g2_label,
            c(id_col, measure_col),
            drop = FALSE
          ]

          if (between_match && !within_match) {
            # PAIRED comparison: same between-group, different within-level
            paired <- merge(
              g1_data, g2_data,
              by = id_col, suffixes = c(".1", ".2")
            )

            if (nrow(paired) < 2) next

            vals1 <- paired[[paste0(measure_col, ".1")]]
            vals2 <- paired[[paste0(measure_col, ".2")]]

            # Paired Wilcoxon signed-rank test
            wilcox_res <- stats$wilcox.test(
              vals1, vals2,
              paired = TRUE, exact = FALSE
            )

            paired_results[[length(paired_results) + 1]] <- data.frame(
              Interaction = paste(g1_label, "vs.", g2_label),
              Type = "Paired",
              Paired.Wilcox.V = signif(unname(wilcox_res$statistic), 3),
              Paired.Wilcox.p.value = signif(wilcox_res$p.value, 3),
              stringsAsFactors = FALSE
            )
          } else if (!between_match && within_match) {
            # UNPAIRED comparison: different between-group, same within-level
            vals1 <- g1_data[[measure_col]]
            vals2 <- g2_data[[measure_col]]

            if (length(vals1) < 2 || length(vals2) < 2) next

            # Independent Wilcoxon rank-sum test
            wilcox_res <- stats$wilcox.test(
              vals1, vals2,
              paired = FALSE, exact = FALSE
            )

            unpaired_results[[length(unpaired_results) + 1]] <- data.frame(
              Interaction = paste(g1_label, "vs.", g2_label),
              Type = "Unpaired",
              Wilcox.W = signif(unname(wilcox_res$statistic), 3),
              Wilcox.p.value = signif(wilcox_res$p.value, 3),
              stringsAsFactors = FALSE
            )
          }
          # Skip comparisons where both between AND within differ
        }
      }

      # Combine results
      all_results <- c(paired_results, unpaired_results)
      if (length(all_results) == 0) {
        stop("No valid comparisons found.")
      }

      merged <- do.call(rbind, all_results)

      # Apply p-value adjustment (separately for paired and unpaired)
      paired_mask <- merged$Type == "Paired"
      unpaired_mask <- merged$Type == "Unpaired"

      if (any(paired_mask) && "Paired.Wilcox.p.value" %in% names(merged)) {
        merged$Paired.Wilcox.p.adjusted <- NA_real_
        merged$Paired.Wilcox.p.adjusted[paired_mask] <- stats$p.adjust(
          merged$Paired.Wilcox.p.value[paired_mask], method = p_adjust_method
        )
      }
      if (any(unpaired_mask) && "Wilcox.p.value" %in% names(merged)) {
        merged$Wilcox.p.adjusted <- NA_real_
        merged$Wilcox.p.adjusted[unpaired_mask] <- stats$p.adjust(
          merged$Wilcox.p.value[unpaired_mask], method = p_adjust_method
        )
      }

      # Round numeric columns
      numeric_cols <- vapply(merged, is.numeric, logical(1))
      merged[numeric_cols] <- lapply(
        merged[numeric_cols], function(x) signif(x, 3)
      )

      # Sort: Paired first, then Unpaired
      merged <- merged[order(merged$Type, merged$Interaction), ]
      rownames(merged) <- NULL

      merged
    },
    operation_name = "rm_nonparametric_posthoc",
    context = error_context,
    error_parser = error_handling$stat_error_parser
  )

  if (!test_result$success) return(test_result$error)

  test_result$result
}
