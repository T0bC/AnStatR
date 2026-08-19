box::use(
  rhino,
)

box::use(
  app/logic/shared/error_handling,
)

# =============================================================================
# Dimension evaluation via one-way ANOVA per LD axis
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Evaluate discriminant dimensions via ANOVA
#'
#' Runs a one-way ANOVA (lm + anova) for each LD dimension
#' to assess how well the grouping variable explains variance
#' in that dimension. Returns F-statistic, p-value, and R².
#'
#' Works for LDA, MDA (using $scores), and QDA (using
#' $lda_scores from companion LDA).
#'
#' @param lda_result Result list from run_lda/run_qda/run_mda
#' @return List with $success, $result (data.frame) or $error.
#'   The data.frame has columns: Dimension, F, p.value, R2,
#'   Significance.
#' @export
evaluate_dimensions <- function(lda_result) {
  error_handling$safe_execute(
    {
      if (is.null(lda_result)) stop("No result available.")

      # Get scores and grouping
      scores <- get_scores(lda_result)
      if (is.null(scores) || ncol(scores) == 0) {
        stop("No LD scores available for dimension evaluation.")
      }

      grouping_col <- lda_result$grouping_col
      if (is.null(grouping_col)) {
        stop("No grouping column in result.")
      }

      # Reconstruct grouping factor from the true group labels in meta
      group <- get_grouping(lda_result)
      if (is.null(group)) {
        stop(
          "Grouping column '", grouping_col, "' is not available in the ",
          "metadata - add it to the Descriptive columns to enable ",
          "Dimension Evaluation."
        )
      }

      if (length(group) != nrow(scores)) {
        stop(
          "Mismatch: group has ", length(group),
          " obs but scores have ", nrow(scores), " rows."
        )
      }

      n_dims <- ncol(scores)
      dim_names <- colnames(scores)

      rows <- vector("list", n_dims)

      for (i in seq_len(n_dims)) {
        dim_score <- scores[[i]]
        fit <- stats::lm(dim_score ~ group)
        aov_table <- stats::anova(fit)
        fit_summary <- summary(fit)

        f_val <- aov_table[["F value"]][1]
        p_val <- aov_table[["Pr(>F)"]][1]
        r_sq <- fit_summary$r.squared * 100

        rows[[i]] <- data.frame(
          Dimension = dim_names[i],
          F = round(f_val, 2),
          p.value = p_val,
          R2 = round(r_sq, 2),
          stringsAsFactors = FALSE
        )
      }

      result <- do.call(rbind, rows)

      # Add significance stars
      result$Significance <- vapply(
        result$p.value, format_sig, character(1)
      )

      # Format p-value for display
      result$p.value <- vapply(
        result$p.value, format_p, character(1)
      )

      rhino$log$info(
        "dimension_eval: evaluated {n_dims} dimensions"
      )

      result
    },
    operation_name = "Dimension Evaluation"
  )
}


# =============================================================================
# Internal helpers
# =============================================================================

#' Extract LD scores from any analysis type
get_scores <- function(lda_result) {
  type <- lda_result$analysis_type
  if (type == "qda") {
    lda_result$lda_scores
  } else {
    lda_result$scores
  }
}


#' Extract grouping factor from result
get_grouping <- function(lda_result) {
  # meta should contain the grouping column
  meta <- lda_result$meta
  gcol <- lda_result$grouping_col

  if (
    !is.null(meta) &&
    !is.null(gcol) &&
    gcol %in% names(meta)
  ) {
    return(as.factor(meta[[gcol]]))
  }

  # No fallback to predicted_class: regressing the model's own scores on
  # its own predictions is circular and would fabricate an inflated
  # R2/F/p-value. Signal "unavailable" instead so the caller can fail
  # explicitly and the panel is omitted.
  NULL
}


#' Format p-value for display
format_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< 0.001")
  if (p < 0.01) return(sprintf("%.3f", p))
  sprintf("%.3f", p)
}


#' Format significance stars
format_sig <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  if (p < 0.1) return(".")
  ""
}
