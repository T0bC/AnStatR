box::use(
  app/logic/shared/column_utils,
  app/logic/shared/error_handling,
  app/logic/statistics/posthoc_columns,
)

# =============================================================================
# Parameter screening: rank measurement parameters by how well they
# separate groups in each pairwise comparison, using already-computed
# post-hoc results. No test is re-run here.
#
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Rank measurement parameters by pairwise-comparison p-value
#'
#' For each pairwise comparison ("Interaction"), ranks the measures by their
#' raw (or adjusted) post-hoc p-value ascending, breaking ties by effect-size
#' magnitude (largest separation from the null wins). The top `top_n`
#' measures per comparison are marked, and their union (deduplicated,
#' `_normalized` suffix stripped) becomes the recommended parameter set for
#' downstream dimension reduction (PCA/LDA).
#'
#' @param posthoc_results Named list, measure name -> post-hoc result
#'   (data frame, app_error, or NULL). Typically
#'   \code{computation_results()$posthoc} from the Statistics module.
#' @param top_n Integer, number of top measures to mark per comparison
#'   (default 3)
#' @param p_column Character, "raw" (default) or "adjusted" - which p-value
#'   column to rank on. Raw is recommended: each measure's post-hoc table
#'   adjusts over a different number of surviving rows, so adjusted p-values
#'   are not comparable across measures.
#' @return List with:
#'   \itemize{
#'     \item \code{$ranking} - long data frame: Interaction, parameter,
#'       measure, p, effect, rank, is_top
#'     \item \code{$recommended} - character vector, union of top-marked
#'       parameters (raw names, `_normalized` suffix stripped)
#'     \item \code{$comparisons} - character vector, unique Interaction
#'       values in input order
#'     \item \code{$effect_label} - character, effect-size label (empty
#'       string if measures used heterogeneous schemas)
#'     \item \code{$effect_null} - numeric, the null value of the effect
#'       size (NA if heterogeneous)
#'     \item \code{$p_column_used} - character, "raw" or "adjusted"
#'     \item \code{$top_n} - integer, the top_n used
#'     \item \code{$skipped} - data frame: measure, reason, detail
#'   }
#'   or a structured app_error when every measure was skipped.
#' @export
rank_parameters_by_comparison <- function(posthoc_results, top_n = 3,
                                          p_column = c("raw", "adjusted")) {
  p_column <- match.arg(p_column)

  skipped <- data.frame(
    measure = character(0), reason = character(0),
    detail = character(0), stringsAsFactors = FALSE
  )
  add_skipped <- function(measure, reason, detail = "") {
    skipped <<- rbind(skipped, data.frame(
      measure = measure, reason = reason, detail = detail,
      stringsAsFactors = FALSE
    ))
  }

  records <- list()
  schemas <- list()

  for (measure in names(posthoc_results)) {
    df <- posthoc_results[[measure]]

    if (error_handling$is_app_error(df)) {
      add_skipped(measure, "error", df$message %||% "")
      next
    }
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      add_skipped(measure, "empty")
      next
    }

    schema <- posthoc_columns$detect_posthoc_schema(df)
    p_col <- if (p_column == "raw") schema$p_raw_col else schema$p_adj_col

    if (is.na(p_col) || !(p_col %in% names(df)) || !is.numeric(df[[p_col]])) {
      add_skipped(measure, "no_p_column")
      next
    }
    if (!("Interaction" %in% names(df))) {
      add_skipped(measure, "no_p_column", "missing Interaction column")
      next
    }

    schemas[[measure]] <- schema

    effect_vals <- if (!is.na(schema$effect_col) &&
                        schema$effect_col %in% names(df)) {
      df[[schema$effect_col]]
    } else {
      rep(NA_real_, nrow(df))
    }

    records[[measure]] <- data.frame(
      Interaction = df$Interaction,
      measure = measure,
      parameter = column_utils$strip_normalized_suffix(measure),
      p = df[[p_col]],
      effect = effect_vals,
      effect_null = schema$effect_null,
      stringsAsFactors = FALSE
    )
  }

  if (length(records) == 0) {
    return(error_handling$simple_error(
      message = paste(
        "No measurement parameter produced a usable post-hoc result;",
        "the separation ranking could not be computed."
      ),
      operation_name = "parameter_ranking"
    ))
  }

  long_df <- do.call(rbind, records)
  rownames(long_df) <- NULL

  # Drop rows with NA p-values (cannot be ranked)
  na_p <- is.na(long_df$p)
  if (any(na_p)) {
    for (m in unique(long_df$measure[na_p])) {
      add_skipped(m, "na_p_value", "some comparisons had NA p-values")
    }
    long_df <- long_df[!na_p, , drop = FALSE]
  }

  if (nrow(long_df) == 0) {
    return(error_handling$simple_error(
      message = paste(
        "All post-hoc p-values were NA;",
        "the separation ranking could not be computed."
      ),
      operation_name = "parameter_ranking"
    ))
  }

  # Rank within each comparison: ascending p, ties broken by larger
  # |effect - effect_null| (falls back to NA-safe ordering when the
  # schema has no effect-size column, e.g. RM robust / RM nonparametric).
  long_df$abs_effect <- abs(long_df$effect - long_df$effect_null)

  comparisons <- unique(long_df$Interaction)
  ranked_parts <- lapply(comparisons, function(comp) {
    part <- long_df[long_df$Interaction == comp, , drop = FALSE]
    ord <- order(part$p, -part$abs_effect, method = "radix", na.last = TRUE)
    part <- part[ord, , drop = FALSE]
    part$rank <- seq_len(nrow(part))
    part$is_top <- part$rank <= top_n
    part
  })
  ranking <- do.call(rbind, ranked_parts)
  rownames(ranking) <- NULL
  ranking$abs_effect <- NULL
  ranking$effect_null <- NULL

  # Union of top-marked parameters, sorted by (n_top desc, best rank asc)
  top_rows <- ranking[ranking$is_top, , drop = FALSE]
  if (nrow(top_rows) == 0) {
    recommended <- character(0)
  } else {
    unique_params <- unique(top_rows$parameter)
    n_top <- vapply(unique_params, function(p) {
      sum(top_rows$parameter == p)
    }, integer(1))
    best_rank <- vapply(unique_params, function(p) {
      min(top_rows$rank[top_rows$parameter == p])
    }, integer(1))
    ord <- order(-n_top, best_rank, unique_params)
    recommended <- unique_params[ord]
  }

  # Effect label/null: report only when all measures share one schema
  unique_labels <- unique(vapply(schemas, function(s) s$effect_label, ""))
  unique_labels <- unique_labels[!is.na(unique_labels)]
  effect_label <- if (length(unique_labels) == 1) unique_labels else ""
  unique_nulls <- unique(vapply(schemas, function(s) s$effect_null, NA_real_))
  unique_nulls <- unique_nulls[!is.na(unique_nulls)]
  effect_null <- if (length(unique_nulls) == 1) unique_nulls else NA_real_

  list(
    ranking = ranking,
    recommended = recommended,
    comparisons = comparisons,
    effect_label = effect_label,
    effect_null = effect_null,
    p_column_used = p_column,
    top_n = top_n,
    skipped = skipped
  )
}

#' Pivot a ranking result into a wide parameter x comparison matrix
#'
#' Builds a display-ready data frame with one row per parameter and one
#' column per pairwise comparison, formatted as "rank (p)". Parameters are
#' sorted by how many comparisons they placed top in (descending).
#'
#' @param ranking_result List returned by rank_parameters_by_comparison()
#'   (must not be an app_error)
#' @return Data frame with columns: parameter, one column per comparison,
#'   n_top. A logical matrix marking top cells is attached as the
#'   \code{"is_top"} attribute (same dimensions as the comparison columns).
#' @export
build_ranking_matrix <- function(ranking_result) {
  ranking <- ranking_result$ranking
  comparisons <- ranking_result$comparisons
  parameters <- unique(ranking$parameter)

  n_top_by_param <- vapply(parameters, function(p) {
    sum(ranking$parameter == p & ranking$is_top)
  }, integer(1))
  ord <- order(-n_top_by_param, parameters)
  parameters <- parameters[ord]
  n_top_by_param <- n_top_by_param[ord]

  cell_text <- matrix(
    NA_character_, nrow = length(parameters), ncol = length(comparisons),
    dimnames = list(parameters, comparisons)
  )
  is_top_mat <- matrix(
    FALSE, nrow = length(parameters), ncol = length(comparisons),
    dimnames = list(parameters, comparisons)
  )

  for (i in seq_len(nrow(ranking))) {
    row <- ranking[i, ]
    cell_text[row$parameter, row$Interaction] <- paste0(
      row$rank, " (", signif(row$p, 3), ")"
    )
    is_top_mat[row$parameter, row$Interaction] <- isTRUE(row$is_top)
  }

  result <- data.frame(
    parameter = parameters,
    stringsAsFactors = FALSE
  )
  for (comp in comparisons) {
    result[[comp]] <- ifelse(is.na(cell_text[, comp]), "", cell_text[, comp])
  }
  result$n_top <- n_top_by_param

  attr(result, "is_top") <- is_top_mat
  result
}
