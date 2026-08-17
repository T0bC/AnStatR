# =============================================================================
# Column-name resolution for post-hoc result data frames.
#
# Post-hoc data frames from robust/parametric/nonparametric_posthoc carry
# different column prefixes depending on the statistical approach and
# whether repeated-measures pairing was used. This module centralizes the
# prefix-detection cascade so it is defined once and consumed by both the
# Statistics view (rendering) and the parameter ranking logic.
#
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Detect which post-hoc schema a result data frame follows
#'
#' Inspects column name prefixes to determine which statistical approach
#' produced the data frame, and returns the resolved p-value and effect-size
#' column names. Detection order mirrors the historical cascade in the
#' Statistics view module (Paired.t -> RM.Lincon -> Paired.Wilcox -> Lincon ->
#' Tukey -> Dunn -> Wilcox -> ART) so behavior is preserved exactly.
#'
#' @param df Data frame, a post-hoc result (e.g. from
#'   robust_posthoc$perform_combined_posthoc())
#' @return List with:
#'   \itemize{
#'     \item \code{$approach} - one of "parametric", "robust",
#'       "nonparametric_1way", "nonparametric_multiway", "rm_parametric",
#'       "rm_robust", "rm_nonparametric", "unknown"
#'     \item \code{$is_rm} - logical, whether this is a repeated-measures schema
#'     \item \code{$is_mixed} - logical, whether a "Type" column (mixed
#'       paired/unpaired RM) is present
#'     \item \code{$p_raw_col} - character or NA, the raw p-value column name
#'     \item \code{$p_adj_col} - character or NA, the adjusted p-value column name
#'     \item \code{$effect_col} - character or NA, the effect-size column name
#'     \item \code{$effect_label} - character, human-readable effect-size label
#'     \item \code{$effect_null} - numeric or NA, the null value of the effect
#'       size (0 for d-family statistics, 0.5 for Cliff's psihat)
#'     \item \code{$left_prefix} - character, prefix for the primary
#'       (location) columns
#'     \item \code{$left_label} - character, human-readable label for the
#'       primary columns
#'     \item \code{$right_prefix} - character or NA, prefix for the
#'       effect-size columns
#'     \item \code{$right_label} - character or NA, human-readable label for
#'       the effect-size columns
#'   }
#' @export
detect_posthoc_schema <- function(df) {
  cols <- names(df)
  is_mixed <- "Type" %in% cols

  has_paired_t <- any(grepl("^Paired\\.t\\.", cols))
  has_rm_lincon <- any(grepl("^RM\\.Lincon\\.", cols))
  has_paired_wilcox <- any(grepl("^Paired\\.Wilcox\\.", cols))
  has_lincon <- any(grepl("^Lincon\\.", cols))
  has_tukey <- any(grepl("^Tukey\\.", cols))
  has_dunn <- any(grepl("^Dunn\\.", cols))
  has_wilcox <- any(grepl("^Wilcox\\.", cols))
  has_art <- any(grepl("^ART\\.", cols))

  if (has_paired_t) {
    return(list(
      approach = "rm_parametric",
      is_rm = TRUE,
      is_mixed = is_mixed,
      p_raw_col = "Paired.t.p.value",
      p_adj_col = "Paired.t.p.adjusted",
      effect_col = "Paired.d",
      effect_label = "Paired Cohen's d",
      effect_null = 0,
      left_prefix = "Paired.t",
      left_label = "Paired t-Test",
      right_prefix = "Paired.d",
      right_label = "Paired Cohen's d"
    ))
  }

  if (has_rm_lincon) {
    return(list(
      approach = "rm_robust",
      is_rm = TRUE,
      is_mixed = is_mixed,
      p_raw_col = "RM.Lincon.p.value",
      p_adj_col = "RM.Lincon.p.adjusted",
      effect_col = NA_character_,
      effect_label = NA_character_,
      effect_null = NA_real_,
      left_prefix = "RM.Lincon",
      left_label = "RM Lincon (Trimmed Means)",
      right_prefix = NA_character_,
      right_label = NA_character_
    ))
  }

  if (has_paired_wilcox) {
    return(list(
      approach = "rm_nonparametric",
      is_rm = TRUE,
      is_mixed = is_mixed,
      p_raw_col = "Paired.Wilcox.p.value",
      p_adj_col = "Paired.Wilcox.p.adjusted",
      effect_col = NA_character_,
      effect_label = NA_character_,
      effect_null = NA_real_,
      left_prefix = "Paired.Wilcox",
      left_label = "Paired Wilcoxon",
      right_prefix = NA_character_,
      right_label = NA_character_
    ))
  }

  if (has_lincon) {
    return(list(
      approach = "robust",
      is_rm = FALSE,
      is_mixed = is_mixed,
      p_raw_col = "Lincon.p.value",
      p_adj_col = "Lincon.p.adjusted",
      effect_col = "Cliff.psihat",
      effect_label = "Cliff's delta P(X<Y), null = 0.5",
      effect_null = 0.5,
      left_prefix = "Lincon",
      left_label = "Lincon",
      right_prefix = "Cliff",
      right_label = "Cliff's Delta"
    ))
  }

  if (has_tukey) {
    return(list(
      approach = "parametric",
      is_rm = FALSE,
      is_mixed = is_mixed,
      p_raw_col = "Tukey.p.value",
      p_adj_col = "Tukey.p.adjusted",
      effect_col = "Cohen.d",
      effect_label = "Cohen's d",
      effect_null = 0,
      left_prefix = "Tukey",
      left_label = "Tukey HSD",
      right_prefix = "Cohen",
      right_label = "Cohen's d"
    ))
  }

  if (has_dunn) {
    return(list(
      approach = "nonparametric_1way",
      is_rm = FALSE,
      is_mixed = is_mixed,
      p_raw_col = "Dunn.p.value",
      p_adj_col = "Dunn.p.adjusted",
      effect_col = "Cliff.psihat",
      effect_label = "Cliff's delta P(X<Y), null = 0.5",
      effect_null = 0.5,
      left_prefix = "Dunn",
      left_label = "Dunn's Test",
      right_prefix = "Cliff",
      right_label = "Cliff's Delta"
    ))
  }

  if (has_wilcox) {
    return(list(
      approach = "nonparametric_1way",
      is_rm = FALSE,
      is_mixed = is_mixed,
      p_raw_col = "Wilcox.p.value",
      p_adj_col = "Wilcox.p.adjusted",
      effect_col = "Cliff.psihat",
      effect_label = "Cliff's delta P(X<Y), null = 0.5",
      effect_null = 0.5,
      left_prefix = "Wilcox",
      left_label = "Pairwise Wilcoxon",
      right_prefix = "Cliff",
      right_label = "Cliff's Delta"
    ))
  }

  if (has_art) {
    return(list(
      approach = "nonparametric_multiway",
      is_rm = FALSE,
      is_mixed = is_mixed,
      p_raw_col = "ART.p.value",
      p_adj_col = "ART.p.adjusted",
      effect_col = "ART.d",
      effect_label = "ART Cohen's d",
      effect_null = 0,
      left_prefix = "ART",
      left_label = "ART Contrasts",
      right_prefix = "ART.d",
      right_label = "ART Cohen's d"
    ))
  }

  list(
    approach = "unknown",
    is_rm = FALSE,
    is_mixed = is_mixed,
    p_raw_col = NA_character_,
    p_adj_col = NA_character_,
    effect_col = NA_character_,
    effect_label = NA_character_,
    effect_null = NA_real_,
    left_prefix = NA_character_,
    left_label = NA_character_,
    right_prefix = NA_character_,
    right_label = NA_character_
  )
}
