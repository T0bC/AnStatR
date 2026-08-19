box::use(
  rhino,
)

# =============================================================================
# Shared derived-statistics helpers for PCA / sPCA results
# No Shiny dependencies allowed in this file.
#
# mixOmics does not compute "contribution %" or "cos2" (quality of
# representation) — these are FactoMineR-style derived statistics the
# app's PCA renderers display. Rather than reconstructing a full
# FactoMineR-shaped result object, these are pulled out as small
# stateless helpers that operate directly on mixOmics' native
# loadings ($loadings$X) and scores ($variates$X) matrices. Not
# meaningful for IPCA (components are not variance-ranked), so callers
# should not use these for ipca results.
# =============================================================================

#' Variable coordinates (loading scaled by component standard deviation)
#'
#' Standard deviation per component is derived from the scores
#' rather than taken from the model object, since sdev is only
#' stored on plain mixOmics::pca() objects, not spca()/ipca().
#'
#' @param loadings Matrix, variables x components (unit-norm loading
#'   vectors, e.g. model$loadings$X)
#' @param scores Matrix, samples x components (e.g. model$variates$X),
#'   same components as loadings — used to derive each component's
#'   standard deviation
#' @return Matrix, variables x components
#' @export
compute_var_coord <- function(loadings, scores) {
  n <- nrow(scores)
  sdev <- sqrt(colSums(scores^2) / (n - 1))
  sweep(loadings, 2, sdev, FUN = "*")
}

#' Variable contributions (% of each component explained by each variable)
#'
#' Loading vectors are unit-norm (sum of squares = 1 per component),
#' so squaring and scaling to 100 yields contributions that sum to
#' 100 per component.
#'
#' @param loadings Matrix, variables x components (unit-norm loadings)
#' @return Matrix, variables x components, values sum to 100 per column
#' @export
compute_var_contrib <- function(loadings) {
  ncomp <- ncol(loadings)
  sweep(loadings^2, 2, rep(100, ncomp), FUN = "*")
}

#' Variable cos2 (squared coordinates, quality of representation)
#'
#' @param var_coord Matrix, variables x components (from compute_var_coord)
#' @return Matrix, variables x components
#' @export
compute_var_cos2 <- function(var_coord) {
  var_coord^2
}

#' Individual (sample) contributions per component
#'
#' Each individual's share of a component's total sum of squared
#' scores. Equivalent to score^2 / ((n-1) * eigenvalue), since
#' sum(scores[, k]^2) == (n-1) * eigenvalue_k by construction —
#' expressed here directly from the scores so no separate
#' eigenvalue argument is needed.
#'
#' @param scores Matrix, samples x components (e.g. model$variates$X)
#' @return Matrix, samples x components, values sum to 100 per column
#' @export
compute_ind_contrib <- function(scores) {
  sweep(
    scores^2, 2, colSums(scores^2), FUN = "/"
  ) * 100
}

#' Individual (sample) cos2 across all fitted components
#'
#' @param scores Matrix, samples x components (all fitted components,
#'   not just the displayed subset — needed so cos2 reflects quality
#'   of representation relative to the full model)
#' @param scores_display Matrix, samples x components to report cos2
#'   for (may be a subset of scores)
#' @return Matrix, samples x ncol(scores_display)
#' @export
compute_ind_cos2 <- function(scores, scores_display) {
  total_dist2 <- rowSums(scores^2)
  total_dist2[total_dist2 == 0] <- 1
  sweep(scores_display^2, 1, total_dist2, FUN = "/")
}
