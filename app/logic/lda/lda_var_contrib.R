box::use(
  rhino,
)

# =============================================================================
# Adapter: convert LDA/QDA/MDA scaling to PCA-like var structure
# so that create_var_contrib_jitter_plot() can be reused as-is.
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Convert LDA result to PCA-like variable structure
#'
#' Builds a fake "pca_result" list with $var$contrib, $var$cos2,
#' $var$coord, and $eig — derived from the LDA scaling matrix
#' and proportion_of_trace. This allows
#' create_var_contrib_jitter_plot() to work unchanged.
#'
#' @param lda_result Result list from run_lda/run_qda/run_mda
#' @return List mimicking PCA result structure, or NULL if
#'   scaling is not available (CV mode, QDA without companion)
#' @export
lda_to_pca_var_structure <- function(lda_result) {
  if (is.null(lda_result)) return(NULL)

  analysis_type <- lda_result$analysis_type

  # Extract scaling and proportion_of_trace based on type
  scaling <- NULL
  prop_trace <- NULL

  if (analysis_type == "qda") {
    # QDA: use companion LDA scaling if available
    scaling <- lda_result$lda_scaling
    prop_trace <- lda_result$lda_proportion_of_trace
  } else {
    # LDA or MDA: use direct scaling
    scaling <- lda_result$scaling
    prop_trace <- lda_result$proportion_of_trace
  }

  if (is.null(scaling)) {
    rhino$log$info(
      "lda_var_contrib: no scaling available ",
      "(analysis_type={analysis_type})"
    )
    return(NULL)
  }

  scaling_mat <- as.matrix(scaling)
  n_dims <- ncol(scaling_mat)
  n_vars <- nrow(scaling_mat)
  dim_names <- colnames(scaling_mat)

  if (is.null(dim_names)) {
    prefix <- if (analysis_type == "mda") "DC" else "LD"
    dim_names <- paste0(prefix, seq_len(n_dims))
  }

  # --- Coord: scaling values (LD coefficients) ---
  var_coord <- scaling_mat
  colnames(var_coord) <- dim_names

  # --- Contribution: scaling^2 normalized to 100% per dim ---
  scaling_sq <- scaling_mat^2
  col_sums <- colSums(scaling_sq)
  # Avoid division by zero

  col_sums[col_sums == 0] <- 1
  var_contrib <- sweep(scaling_sq, 2, col_sums, "/") * 100
  colnames(var_contrib) <- dim_names

  # --- Cos2: proportion of variable's total squared loading
  #     captured in each dimension ---
  row_totals <- rowSums(scaling_sq)
  row_totals[row_totals == 0] <- 1
  var_cos2 <- sweep(scaling_sq, 1, row_totals, "/")
  colnames(var_cos2) <- dim_names

  # --- Eig: build eigenvalue-like table from
  #     proportion_of_trace ---
  if (!is.null(prop_trace) && nrow(prop_trace) > 0) {
    # prop_trace has columns: LD, Proportion, Cumulative
    # (and optionally Singular Value)
    var_pct <- prop_trace$Proportion * 100
    cum_pct <- prop_trace$Cumulative * 100

    eig <- data.frame(
      eigenvalue = var_pct / 100,
      `variance.percent` = var_pct,
      `cumulative.variance.percent` = cum_pct,
      check.names = FALSE
    )
    rownames(eig) <- dim_names[seq_len(nrow(eig))]
    colnames(eig) <- c(
      "eigenvalue", "variance.percent",
      "cumulative.variance.percent"
    )
  } else {
    # Fallback: equal weight per dimension
    var_pct <- rep(100 / n_dims, n_dims)
    cum_pct <- cumsum(var_pct)
    eig <- data.frame(
      eigenvalue = var_pct / 100,
      `variance.percent` = var_pct,
      `cumulative.variance.percent` = cum_pct,
      check.names = FALSE
    )
    rownames(eig) <- dim_names
    colnames(eig) <- c(
      "eigenvalue", "variance.percent",
      "cumulative.variance.percent"
    )
  }

  rhino$log$info(
    "lda_var_contrib: built PCA-like structure ",
    "({n_vars} vars, {n_dims} dims)"
  )

  list(
    var = list(
      coord = var_coord,
      contrib = var_contrib,
      cos2 = var_cos2
    ),
    eig = eig
  )
}
