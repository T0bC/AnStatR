box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/pca/ind_contrib,
)

impl <- attr(ind_contrib, "namespace")

# =============================================================================
# Helper: build a minimal PCA result for testing
# =============================================================================

make_pca_result <- function(n = 20, p = 5) {
  set.seed(42)
  data <- as.data.frame(
    matrix(rnorm(n * p), nrow = n)
  )
  colnames(data) <- paste0("V", seq_len(p))
  pca_obj <- stats::prcomp(data, center = TRUE,
                           scale. = TRUE)

  ncp <- min(p, n - 1)
  sdev <- pca_obj$sdev
  eigenvalues <- sdev^2
  total_var <- sum(eigenvalues)
  var_pct <- eigenvalues / total_var * 100
  cum_pct <- cumsum(var_pct)

  eig <- data.frame(
    eigenvalue = eigenvalues,
    variance.percent = var_pct,
    cumulative.variance.percent = cum_pct
  )
  rownames(eig) <- paste0("Dim.", seq_along(eigenvalues))

  comp_idx <- seq_len(ncp)
  dim_names <- paste0("Dim.", comp_idx)

  rotation <- pca_obj$rotation[, comp_idx, drop = FALSE]
  var_coord <- sweep(rotation, 2, sdev[comp_idx],
                     FUN = "*")
  colnames(var_coord) <- dim_names
  var_contrib <- sweep(rotation^2, 2, rep(100, ncp),
                       FUN = "*")
  colnames(var_contrib) <- dim_names
  var_cos2 <- var_coord^2
  colnames(var_cos2) <- dim_names

  scores <- pca_obj$x[, comp_idx, drop = FALSE]
  colnames(scores) <- dim_names
  total_dist2 <- rowSums(pca_obj$x^2)
  total_dist2[total_dist2 == 0] <- 1
  ind_cos2 <- sweep(scores^2, 1, total_dist2,
                    FUN = "/")
  colnames(ind_cos2) <- dim_names
  n_eff <- n - 1
  ind_contrib_mat <- sweep(scores^2, 2,
                           n_eff * eigenvalues[comp_idx],
                           FUN = "/") * 100
  colnames(ind_contrib_mat) <- dim_names
  rownames(ind_contrib_mat) <- paste0("Ind.", seq_len(n))
  rownames(ind_cos2) <- paste0("Ind.", seq_len(n))
  rownames(scores) <- paste0("Ind.", seq_len(n))

  meta <- data.frame(
    G1 = rep(c("A", "B"), length.out = n),
    stringsAsFactors = FALSE
  )

  list(
    eig = eig,
    var = list(
      coord = var_coord,
      contrib = var_contrib,
      cos2 = var_cos2
    ),
    ind = list(
      coord = scores,
      contrib = ind_contrib_mat,
      cos2 = ind_cos2,
      meta = meta
    ),
    ncp = ncp,
    call_info = list(n = n, p = p, ncp = ncp)
  )
}


# =============================================================================
# create_ind_contrib_plot
# =============================================================================

describe("create_ind_contrib_plot", {
  it("returns success with a ggplot object", {
    pca_res <- make_pca_result()
    result <- ind_contrib$create_ind_contrib_plot(
      pca_result = pca_res,
      display_ncp = 3L
    )
    expect_true(result$success)
    expect_true(inherits(result$result, "gg"))
  })

  it("returns success with grouping", {
    pca_res <- make_pca_result()
    result <- ind_contrib$create_ind_contrib_plot(
      pca_result = pca_res,
      display_ncp = 3L,
      group_cols = "G1"
    )
    expect_true(result$success)
  })

  it("returns error for NULL pca_result", {
    result <- ind_contrib$create_ind_contrib_plot(
      pca_result = NULL
    )
    expect_true(!result$success)
  })

  it("clamps display_ncp to available dims", {
    pca_res <- make_pca_result(n = 20, p = 3)
    result <- ind_contrib$create_ind_contrib_plot(
      pca_result = pca_res,
      display_ncp = 10L
    )
    expect_true(result$success)
  })

  it("works without title", {
    pca_res <- make_pca_result()
    result <- ind_contrib$create_ind_contrib_plot(
      pca_result = pca_res,
      display_ncp = 3L,
      show_title = FALSE
    )
    expect_true(result$success)
  })
})


# =============================================================================
# add_group_column
# =============================================================================

describe("add_group_column", {
  it("adds group column for single group_col", {
    pca_res <- make_pca_result()
    df <- data.frame(
      label = rep(paste0("Ind.", 1:20), 2),
      dim = rep(c("Dim.1", "Dim.2"), each = 20),
      stringsAsFactors = FALSE
    )
    result <- impl$add_group_column(
      df, pca_res$ind$meta, "G1", 20, 2
    )
    expect_true("group" %in% names(result))
    expect_equal(nrow(result), 40)
  })

  it("returns df unchanged when no group_cols", {
    df <- data.frame(label = "a", dim = "Dim.1")
    result <- impl$add_group_column(
      df, NULL, NULL, 1, 1
    )
    expect_true(!"group" %in% names(result))
  })
})


# =============================================================================
# ind_contrib_error_parser
# =============================================================================

describe("ind_contrib_error_parser", {
  it("handles dimension errors", {
    msg <- ind_contrib$ind_contrib_error_parser(
      "Dimension not found: Dim.99"
    )
    expect_true(grepl("Invalid dimension", msg))
  })

  it("handles NULL pca_result", {
    msg <- ind_contrib$ind_contrib_error_parser(
      "pca_result is NULL"
    )
    expect_true(grepl("No PCA result", msg))
  })

  it("falls back for unknown errors", {
    msg <- ind_contrib$ind_contrib_error_parser(
      "something unexpected"
    )
    expect_true(grepl("failed:", msg))
  })
})
