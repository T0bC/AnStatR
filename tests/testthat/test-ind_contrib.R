box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/pca/ind_contrib,
  app/logic/pca/pca,
)

impl <- attr(ind_contrib, "namespace")

# =============================================================================
# Helper: build a real PCA result for testing
# =============================================================================

make_pca_result <- function(n = 20, p = 5) {
  set.seed(42)
  data <- as.data.frame(
    matrix(rnorm(n * p), nrow = n)
  )
  colnames(data) <- paste0("V", seq_len(p))
  data$G1 <- rep(c("A", "B"), length.out = n)

  res <- pca$run_pca(
    data, paste0("V", seq_len(p)),
    meta_cols = "G1",
    center = TRUE, scale. = TRUE
  )
  res$result
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
      label = rep(rownames(pca_res$scores), 2),
      dim = rep(c("Dim.1", "Dim.2"), each = 20),
      stringsAsFactors = FALSE
    )
    result <- impl$add_group_column(
      df, pca_res$ind_meta, "G1", 20, 2
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
