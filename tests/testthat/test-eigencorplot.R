box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/pca/eigencorplot,
  app/logic/pca/pca,
)

impl <- attr(eigencorplot, "namespace")

# =============================================================================
# Helper: build a PCA result with metadata for testing
# =============================================================================

build_test_pca <- function(meta_cols = c("GROUP", "SITE")) {
  set.seed(42)
  test_data <- data.frame(
    GROUP = rep(c("A", "B"), each = 10),
    SITE = rep(c("X", "Y", "Z", "W", "V"), 4),
    a = rnorm(20, mean = 10, sd = 2),
    b = rnorm(20, mean = 5, sd = 1),
    c = rnorm(20, mean = 0, sd = 3),
    d = rnorm(20, mean = 20, sd = 5),
    stringsAsFactors = FALSE
  )
  pca$run_pca(
    test_data, c("a", "b", "c", "d"),
    meta_cols = meta_cols
  )
}

# =============================================================================
# compute_eigencor_data
# =============================================================================

describe("compute_eigencor_data", {
  it("returns success with valid PCA result and metadata", {
    pca_res <- build_test_pca()
    res <- eigencorplot$compute_eigencor_data(
      pca_res$result, display_ncp = 3L
    )
    expect_true(res$success)
  })

  it("returns correct matrix dimensions", {
    pca_res <- build_test_pca()
    res <- eigencorplot$compute_eigencor_data(
      pca_res$result, display_ncp = 3L
    )
    r <- res$result
    # 3 dims x 2 metadata columns
    expect_equal(nrow(r$cor_matrix), 3)
    expect_equal(ncol(r$cor_matrix), 2)
    expect_equal(nrow(r$pval_matrix), 3)
    expect_equal(ncol(r$pval_matrix), 2)
  })

  it("correlations are in [-1, 1]", {
    pca_res <- build_test_pca()
    res <- eigencorplot$compute_eigencor_data(
      pca_res$result, display_ncp = 4L
    )
    r <- res$result
    expect_true(all(r$cor_matrix >= -1 & r$cor_matrix <= 1))
  })

  it("p-values are in [0, 1]", {
    pca_res <- build_test_pca()
    res <- eigencorplot$compute_eigencor_data(
      pca_res$result, display_ncp = 4L
    )
    r <- res$result
    non_na <- r$pval_matrix[!is.na(r$pval_matrix)]
    expect_true(all(non_na >= 0 & non_na <= 1))
  })

  it("reports coerced columns for non-numeric metadata", {
    pca_res <- build_test_pca()
    res <- eigencorplot$compute_eigencor_data(
      pca_res$result, display_ncp = 2L
    )
    r <- res$result
    # GROUP and SITE are character -> should be coerced
    expect_true("GROUP" %in% r$coerced_cols)
    expect_true("SITE" %in% r$coerced_cols)
  })

  it("dim_labels contain variance percentages", {
    pca_res <- build_test_pca()
    res <- eigencorplot$compute_eigencor_data(
      pca_res$result, display_ncp = 2L
    )
    r <- res$result
    expect_true(all(grepl("%", r$dim_labels)))
  })

  it("respects display_ncp limit", {
    pca_res <- build_test_pca()
    res <- eigencorplot$compute_eigencor_data(
      pca_res$result, display_ncp = 2L
    )
    expect_equal(nrow(res$result$cor_matrix), 2)
  })

  it("fails when pca_result is NULL", {
    res <- eigencorplot$compute_eigencor_data(NULL)
    expect_true(!res$success)
  })

  it("fails when metadata is only Row column", {
    set.seed(42)
    test_data <- data.frame(
      a = rnorm(10), b = rnorm(10), c = rnorm(10)
    )
    pca_res <- pca$run_pca(test_data, c("a", "b", "c"))
    res <- eigencorplot$compute_eigencor_data(pca_res$result)
    expect_true(!res$success)
  })
})

# =============================================================================
# create_eigencor_plot
# =============================================================================

describe("create_eigencor_plot", {
  it("returns success with a ggplot object", {
    pca_res <- build_test_pca()
    eigencor_res <- eigencorplot$compute_eigencor_data(
      pca_res$result, display_ncp = 3L
    )
    plot_res <- eigencorplot$create_eigencor_plot(
      eigencor_res$result
    )
    expect_true(plot_res$success)
    expect_true(inherits(plot_res$result, "ggplot"))
  })
})

# =============================================================================
# eigencor_error_parser
# =============================================================================

describe("eigencor_error_parser", {
  it("parses metadata-related errors", {
    msg <- eigencorplot$eigencor_error_parser(
      "no valid metadata columns"
    )
    expect_true(grepl("metadata", msg, ignore.case = TRUE))
  })

  it("parses NULL pca_result errors", {
    msg <- eigencorplot$eigencor_error_parser(
      "pca_result is NULL"
    )
    expect_true(grepl("PCA result", msg, ignore.case = TRUE))
  })

  it("falls back for unknown errors", {
    msg <- eigencorplot$eigencor_error_parser(
      "something unexpected"
    )
    expect_true(grepl("failed", msg))
  })
})

# =============================================================================
# Internal: validate_metadata
# =============================================================================

describe("validate_metadata", {
  it("passes for valid metadata", {
    meta <- data.frame(
      GROUP = c("A", "B"), SITE = c("X", "Y"),
      stringsAsFactors = FALSE
    )
    expect_true(impl$validate_metadata(meta))
  })

  it("fails for NULL metadata", {
    expect_error(impl$validate_metadata(NULL))
  })

  it("fails for Row-only metadata", {
    meta <- data.frame(Row = 1:5)
    expect_error(impl$validate_metadata(meta))
  })
})

# =============================================================================
# Internal: significance_stars
# =============================================================================

describe("significance_stars", {
  it("returns *** for p < 0.001", {
    expect_equal(impl$significance_stars(0.0005), "***")
  })

  it("returns ** for p < 0.01", {
    expect_equal(impl$significance_stars(0.005), "**")
  })

  it("returns * for p < 0.05", {
    expect_equal(impl$significance_stars(0.03), "*")
  })

  it("returns empty for p >= 0.05", {
    expect_equal(impl$significance_stars(0.1), "")
  })

  it("returns empty for NA", {
    expect_equal(impl$significance_stars(NA), "")
  })
})
