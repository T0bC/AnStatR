box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/pca/pca,
)

impl <- attr(pca, "namespace")

# =============================================================================
# validate_inputs
# =============================================================================

describe("validate_inputs", {
  it("returns valid = TRUE for valid columns", {
    data <- data.frame(a = 1:3, b = 4:6, c = 7:9)
    result <- pca$validate_inputs(c("a", "b"), data)
    expect_true(result$valid)
  })

  it("returns valid = FALSE when no columns selected", {
    data <- data.frame(a = 1:3)
    result <- pca$validate_inputs(NULL, data)
    expect_true(!result$valid)
  })

  it("returns valid = FALSE for missing columns", {
    data <- data.frame(a = 1:3)
    result <- pca$validate_inputs(c("a", "z"), data)
    expect_true(!result$valid)
  })
})

# =============================================================================
# run_pca
# =============================================================================

describe("run_pca", {
  # Shared test data: 20 observations, 4 variables
  test_data <- data.frame(
    a = rnorm(20, mean = 10, sd = 2),
    b = rnorm(20, mean = 5, sd = 1),
    c = rnorm(20, mean = 0, sd = 3),
    d = rnorm(20, mean = 20, sd = 5)
  )

  it("returns success for valid numeric data", {
    res <- pca$run_pca(test_data, c("a", "b", "c", "d"))
    expect_true(res$success)
  })

  it("returns error for non-existent columns", {
    res <- pca$run_pca(test_data, c("nonexistent"))
    expect_true(!res$success)
  })

  it("result contains eig, var, and ind", {
    res <- pca$run_pca(test_data, c("a", "b", "c", "d"))
    expect_true(res$success)
    r <- res$result
    expect_true("eig" %in% names(r))
    expect_true("var" %in% names(r))
    expect_true("ind" %in% names(r))
  })

  it("eigenvalue table has correct dimensions", {
    cols <- c("a", "b", "c", "d")
    res <- pca$run_pca(test_data, cols)
    r <- res$result
    # One row per variable
    expect_equal(nrow(r$eig), length(cols))
    expect_equal(ncol(r$eig), 3)
  })

  it("cumulative variance sums to 100", {
    res <- pca$run_pca(test_data, c("a", "b", "c", "d"))
    r <- res$result
    last_cum <- r$eig[nrow(r$eig), "cumulative.variance.percent"]
    expect_equal(last_cum, 100, tolerance = 1e-10)
  })

  it("variable contributions sum to 100 per component", {
    res <- pca$run_pca(test_data, c("a", "b", "c", "d"))
    r <- res$result
    col_sums <- colSums(r$var$contrib)
    expect_equal(
      as.numeric(col_sums),
      rep(100, length(col_sums)),
      tolerance = 1e-10
    )
  })

  it("individual contributions sum to 100 per component", {
    res <- pca$run_pca(test_data, c("a", "b", "c", "d"))
    r <- res$result
    col_sums <- colSums(r$ind$contrib)
    expect_equal(
      as.numeric(col_sums),
      rep(100, length(col_sums)),
      tolerance = 1e-10
    )
  })

  it("default ncp=NULL retains all components", {
    cols <- c("a", "b", "c", "d")
    res <- pca$run_pca(test_data, cols)
    r <- res$result
    # All 4 components retained in var/ind matrices
    expect_equal(ncol(r$var$coord), length(cols))
    expect_equal(ncol(r$ind$coord), length(cols))
    expect_equal(nrow(r$eig), length(cols))
  })

  it("explicit ncp limits retained components", {
    res <- pca$run_pca(
      test_data, c("a", "b", "c", "d"), ncp = 2
    )
    r <- res$result
    expect_equal(ncol(r$var$coord), 2)
    expect_equal(ncol(r$ind$coord), 2)
    # eig still has all components
    expect_equal(nrow(r$eig), 4)
  })

  it("works with 2 columns (minimum)", {
    res <- pca$run_pca(test_data, c("a", "b"))
    expect_true(res$success)
    r <- res$result
    expect_equal(nrow(r$eig), 2)
  })
})

# =============================================================================
# run_pca with meta_cols
# =============================================================================

describe("run_pca with meta_cols", {
  test_data <- data.frame(
    SEX = c("M", "F", "M", "F", "M"),
    TREATMENT = c("A", "B", "A", "B", "A"),
    x = c(1.0, 2.0, 3.0, 4.0, 5.0),
    y = c(2.0, 4.0, 5.0, 4.0, 5.0),
    z = c(3.0, 1.0, 2.0, 5.0, 4.0),
    stringsAsFactors = FALSE
  )

  it("attaches metadata to ind$meta", {
    res <- pca$run_pca(
      test_data, c("x", "y", "z"),
      meta_cols = c("SEX", "TREATMENT")
    )
    expect_true(res$success)
    r <- res$result
    expect_true("meta" %in% names(r$ind))
    expect_equal(ncol(r$ind$meta), 2)
    expect_equal(nrow(r$ind$meta), 5)
    expect_equal(names(r$ind$meta), c("SEX", "TREATMENT"))
  })

  it("uses metadata for row labels", {
    res <- pca$run_pca(
      test_data, c("x", "y", "z"),
      meta_cols = c("SEX", "TREATMENT")
    )
    r <- res$result
    labels <- rownames(r$ind$coord)
    expect_true(all(grepl("\\|", labels)))
  })

  it("falls back to row numbers without meta_cols", {
    res <- pca$run_pca(
      test_data, c("x", "y", "z")
    )
    r <- res$result
    expect_true("meta" %in% names(r$ind))
    expect_equal(names(r$ind$meta), "Row")
    labels <- rownames(r$ind$coord)
    expect_equal(labels, as.character(1:5))
  })

  it("handles duplicate metadata labels", {
    res <- pca$run_pca(
      test_data, c("x", "y", "z"),
      meta_cols = c("SEX")
    )
    r <- res$result
    labels <- rownames(r$ind$coord)
    # 3 M's and 2 F's — duplicates get suffixed
    expect_equal(length(unique(labels)), 5)
  })
})

# =============================================================================
# build_pca_result (internal)
# =============================================================================

describe("build_pca_result", {
  it("produces correct structure from prcomp", {
    test_data <- data.frame(
      x = c(1, 2, 3, 4, 5),
      y = c(2, 4, 5, 4, 5),
      z = c(3, 1, 2, 5, 4)
    )
    pca_obj <- stats::prcomp(
      test_data, center = FALSE, scale. = FALSE
    )
    r <- impl$build_pca_result(pca_obj, ncp = 2, n = 5, p = 3)

    expect_true("eig" %in% names(r))
    expect_true("var" %in% names(r))
    expect_true("ind" %in% names(r))
    expect_equal(r$ncp, 2)
    expect_equal(ncol(r$var$coord), 2)
    expect_equal(nrow(r$ind$coord), 5)
  })
})

# =============================================================================
# pca_error_parser
# =============================================================================

describe("pca_error_parser", {
  it("parses singular matrix error", {
    msg <- pca$pca_error_parser("matrix is singular")
    expect_true(grepl("singular", msg, ignore.case = TRUE))
  })

  it("parses missing values error", {
    msg <- pca$pca_error_parser("contains NA values")
    expect_true(grepl("missing", msg, ignore.case = TRUE))
  })

  it("falls back for unknown errors", {
    msg <- pca$pca_error_parser("something weird happened")
    expect_true(grepl("failed", msg))
  })
})
