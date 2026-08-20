box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/pca/pca,
)

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
# run_pca — standard PCA
# =============================================================================

describe("run_pca (pca)", {
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

  it("result contains model, scores, loadings, variance", {
    res <- pca$run_pca(test_data, c("a", "b", "c", "d"))
    expect_true(res$success)
    r <- res$result
    expect_true("model" %in% names(r))
    expect_true("scores" %in% names(r))
    expect_true("loadings" %in% names(r))
    expect_true("variance" %in% names(r))
    expect_equal(r$analysis_type, "pca")
  })

  it("variance table has one row per component", {
    cols <- c("a", "b", "c", "d")
    res <- pca$run_pca(test_data, cols)
    r <- res$result
    expect_equal(nrow(r$variance), length(cols))
    expect_true("variance_percent" %in% names(r$variance))
    expect_true(
      "cumulative_variance_percent" %in% names(r$variance)
    )
  })

  it("cumulative variance reaches 100", {
    res <- pca$run_pca(test_data, c("a", "b", "c", "d"))
    r <- res$result
    last_cum <- r$variance[
      nrow(r$variance), "cumulative_variance_percent"
    ]
    expect_equal(last_cum, 100, tolerance = 1e-6)
  })

  it("default ncp=NULL retains all components", {
    cols <- c("a", "b", "c", "d")
    res <- pca$run_pca(test_data, cols)
    r <- res$result
    expect_equal(ncol(r$loadings), length(cols))
    expect_equal(ncol(r$scores), length(cols))
    expect_equal(nrow(r$variance), length(cols))
  })

  it("explicit ncp limits retained components", {
    res <- pca$run_pca(
      test_data, c("a", "b", "c", "d"), ncp = 2
    )
    r <- res$result
    expect_equal(ncol(r$loadings), 2)
    expect_equal(ncol(r$scores), 2)
    expect_equal(nrow(r$variance), 2)
  })

  it("works with 2 columns (minimum)", {
    res <- pca$run_pca(test_data, c("a", "b"))
    expect_true(res$success)
    r <- res$result
    expect_equal(nrow(r$variance), 2)
  })

  it("stores center/scale vectors named by column", {
    res <- pca$run_pca(
      test_data, c("a", "b", "c", "d"),
      center = TRUE, scale. = TRUE
    )
    r <- res$result
    expect_equal(names(r$center), c("a", "b", "c", "d"))
    expect_equal(names(r$scale), c("a", "b", "c", "d"))
  })
})

# =============================================================================
# run_pca — sPCA
# =============================================================================

describe("run_pca (spca)", {
  test_data <- data.frame(
    a = rnorm(20, mean = 10, sd = 2),
    b = rnorm(20, mean = 5, sd = 1),
    c = rnorm(20, mean = 0, sd = 3),
    d = rnorm(20, mean = 20, sd = 5),
    e = rnorm(20, mean = 1, sd = 1)
  )
  cols <- c("a", "b", "c", "d", "e")

  it("returns success with valid keepX", {
    res <- pca$run_pca(
      test_data, cols,
      analysis_type = "spca", ncp = 3,
      keep_x = c(3, 3, 3), center = TRUE
    )
    expect_true(res$success)
    expect_equal(res$result$analysis_type, "spca")
  })

  it("fails when keepX is NULL", {
    res <- pca$run_pca(
      test_data, cols,
      analysis_type = "spca", ncp = 3, keep_x = NULL
    )
    expect_true(!res$success)
  })

  it("fails when keepX length does not match ncp", {
    res <- pca$run_pca(
      test_data, cols,
      analysis_type = "spca", ncp = 3,
      keep_x = c(3, 3)
    )
    expect_true(!res$success)
  })

  it("produces sparse loadings honoring keepX", {
    res <- pca$run_pca(
      test_data, cols,
      analysis_type = "spca", ncp = 2,
      keep_x = c(2, 2), center = TRUE
    )
    r <- res$result
    n_nonzero_comp1 <- sum(r$loadings[, 1] != 0)
    expect_equal(n_nonzero_comp1, 2)
  })

  it("includes keep_x and selected_variables in result", {
    res <- pca$run_pca(
      test_data, cols,
      analysis_type = "spca", ncp = 2,
      keep_x = c(2, 2), center = TRUE
    )
    r <- res$result
    expect_true("keep_x" %in% names(r))
    expect_true("selected_variables" %in% names(r))
    expect_equal(length(r$selected_variables[["Dim.1"]]), 2)
  })
})

# =============================================================================
# run_pca — IPCA
# =============================================================================

describe("run_pca (ipca)", {
  test_data <- data.frame(
    a = rnorm(30, mean = 10, sd = 2),
    b = rnorm(30, mean = 5, sd = 1),
    c = rnorm(30, mean = 0, sd = 3),
    d = rnorm(30, mean = 20, sd = 5)
  )
  cols <- c("a", "b", "c", "d")

  it("returns success for deflation mode", {
    res <- pca$run_pca(
      test_data, cols,
      analysis_type = "ipca", ncp = 3,
      ipca_mode = "deflation"
    )
    expect_true(res$success)
    expect_equal(res$result$analysis_type, "ipca")
  })

  it("returns success for parallel mode", {
    res <- pca$run_pca(
      test_data, cols,
      analysis_type = "ipca", ncp = 3,
      ipca_mode = "parallel"
    )
    expect_true(res$success)
  })

  it("scores dimensions match n and ncp", {
    res <- pca$run_pca(
      test_data, cols,
      analysis_type = "ipca", ncp = 3
    )
    r <- res$result
    expect_equal(nrow(r$scores), 30)
    expect_equal(ncol(r$scores), 3)
  })

  it("always records true column means as center", {
    # ipca() has no `center` argument and always centers
    # internally — even when center=FALSE is requested,
    # $center must reflect the actual column means used.
    res <- pca$run_pca(
      test_data, cols,
      analysis_type = "ipca", ncp = 2, center = FALSE
    )
    r <- res$result
    expect_equal(
      as.numeric(r$center), as.numeric(colMeans(test_data[cols])),
      tolerance = 1e-8
    )
  })
})

# =============================================================================
# run_pca_tune_keepx
# =============================================================================

describe("run_pca_tune_keepx", {
  test_data <- data.frame(
    a = rnorm(30), b = rnorm(30), c = rnorm(30),
    d = rnorm(30), e = rnorm(30), f = rnorm(30)
  )
  cols <- names(test_data)

  it("returns a named keepX vector with nrepeat >= 3", {
    res <- pca$run_pca_tune_keepx(
      test_data, cols, ncomp = 2,
      test_keep_x = c(2, 4, 6),
      folds = 3, repeats = 3
    )
    expect_true(res$success)
    expect_equal(length(res$result$keep_x), 2)
    expect_equal(names(res$result$keep_x), c("Dim.1", "Dim.2"))
  })

  it("records the settings actually used, for provenance", {
    res <- pca$run_pca_tune_keepx(
      test_data, cols, ncomp = 2,
      test_keep_x = c(2, 4, 6),
      folds = 3, repeats = 3
    )
    expect_true(res$success)
    expect_equal(res$result$settings$folds, 3)
    expect_equal(res$result$settings$repeats, 3)
    expect_equal(res$result$settings$grid, c(2, 4, 6))
  })

  it("returns the component-stability values used to choose keepX", {
    res <- pca$run_pca_tune_keepx(
      test_data, cols, ncomp = 2,
      test_keep_x = c(2, 4, 6),
      folds = 3, repeats = 3
    )
    expect_true(res$success)
    # $cor_comp is the evidence behind the choice; without it the
    # stability curve has nothing to plot.
    expect_false(is.null(res$result$cor_comp))
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

  it("attaches metadata to ind_meta", {
    res <- pca$run_pca(
      test_data, c("x", "y", "z"),
      meta_cols = c("SEX", "TREATMENT")
    )
    expect_true(res$success)
    r <- res$result
    expect_true("ind_meta" %in% names(r))
    expect_equal(ncol(r$ind_meta), 2)
    expect_equal(nrow(r$ind_meta), 5)
    expect_equal(names(r$ind_meta), c("SEX", "TREATMENT"))
  })

  it("uses metadata for row labels", {
    res <- pca$run_pca(
      test_data, c("x", "y", "z"),
      meta_cols = c("SEX", "TREATMENT")
    )
    r <- res$result
    labels <- rownames(r$scores)
    expect_true(all(grepl("\\|", labels)))
  })

  it("falls back to row numbers without meta_cols", {
    res <- pca$run_pca(
      test_data, c("x", "y", "z")
    )
    r <- res$result
    expect_true("ind_meta" %in% names(r))
    expect_equal(names(r$ind_meta), "Row")
    labels <- rownames(r$scores)
    expect_equal(labels, as.character(1:5))
  })

  it("handles duplicate metadata labels", {
    res <- pca$run_pca(
      test_data, c("x", "y", "z"),
      meta_cols = c("SEX")
    )
    r <- res$result
    labels <- rownames(r$scores)
    # 3 M's and 2 F's — duplicates get suffixed
    expect_equal(length(unique(labels)), 5)
  })
})

# =============================================================================
# extract_pca_scores / extract_variance_explained
# =============================================================================

describe("extract_pca_scores", {
  test_data <- data.frame(
    SEX = c("M", "F", "M", "F", "M"),
    x = c(1.0, 2.0, 3.0, 4.0, 5.0),
    y = c(2.0, 4.0, 5.0, 4.0, 5.0),
    z = c(3.0, 1.0, 2.0, 5.0, 4.0)
  )

  it("returns NULL when the reactive is NULL", {
    expect_true(is.null(pca$extract_pca_scores(NULL)))
  })

  it("returns NULL when the wrapped result failed", {
    fake_reactive <- function() list(success = FALSE)
    expect_true(is.null(pca$extract_pca_scores(fake_reactive)))
  })

  it("combines metadata and scores into one data frame", {
    res <- pca$run_pca(
      test_data, c("x", "y", "z"), meta_cols = "SEX"
    )
    fake_reactive <- function() res
    df <- pca$extract_pca_scores(fake_reactive)
    expect_true("SEX" %in% names(df))
    expect_true("Dim.1" %in% names(df))
    expect_equal(nrow(df), 5)
  })
})

describe("extract_variance_explained", {
  test_data <- data.frame(
    x = rnorm(20), y = rnorm(20), z = rnorm(20), w = rnorm(20)
  )

  it("returns NULL when the reactive is NULL", {
    expect_true(is.null(pca$extract_variance_explained(NULL)))
  })

  it("returns n90/cum90/n95/cum95", {
    res <- pca$run_pca(test_data, names(test_data))
    fake_reactive <- function() res
    rec <- pca$extract_variance_explained(fake_reactive)
    expect_true(all(
      c("n90", "cum90", "n95", "cum95") %in% names(rec)
    ))
    expect_true(rec$cum90 >= 90 || rec$n90 == ncol(test_data))
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

  it("parses keepX errors distinctly from numeric errors", {
    msg <- pca$pca_error_parser(
      "keepX invalid or incomplete: a numeric value is required"
    )
    expect_true(grepl("keepX", msg))
  })

  it("falls back for unknown errors", {
    msg <- pca$pca_error_parser("something weird happened")
    expect_true(grepl("failed", msg))
  })
})
