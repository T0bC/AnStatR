box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/pca/biplot3d,
  app/logic/pca/pca,
)

impl <- attr(biplot3d, "namespace")

# =============================================================================
# Helper: build a real PCA result for testing
# =============================================================================

make_pca_result <- function(n = 20, p = 5,
                            meta_cols = c("G1")) {
  set.seed(42)
  data <- as.data.frame(
    matrix(rnorm(n * p), nrow = n)
  )
  colnames(data) <- paste0("V", seq_len(p))
  data$G1 <- rep(c("A", "B"), length.out = n)

  res <- pca$run_pca(
    data, paste0("V", seq_len(p)),
    meta_cols = meta_cols,
    center = TRUE, scale. = TRUE
  )
  res$result
}


# =============================================================================
# create_biplot3d
# =============================================================================

describe("create_biplot3d", {
  it("returns success with a plotly object", {
    pca_res <- make_pca_result()
    result <- biplot3d$create_biplot3d(
      pca_result = pca_res,
      dim_x = "Dim.1",
      dim_y = "Dim.2",
      dim_z = "Dim.3"
    )
    expect_true(result$success)
    expect_true(inherits(result$result, "plotly"))
  })

  it("returns success with grouping", {
    pca_res <- make_pca_result()
    result <- biplot3d$create_biplot3d(
      pca_result = pca_res,
      dim_x = "Dim.1",
      dim_y = "Dim.2",
      dim_z = "Dim.3",
      group_cols = "G1"
    )
    expect_true(result$success)
  })

  it("returns error for NULL pca_result", {
    result <- biplot3d$create_biplot3d(
      pca_result = NULL
    )
    expect_true(!result$success)
  })

  it("returns error for invalid dimension", {
    pca_res <- make_pca_result()
    result <- biplot3d$create_biplot3d(
      pca_result = pca_res,
      dim_x = "Dim.99",
      dim_y = "Dim.2",
      dim_z = "Dim.3"
    )
    expect_true(!result$success)
  })

  it("returns error when fewer than 3 dims", {
    pca_res <- make_pca_result(n = 20, p = 2)
    result <- biplot3d$create_biplot3d(
      pca_result = pca_res,
      dim_x = "Dim.1",
      dim_y = "Dim.2",
      dim_z = "Dim.3"
    )
    expect_true(!result$success)
  })

  it("returns error for duplicate dimensions", {
    pca_res <- make_pca_result()
    result <- biplot3d$create_biplot3d(
      pca_result = pca_res,
      dim_x = "Dim.1",
      dim_y = "Dim.1",
      dim_z = "Dim.3"
    )
    expect_true(!result$success)
  })
})


# =============================================================================
# validate_biplot3d_inputs
# =============================================================================

describe("validate_biplot3d_inputs", {
  it("passes for valid inputs", {
    pca_res <- make_pca_result()
    expect_true({
      impl$validate_biplot3d_inputs(
        pca_res, "Dim.1", "Dim.2", "Dim.3"
      )
      TRUE
    })
  })
})


# =============================================================================
# build_ind_data
# =============================================================================

describe("build_ind_data", {
  it("returns data frame with group column", {
    pca_res <- make_pca_result()
    dims <- c("Dim.1", "Dim.2", "Dim.3")
    df <- impl$build_ind_data(
      pca_res, dims, "G1"
    )
    expect_true("group" %in% names(df))
    expect_equal(nrow(df), 20)
    expect_true(all(dims %in% names(df)))
  })

  it("uses 'No Grouping' when no group_cols", {
    pca_res <- make_pca_result()
    dims <- c("Dim.1", "Dim.2", "Dim.3")
    df <- impl$build_ind_data(
      pca_res, dims, NULL
    )
    expect_true(
      all(as.character(df$group) == "No Grouping")
    )
  })
})


# =============================================================================
# build_var_data
# =============================================================================

describe("build_var_data", {
  it("returns scaled loadings data frame", {
    pca_res <- make_pca_result()
    dims <- c("Dim.1", "Dim.2", "Dim.3")
    ind_data <- impl$build_ind_data(
      pca_res, dims, NULL
    )
    df <- impl$build_var_data(
      pca_res, dims, ind_data
    )
    expect_equal(nrow(df), 5)
    expect_true(all(dims %in% names(df)))
  })
})


# =============================================================================
# biplot3d_error_parser
# =============================================================================

describe("biplot3d_error_parser", {
  it("handles dimension errors", {
    msg <- biplot3d$biplot3d_error_parser(
      "Dimension not found: Dim.99"
    )
    expect_true(grepl("Invalid dimension", msg))
  })

  it("handles insufficient dimensions", {
    msg <- biplot3d$biplot3d_error_parser(
      "Need at least 3 PCA dimensions"
    )
    expect_true(grepl("at least 3", msg))
  })

  it("handles NULL pca_result", {
    msg <- biplot3d$biplot3d_error_parser(
      "pca_result is NULL"
    )
    expect_true(grepl("No PCA result", msg))
  })

  it("falls back for unknown errors", {
    msg <- biplot3d$biplot3d_error_parser(
      "something unexpected"
    )
    expect_true(grepl("failed:", msg))
  })
})
