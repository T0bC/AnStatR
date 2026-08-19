box::use(
  testthat[describe, expect_equal, expect_false, expect_true, it],
  openxlsx,
)

box::use(
  app/logic/pca/pca_export,
  app/logic/pca/pca[run_pca],
)

# =============================================================================
# Helper: build a real PCA result for testing
# =============================================================================

make_pca_result <- function(analysis_type = "pca", n = 20, p = 5) {
  set.seed(42)
  data <- as.data.frame(
    matrix(rnorm(n * p), nrow = n)
  )
  colnames(data) <- paste0("V", seq_len(p))
  data$G1 <- rep(c("A", "B"), length.out = n)

  keep_x <- if (analysis_type == "spca") rep(3L, p) else NULL

  res <- run_pca(
    data, paste0("V", seq_len(p)),
    meta_cols = "G1",
    center = TRUE, scale. = TRUE,
    analysis_type = analysis_type,
    keep_x = keep_x
  )
  res$result
}

# =============================================================================
# create_pca_excel
# =============================================================================

describe("create_pca_excel", {
  it("writes 7 sheets for analysis_type = 'pca'", {
    pca_res <- make_pca_result("pca")
    file <- tempfile(fileext = ".xlsx")
    on.exit(unlink(file))

    pca_export$create_pca_excel(pca_res, file)

    wb <- openxlsx$loadWorkbook(file)
    sheet_names <- openxlsx$sheets(wb)

    expect_equal(length(sheet_names), 7)
    expect_true("Variance Explained" %in% sheet_names)
    expect_true("Variable Loadings" %in% sheet_names)
    expect_true("Variable Contributions" %in% sheet_names)
    expect_true("Variable Cos2" %in% sheet_names)
    expect_true("Individual Scores" %in% sheet_names)
    expect_true("Individual Contributions" %in% sheet_names)
    expect_true("Individual Cos2" %in% sheet_names)
  })

  it("writes 3 sheets for analysis_type = 'ipca', omitting contrib/cos2", {
    pca_res <- make_pca_result("ipca")
    file <- tempfile(fileext = ".xlsx")
    on.exit(unlink(file))

    pca_export$create_pca_excel(pca_res, file)

    wb <- openxlsx$loadWorkbook(file)
    sheet_names <- openxlsx$sheets(wb)

    expect_equal(length(sheet_names), 3)
    expect_true("Variance Explained" %in% sheet_names)
    expect_true("Variable Loadings" %in% sheet_names)
    expect_true("Individual Scores" %in% sheet_names)
    expect_false("Variable Contributions" %in% sheet_names)
    expect_false("Variable Cos2" %in% sheet_names)
    expect_false("Individual Contributions" %in% sheet_names)
    expect_false("Individual Cos2" %in% sheet_names)
  })
})

# =============================================================================
# create_pca_bundle
# =============================================================================

describe("create_pca_bundle", {
  it("returns a bundle matching the result's analysis_type, model, and scale_params", {
    pca_res <- make_pca_result("pca")
    raw_data <- as.data.frame(matrix(rnorm(20 * 5), nrow = 20))

    bundle <- pca_export$create_pca_bundle(
      pca_result = pca_res,
      raw_data = raw_data,
      used_data = raw_data,
      numeric_cols = colnames(raw_data),
      meta_cols = character(0)
    )

    expect_equal(bundle$analysis_type, pca_res$analysis_type)
    expect_true(identical(bundle$model, pca_res$model))
    expect_equal(bundle$scale_params$center, pca_res$center)
    expect_equal(bundle$scale_params$scale, pca_res$scale)
  })

  it("reflects analysis_type = 'spca' rather than hardcoding 'pca'", {
    pca_res <- make_pca_result("spca")
    raw_data <- as.data.frame(matrix(rnorm(20 * 5), nrow = 20))

    bundle <- pca_export$create_pca_bundle(
      pca_result = pca_res,
      raw_data = raw_data,
      used_data = raw_data,
      numeric_cols = colnames(raw_data),
      meta_cols = character(0)
    )

    expect_equal(bundle$analysis_type, "spca")
  })
})
