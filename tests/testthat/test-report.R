box::use(
  testthat[describe, expect_true, expect_false, it],
)

box::use(
  app/logic/statistics/report,
)

# =============================================================================
# Helper: minimal Lincon/Cliff post-hoc table (matches robust output shape)
# =============================================================================

make_lincon_cliff_df <- function() {
  data.frame(
    Interaction = c("A.T1 vs. A.T2", "A.T1 vs. B.T1"),
    Lincon.psihat = c(0.5, 0.3),
    Lincon.ci.lower = c(-0.1, -0.2),
    Lincon.ci.upper = c(1.1, 0.8),
    Lincon.p.value = c(0.04, 0.20),
    Lincon.p.adjusted = c(0.08, 0.40),
    Cliff.psihat = c(0.16, -0.25),
    Cliff.ci.lower = c(NA, -0.6),
    Cliff.ci.upper = c(NA, 0.1),
    Cliff.p.value = c(NA, 0.30),
    Cliff.p.adjusted = c(NA, 0.60),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# build_posthoc_html — robust RM note
# =============================================================================

describe("build_posthoc_html robust RM note", {
  it("renders the Yuen paired note for robust RM results", {
    df <- make_lincon_cliff_df()
    html <- report$build_posthoc_html(
      df,
      params = list(
        is_repeated_measures = TRUE,
        test_approach = "robust",
        rm_within_col = "TIME"
      )
    )
    expect_true(grepl("Repeated measures", html, fixed = TRUE))
    expect_true(grepl("yuend", html, fixed = TRUE))
    expect_true(grepl("AKP", html, fixed = TRUE))
    expect_true(grepl("TIME", html, fixed = TRUE))
  })

  it("omits the note when repeated measures is off", {
    df <- make_lincon_cliff_df()
    html <- report$build_posthoc_html(
      df,
      params = list(
        is_repeated_measures = FALSE,
        test_approach = "robust",
        rm_within_col = "TIME"
      )
    )
    expect_false(grepl("Repeated measures", html, fixed = TRUE))
  })

  it("omits the note when params is NULL", {
    df <- make_lincon_cliff_df()
    html <- report$build_posthoc_html(df, params = NULL)
    expect_false(grepl("Repeated measures", html, fixed = TRUE))
  })
})
