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
    expect_true(grepl("sign test", html, fixed = TRUE))
    expect_true(grepl("P(X&lt;Y)", html, fixed = TRUE))
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

# =============================================================================
# generate_html_report — NULL plot_object (screening / no-plots mode)
# =============================================================================

describe("generate_html_report with plot_object = NULL", {
  it("omits the Plot section and does not error", {
    df <- make_lincon_cliff_df()
    html <- report$generate_html_report(
      measure = "Asfc",
      plot_object = NULL,
      omnibus_result = data.frame(
        Df = 3, SS = 1, MS = 1, F_statistic = 1, p_value = 0.05
      ),
      posthoc_result = df,
      params = list(
        test_approach = "robust",
        use_bootstrap = FALSE,
        p_val_cor_method = "bonferroni",
        is_repeated_measures = FALSE
      ),
      x_axis = c("FOOD_TYPE"),
      timestamp = Sys.time()
    )
    expect_false(grepl("<h2>Plot</h2>", html, fixed = TRUE))
    expect_false(grepl("data:image/png", html, fixed = TRUE))
    expect_true(grepl("Pairwise Comparisons", html, fixed = TRUE))
  })
})
