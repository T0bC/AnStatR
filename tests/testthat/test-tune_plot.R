box::use(
  testthat[
    describe, expect_equal, expect_null, expect_true, it
  ],
)

box::use(
  app/logic/pca/tune_plot[create_tune_spca_plot, tidy_cor_comp],
)

# Mirrors the structure mixOmics 6.36 returns in
# tune.spca()$cor.comp: one data frame per component, with the
# correlation in "cor.mean" alongside the keepX grid itself.
make_cor_comp <- function() {
  list(
    comp1 = data.frame(
      keepX = c(2, 4, 6, 8),
      cor.mean = c(0.71, 0.59, 0.64, 0.81),
      cor.sd = c(0.30, 0.11, 0.04, 0.10),
      opt.keepX = c(TRUE, NA, NA, NA)
    ),
    comp2 = data.frame(
      keepX = c(2, 4, 6, 8),
      cor.mean = c(0.50, 0.51, 0.51, 0.64),
      cor.sd = c(0.21, 0.26, 0.24, 0.06),
      opt.keepX = c(TRUE, NA, NA, NA)
    )
  )
}

describe("tidy_cor_comp", {
  it("reads the correlation column, not the keepX grid", {
    df <- tidy_cor_comp(make_cor_comp(), grid = c(2, 4, 6, 8))
    expect_true(is.data.frame(df))
    expect_equal(nrow(df), 8)
    # Regression guard: picking the keepX column instead would
    # plot the x-axis against itself and look plausible.
    expect_equal(
      df$Correlation[df$Component == "Dim.1"],
      c(0.71, 0.59, 0.64, 0.81)
    )
    expect_equal(
      df$keepX[df$Component == "Dim.1"], c(2, 4, 6, 8)
    )
  })

  it("labels components in Dim.N form", {
    df <- tidy_cor_comp(make_cor_comp(), grid = c(2, 4, 6, 8))
    expect_equal(unique(df$Component), c("Dim.1", "Dim.2"))
  })

  it("returns NULL on empty or missing input", {
    expect_null(tidy_cor_comp(NULL, grid = c(2, 4)))
    expect_null(tidy_cor_comp(list(), grid = c(2, 4)))
  })

  it("recovers keepX from the block when the grid disagrees", {
    # A stale grid must not silently misalign the x-axis.
    df <- tidy_cor_comp(make_cor_comp(), grid = c(1, 2))
    expect_equal(df$keepX[df$Component == "Dim.1"], c(2, 4, 6, 8))
  })
})

describe("create_tune_spca_plot", {
  it("builds a plot from the mixOmics structure", {
    res <- create_tune_spca_plot(
      make_cor_comp(),
      grid = c(2, 4, 6, 8),
      chosen = c(Dim.1 = 2, Dim.2 = 2)
    )
    expect_true(res$success)
  })

  it("fails cleanly rather than erroring on unusable input", {
    res <- create_tune_spca_plot(NULL, grid = c(2, 4))
    expect_true(!is.null(res$error))
  })
})
