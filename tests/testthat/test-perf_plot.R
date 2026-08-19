box::use(
  testthat[describe, expect_equal, expect_false, expect_true, it],
)

box::use(
  app/logic/lda/perf_plot[create_perf_error_plot, suggest_ncomp],
)

make_errors <- function(overall, ber) {
  data.frame(
    Component = paste0("Comp", seq_along(ber)),
    `Overall Error` = overall,
    BER = ber,
    check.names = FALSE
  )
}


describe("suggest_ncomp", {
  it("prefers the smallest count within tolerance of the minimum", {
    # Comp3/Comp4 are only marginally better than Comp2 - not worth
    # the extra components.
    expect_equal(suggest_ncomp(c(0.40, 0.20, 0.19, 0.19)), 2L)
  })

  it("picks a clear minimum when the gain is substantial", {
    expect_equal(suggest_ncomp(c(0.40, 0.30, 0.10, 0.25)), 3L)
  })

  it("picks the last component when error falls throughout", {
    expect_equal(suggest_ncomp(c(0.50, 0.40, 0.30, 0.20)), 4L)
  })

  it("picks one component when adding more only hurts", {
    expect_equal(suggest_ncomp(c(0.10, 0.40, 0.50)), 1L)
  })

  it("returns NA for unusable input", {
    expect_true(is.na(suggest_ncomp(numeric(0))))
    expect_true(is.na(suggest_ncomp(c(NA_real_, NA_real_))))
    expect_true(is.na(suggest_ncomp(NULL)))
  })
})


describe("create_perf_error_plot", {
  it("builds an interactive plot from a perf error table", {
    res <- create_perf_error_plot(
      make_errors(c(0.42, 0.21, 0.20), c(0.45, 0.22, 0.19))
    )
    expect_true(res$success)
    expect_true(inherits(res$result, "girafe"))
  })

  it("handles a single-component model", {
    res <- create_perf_error_plot(make_errors(0.3, 0.32))
    expect_true(res$success)
  })

  it("fails cleanly when required columns are missing", {
    res <- create_perf_error_plot(
      data.frame(Component = "Comp1", Something = 1)
    )
    expect_false(isTRUE(res$success))
    expect_true(grepl("missing column", res$error$message))
  })

  it("fails cleanly on empty or NULL input", {
    expect_false(isTRUE(create_perf_error_plot(NULL)$success))
    expect_false(
      isTRUE(create_perf_error_plot(make_errors(
        numeric(0), numeric(0)
      )[0, ])$success)
    )
  })
})
