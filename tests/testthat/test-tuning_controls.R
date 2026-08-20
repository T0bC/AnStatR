box::use(
  testthat[
    describe, expect_equal, expect_false, expect_null,
    expect_true, it
  ],
)

box::use(
  app/view/shared/tuning_controls[
    check_cv_settings, estimate_cv_runtime, parse_keepx_grid
  ],
)

describe("check_cv_settings", {
  # mixOmics advises >= 5-6 samples per fold and 50-100 repeats
  # for a final reported result.
  it("is silent when the settings follow the guidance", {
    expect_equal(
      length(check_cv_settings(
        n_samples = 100, folds = 5, repeats = 50
      )),
      0
    )
  })

  it("warns when folds leave too few samples per fold", {
    # 20 samples / 10 folds = 2 per fold: below mixOmics' hard
    # minimum of 3.
    msgs <- check_cv_settings(
      n_samples = 20, folds = 10, repeats = 50
    )
    expect_true(length(msgs) > 0)
    expect_true(any(grepl("fold", msgs)))
  })

  it("flags a borderline fold count separately from a failing one", {
    borderline <- check_cv_settings(
      n_samples = 20, folds = 5, repeats = 50
    )
    failing <- check_cv_settings(
      n_samples = 20, folds = 10, repeats = 50
    )
    expect_true(any(grepl("noisy", borderline)))
    expect_true(any(grepl("likely", failing)))
  })

  it("advises raising repeats below 50", {
    msgs <- check_cv_settings(
      n_samples = 100, folds = 5, repeats = 3
    )
    expect_true(any(grepl("50-100 repeats", msgs)))
  })

  it("tolerates absent or unusable input", {
    expect_equal(length(check_cv_settings()), 0)
    expect_equal(
      length(check_cv_settings(
        n_samples = NULL, folds = NULL, repeats = NULL
      )),
      0
    )
  })
})

describe("estimate_cv_runtime", {
  base_args <- list(
    n_samples = 100, n_vars = 50, folds = 5,
    repeats = 10, n_grid = 5, ncomp = 2, method = "spca"
  )

  call_est <- function(...) {
    do.call(estimate_cv_runtime, utils::modifyList(
      base_args, list(...)
    ))
  }

  it("grows with more folds", {
    expect_true(
      call_est(folds = 10)$seconds > call_est(folds = 5)$seconds
    )
  })

  it("grows with more repeats", {
    expect_true(
      call_est(repeats = 20)$seconds >
        call_est(repeats = 10)$seconds
    )
  })

  it("grows with a longer keepX grid", {
    expect_true(
      call_est(n_grid = 10)$seconds > call_est(n_grid = 5)$seconds
    )
  })

  it("grows with data size", {
    expect_true(
      call_est(n_samples = 1000)$seconds >
        call_est(n_samples = 100)$seconds
    )
  })

  it("tiers a tiny job as fast and a huge one as slow", {
    tiny <- call_est(
      n_samples = 20, n_vars = 5, folds = 2,
      repeats = 1, n_grid = 2, ncomp = 1
    )
    huge <- call_est(
      n_samples = 5000, n_vars = 2000, folds = 10,
      repeats = 50, n_grid = 10, ncomp = 5
    )
    expect_equal(tiny$tier, "fast")
    expect_equal(huge$tier, "slow")
  })

  it("returns NULL rather than guessing on unusable input", {
    expect_null(call_est(n_vars = 0))
    expect_null(call_est(folds = NA))
    expect_null(call_est(repeats = -1))
  })

  it("tolerates arguments being absent or NULL", {
    # Shiny inputs are NULL before the UI initialises, so a
    # renderUI calling this must never hit a missing-argument error.
    expect_null(estimate_cv_runtime())
    expect_null(estimate_cv_runtime(
      n_samples = NULL, n_vars = NULL,
      folds = NULL, repeats = NULL
    ))
  })

  it("always produces a human-readable label", {
    expect_true(nzchar(call_est()$label))
  })
})


describe("parse_keepx_grid", {
  it("reads a comma-separated list", {
    expect_equal(
      parse_keepx_grid("5, 10, 15", n_vars = 50)$values,
      c(5L, 10L, 15L)
    )
  })

  it("reads a space-separated list", {
    expect_equal(
      parse_keepx_grid("5 10 15", n_vars = 50)$values,
      c(5L, 10L, 15L)
    )
  })

  it("sorts and de-duplicates", {
    expect_equal(
      parse_keepx_grid("15, 5, 5, 10", n_vars = 50)$values,
      c(5L, 10L, 15L)
    )
  })

  it("caps values above the variable count and says so", {
    res <- parse_keepx_grid("5, 10, 200", n_vars = 20)
    expect_equal(res$values, c(5L, 10L, 20L))
    expect_false(is.null(res$message))
  })

  it("signals fallback with NULL for blank input", {
    res <- parse_keepx_grid("   ", n_vars = 50)
    expect_null(res$values)
    expect_null(res$message)
  })

  it("warns and falls back when nothing is usable", {
    res <- parse_keepx_grid("abc, def", n_vars = 50)
    expect_null(res$values)
    expect_false(is.null(res$message))
  })

  it("drops non-positive values", {
    res <- parse_keepx_grid("0, -5, 10", n_vars = 50)
    expect_equal(res$values, 10L)
  })
})
