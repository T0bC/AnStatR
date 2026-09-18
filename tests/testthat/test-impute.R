box::use(
  testthat[
    describe,
    expect_equal,
    expect_false,
  ],
  testthat[
    expect_true,
    it,
  ],
)

box::use(
  app/logic/preprocessing/impute,
)

# =============================================================================
# Helper: a well-conditioned matrix with a controllable set of gaps
# =============================================================================

make_data <- function(n = 60, p = 8, na_cells = integer(0)) {
  set.seed(42)
  df <- as.data.frame(
    matrix(stats::rnorm(n * p), nrow = n)
  )
  colnames(df) <- paste0("V", seq_len(p))
  df$meta <- rep_len(c("A", "B"), n)
  for (cell in na_cells) {
    df[[cell[[1]]]][cell[[2]]] <- NA
  }
  df
}

meas <- function(p = 8) paste0("V", seq_len(p))


# =============================================================================
# assess_imputation
# =============================================================================

describe("assess_imputation", {
  it("reports zero missing for complete data", {
    a <- impute$assess_imputation(make_data(), meas())
    expect_equal(a$n_missing, 0L)
    expect_equal(a$percent_missing, 0)
    expect_true(a$can_impute)
  })

  it("counts missing cells and overall percentage", {
    df <- make_data()
    df$V1[1:6] <- NA
    a <- impute$assess_imputation(df, meas())
    expect_equal(a$n_missing, 6L)
    # 6 of 60 * 8 = 480 cells
    expect_equal(a$percent_missing, 1.2)
    expect_true(a$can_impute)
  })

  it("blocks a column above the per-column cap", {
    df <- make_data()
    # 21 of 60 rows = 35%, over the 20% cap
    df$V2[1:21] <- NA
    a <- impute$assess_imputation(df, meas())
    expect_false(a$can_impute)
    expect_equal(a$cols_over_cap$column, "V2")
    expect_equal(a$cols_over_cap$na_percent, 35)
  })

  it("applies the cap per column, not globally", {
    # Overall missingness stays low while one column is ruinous —
    # the case a global threshold would wave through.
    df <- make_data()
    df$V3[1:30] <- NA
    a <- impute$assess_imputation(df, meas())
    expect_true(a$percent_missing < 10)
    expect_false(a$can_impute)
    expect_equal(a$cols_over_cap$column, "V3")
  })

  it("identifies all-NA columns separately from capped ones", {
    df <- make_data()
    df$V4 <- NA_real_
    a <- impute$assess_imputation(df, meas())
    expect_equal(a$cols_all_na, "V4")
    expect_false("V4" %in% a$cols_over_cap$column)
    expect_false(a$can_impute)
  })

  it("identifies rows with no observed measurement", {
    df <- make_data()
    df[7, meas()] <- NA
    a <- impute$assess_imputation(df, meas())
    expect_equal(a$rows_all_na, 7L)
  })

  it("honours a caller-supplied cap", {
    df <- make_data()
    df$V2[1:21] <- NA
    a <- impute$assess_imputation(df, meas(), max_col_na_percent = 50)
    expect_true(a$can_impute)
  })
})


# =============================================================================
# choose_impute_ncomp
# =============================================================================

describe("choose_impute_ncomp", {
  it("defaults to 5 when the matrix allows it", {
    expect_equal(impute$choose_impute_ncomp(60, 8), 5)
  })

  it("never exceeds the matrix dimensions", {
    expect_equal(impute$choose_impute_ncomp(60, 3), 2)
    expect_equal(impute$choose_impute_ncomp(3, 20), 2)
  })

  it("returns at least 1", {
    expect_equal(impute$choose_impute_ncomp(1, 1), 1)
  })

  it("respects an explicit request", {
    expect_equal(impute$choose_impute_ncomp(60, 20, requested = 3), 3)
  })
})


# =============================================================================
# impute_missing
# =============================================================================

describe("impute_missing", {
  it("fills every gap", {
    df <- make_data()
    df$V1[c(2, 9)] <- NA
    df$V5[4] <- NA
    result <- impute$impute_missing(df, meas())
    expect_true(result$success)
    expect_false(anyNA(result$result$data[, meas()]))
    expect_equal(result$result$n_imputed, 3L)
    expect_equal(result$result$rows_affected, 3L)
  })

  it("leaves observed values bit-identical", {
    df <- make_data()
    df$V1[c(2, 9)] <- NA
    result <- impute$impute_missing(df, meas())
    expect_true(result$success)
    out <- result$result$data
    observed <- !is.na(df$V2)
    expect_equal(out$V2[observed], df$V2[observed])
    # And the untouched cells of the imputed column itself
    expect_equal(out$V1[-c(2, 9)], df$V1[-c(2, 9)])
  })

  it("preserves metadata columns", {
    df <- make_data()
    df$V1[3] <- NA
    result <- impute$impute_missing(df, meas())
    expect_true(result$success)
    expect_equal(result$result$data$meta, df$meta)
  })

  it("returns complete data untouched without running NIPALS", {
    df <- make_data()
    result <- impute$impute_missing(df, meas())
    expect_true(result$success)
    expect_equal(result$result$n_imputed, 0L)
    expect_equal(result$result$ncomp_used, 0L)
    expect_equal(result$result$data, df)
  })

  it("drops rows with no observed measurement", {
    # mixOmics::impute.nipals() errors on an all-NA row; there is
    # nothing to reconstruct from, so it must go first.
    df <- make_data()
    df[7, meas()] <- NA
    df$V1[2] <- NA
    result <- impute$impute_missing(df, meas())
    expect_true(result$success)
    expect_equal(result$result$rows_dropped, 1L)
    expect_equal(nrow(result$result$data), nrow(df) - 1)
    expect_false(anyNA(result$result$data[, meas()]))
  })

  it("refuses a column above the cap, naming it", {
    df <- make_data()
    df$V2[1:21] <- NA
    result <- impute$impute_missing(df, meas())
    expect_false(result$success)
    expect_true(grepl("V2", result$error$message))
    expect_true(grepl("35", result$error$message))
  })

  it("refuses an all-NA column", {
    df <- make_data()
    df$V4 <- NA_real_
    result <- impute$impute_missing(df, meas())
    expect_false(result$success)
    expect_true(grepl("V4", result$error$message))
  })

  it("refuses fewer than 2 measurement columns", {
    df <- make_data()
    df$V1[3] <- NA
    result <- impute$impute_missing(df, "V1")
    expect_false(result$success)
    expect_true(grepl("at least 2", result$error$message))
  })

  it("refuses non-numeric measurement columns", {
    df <- make_data()
    df$V1[3] <- NA
    result <- impute$impute_missing(df, c("V1", "V2", "meta"))
    expect_false(result$success)
    expect_true(grepl("meta", result$error$message))
  })

  it("reports the component count actually used", {
    df <- make_data()
    df$V1[3] <- NA
    result <- impute$impute_missing(df, meas(), ncomp = 3)
    expect_true(result$success)
    expect_equal(result$result$ncomp_used, 3L)
  })

  it("recovers values close to the truth on structured data", {
    # Correlated columns: NIPALS should land near the real value,
    # which is the whole premise of offering imputation at all.
    set.seed(7)
    latent <- stats::rnorm(80)
    df <- data.frame(
      V1 = latent + stats::rnorm(80, sd = 0.1),
      V2 = latent + stats::rnorm(80, sd = 0.1),
      V3 = latent + stats::rnorm(80, sd = 0.1),
      V4 = latent + stats::rnorm(80, sd = 0.1)
    )
    truth <- df$V1[5]
    df$V1[5] <- NA
    result <- impute$impute_missing(df, paste0("V", 1:4))
    expect_true(result$success)
    err <- abs(result$result$data$V1[5] - truth)
    expect_true(err < 0.5)
  })
})


# =============================================================================
# impute_error_parser
# =============================================================================

describe("impute_error_parser", {
  it("passes through cap messages with their column names", {
    msg <- impute$impute_error_parser(
      "Column(s) above the 20% missing-value cap: V2 (35%)."
    )
    expect_true(grepl("V2", msg))
  })

  it("explains the two-column minimum", {
    msg <- impute$impute_error_parser(
      "NIPALS imputation needs at least 2 measurement columns"
    )
    expect_true(grepl("at least 2", msg))
  })

  it("gives a fallback for unknown errors", {
    msg <- impute$impute_error_parser("something broke")
    expect_true(grepl("something broke", msg))
  })
})


# =============================================================================
# build_impute_spec
# =============================================================================

describe("build_impute_spec", {
  it("returns NULL when nothing was imputed", {
    expect_true(is.null(impute$build_impute_spec(NULL)))
    expect_true(is.null(impute$build_impute_spec(
      list(n_imputed = 0L, data = data.frame(x = 1))
    )))
  })

  it("keeps the counts and drops the data frame", {
    df <- make_data()
    df$V1[c(2, 9)] <- NA
    result <- impute$impute_missing(df, meas())
    spec <- impute$build_impute_spec(result$result)
    expect_equal(spec$n_imputed, 2L)
    expect_equal(spec$rows_affected, 2L)
    expect_true(is.null(spec$data))
    expect_equal(
      sort(names(spec)),
      sort(c(
        "n_imputed", "rows_affected", "rows_dropped",
        "ncomp_used", "percent_missing"
      ))
    )
  })
})
