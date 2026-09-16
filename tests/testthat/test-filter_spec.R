box::use(
  testthat[
    describe,
    expect_equal,
    expect_false,
    expect_length,
    expect_null,
    expect_true,
    it
  ],
)

box::use(
  app/logic/shared/data_utils,
  app/logic/shared/filter_spec,
)

# A small nested dataset in the shape the app expects: a handful of
# specimens, each measured on several teeth and facets.
make_data <- function(n_specimens = 10) {
  expand <- expand.grid(
    SPECIMEN_ID = paste0("S", seq_len(n_specimens)),
    TOOTH = c("M1", "M2", "P4"),
    FACET = c("buccal", "lingual"),
    stringsAsFactors = FALSE
  )
  expand$JAW <- ifelse(expand$TOOTH == "P4", "upper", "lower")
  expand$Asfc <- seq_len(nrow(expand))
  expand
}

# =============================================================================
# constrained_cols
# =============================================================================

describe("constrained_cols", {
  it("returns nothing for an empty filter state", {
    expect_equal(
      filter_spec$constrained_cols(list(), make_data()),
      character(0)
    )
  })

  it("ignores a column where every level is selected", {
    data <- make_data()
    state <- list(TOOTH = c("M1", "M2", "P4"))
    expect_equal(filter_spec$constrained_cols(state, data), character(0))
  })

  it("detects a column restricted to a subset of its levels", {
    data <- make_data()
    state <- list(TOOTH = "M1")
    expect_equal(filter_spec$constrained_cols(state, data), "TOOTH")
  })

  it("ignores an empty selection, matching filter_data", {
    data <- make_data()
    state <- list(TOOTH = character(0))
    expect_equal(filter_spec$constrained_cols(state, data), character(0))
  })

  it("ignores columns absent from the data", {
    data <- make_data()
    state <- list(MISSING = "x")
    expect_equal(filter_spec$constrained_cols(state, data), character(0))
  })
})

# =============================================================================
# suggest_reapply_cols
# =============================================================================

describe("suggest_reapply_cols", {
  it("suggests a structural column with few recurring levels", {
    data <- make_data()
    state <- list(TOOTH = "M1")
    expect_equal(filter_spec$suggest_reapply_cols(state, data), "TOOTH")
  })

  it("does not suggest a near-unique identity column", {
    data <- make_data(n_specimens = 30)
    state <- list(SPECIMEN_ID = paste0("S", 1:15))
    expect_equal(
      filter_spec$suggest_reapply_cols(state, data),
      character(0)
    )
  })

  it("splits structural from identity in a mixed selection", {
    data <- make_data(n_specimens = 30)
    state <- list(
      TOOTH = "M1",
      SPECIMEN_ID = paste0("S", 1:15)
    )
    expect_equal(filter_spec$suggest_reapply_cols(state, data), "TOOTH")
  })

  it("honours explicitly excluded columns", {
    data <- make_data()
    state <- list(TOOTH = "M1", JAW = "lower")
    result <- filter_spec$suggest_reapply_cols(
      state, data,
      exclude_cols = "JAW"
    )
    expect_equal(result, "TOOTH")
  })

  it("never suggests an unconstrained column", {
    data <- make_data()
    state <- list(TOOTH = c("M1", "M2", "P4"))
    expect_equal(
      filter_spec$suggest_reapply_cols(state, data),
      character(0)
    )
  })
})

# =============================================================================
# build_filter_spec
# =============================================================================

describe("analyze_filter", {
  it("reports constrained and suggested columns together", {
    data <- make_data(n_specimens = 30)
    state <- list(TOOTH = "M1", SPECIMEN_ID = paste0("S", 1:15))
    result <- filter_spec$analyze_filter(state, data)

    expect_equal(sort(result$constrained), c("SPECIMEN_ID", "TOOTH"))
    expect_equal(result$suggested, "TOOTH")
  })

  it("reports nothing when the filter does not narrow anything", {
    data <- make_data()
    result <- filter_spec$analyze_filter(
      list(TOOTH = c("M1", "M2", "P4")), data
    )
    expect_equal(result$constrained, character(0))
    expect_equal(result$suggested, character(0))
  })
})

describe("build_filter_spec", {
  it("returns NULL when nothing is constrained", {
    expect_null(
      filter_spec$build_filter_spec(list(), character(0), character(0))
    )
    expect_null(
      filter_spec$build_filter_spec(
        list(TOOTH = c("M1", "M2", "P4")), character(0), "TOOTH"
      )
    )
  })

  it("records the reapply and training-only split", {
    state <- list(TOOTH = "M1", SPECIMEN_ID = paste0("S", 1:15))
    spec <- filter_spec$build_filter_spec(
      state, c("TOOTH", "SPECIMEN_ID"), "TOOTH"
    )

    expect_equal(names(spec$reapply), "TOOTH")
    expect_equal(spec$reapply$TOOTH, "M1")
    expect_equal(names(spec$training_only), "SPECIMEN_ID")
  })

  it("omits unconstrained columns from both halves", {
    state <- list(TOOTH = "M1", FACET = c("buccal", "lingual"))
    spec <- filter_spec$build_filter_spec(
      state, "TOOTH", c("TOOTH", "FACET")
    )
    expect_equal(names(spec$reapply), "TOOTH")
    expect_length(spec$training_only, 0)
  })

  it("records row counts and a schema version", {
    spec <- filter_spec$build_filter_spec(
      list(TOOTH = "M1"), "TOOTH", "TOOTH",
      n_rows_before = 999,
      n_rows_after = 333
    )
    expect_equal(spec$version, 1L)
    expect_equal(spec$n_rows_before, 999)
    expect_equal(spec$n_rows_after, 333)
  })
})

# =============================================================================
# describe_filter_spec
# =============================================================================

describe("describe_filter_spec", {
  it("returns NULL when there is nothing to reapply", {
    expect_null(filter_spec$describe_filter_spec(NULL))
    expect_null(
      filter_spec$describe_filter_spec(list(reapply = list()))
    )
  })

  it("summarises the reapplied columns only", {
    data <- make_data(n_specimens = 30)
    state <- list(TOOTH = "M1", SPECIMEN_ID = paste0("S", 1:15))
    spec <- filter_spec$build_filter_spec(
      state, c("TOOTH", "SPECIMEN_ID"), "TOOTH"
    )
    result <- filter_spec$describe_filter_spec(spec)

    expect_true(grepl("TOOTH = M1", result, fixed = TRUE))
    expect_false(grepl("SPECIMEN_ID", result, fixed = TRUE))
  })
})

# =============================================================================
# apply_filter_spec
# =============================================================================

describe("apply_filter_spec", {
  it("returns the data unchanged when there is no spec", {
    data <- make_data()
    expect_equal(filter_spec$apply_filter_spec(NULL, data), data)
  })

  it("agrees with filter_data for the reapplied columns", {
    data <- make_data()
    spec <- filter_spec$build_filter_spec(
      list(TOOTH = "M1"), "TOOTH", "TOOTH"
    )
    expect_equal(
      filter_spec$apply_filter_spec(spec, data),
      data_utils$filter_data(data, list(TOOTH = "M1"))
    )
  })

  it("does not apply the training-only half", {
    data <- make_data(n_specimens = 30)
    state <- list(TOOTH = "M1", SPECIMEN_ID = paste0("S", 1:15))
    spec <- filter_spec$build_filter_spec(
      state, c("TOOTH", "SPECIMEN_ID"), "TOOTH"
    )
    result <- filter_spec$apply_filter_spec(spec, data)

    expect_true(all(result$TOOTH == "M1"))
    expect_equal(length(unique(result$SPECIMEN_ID)), 30)
  })

  it("ignores reapply columns absent from the data", {
    data <- make_data()
    spec <- list(reapply = list(MISSING = "x"))
    expect_equal(filter_spec$apply_filter_spec(spec, data), data)
  })
})

# =============================================================================
# check_filter_spec
# =============================================================================

describe("check_filter_spec", {
  it("passes when there is no spec", {
    data <- make_data()
    result <- filter_spec$check_filter_spec(NULL, data)
    expect_length(result$errors, 0)
    expect_equal(result$n_matching, nrow(data))
  })

  it("passes and counts matching rows when the filter applies", {
    data <- make_data()
    spec <- filter_spec$build_filter_spec(
      list(TOOTH = "M1"), "TOOTH", "TOOTH"
    )
    result <- filter_spec$check_filter_spec(spec, data)

    expect_length(result$errors, 0)
    expect_equal(result$n_matching, sum(data$TOOTH == "M1"))
  })

  it("errors when a required column is missing", {
    data <- make_data()
    spec <- list(reapply = list(TOOTH = "M1"))
    result <- filter_spec$check_filter_spec(spec, data[, c("FACET", "Asfc")])

    expect_length(result$errors, 1)
    expect_true(grepl("no 'TOOTH' column", result$errors, fixed = TRUE))
    expect_equal(result$n_matching, 0L)
  })

  it("errors with a level diff when no required level is present", {
    data <- make_data()
    data$TOOTH <- tolower(data$TOOTH)
    spec <- list(reapply = list(TOOTH = "M1"))
    result <- filter_spec$check_filter_spec(spec, data)

    expect_length(result$errors, 1)
    expect_true(grepl("Required: M1", result$errors, fixed = TRUE))
    expect_true(grepl("m1", result$errors, fixed = TRUE))
  })

  it("errors when each column matches but the combination does not", {
    data <- make_data()
    # M1 is never an upper-jaw tooth in this fixture, so each column
    # matches alone but no row satisfies both
    spec <- list(reapply = list(TOOTH = "M1", JAW = "upper"))
    result <- filter_spec$check_filter_spec(spec, data)

    expect_length(result$errors, 1)
    expect_true(grepl("combined training filter", result$errors))
  })

  it("tolerates extra levels in the unknown data", {
    data <- make_data()
    spec <- list(reapply = list(TOOTH = "M1"))
    result <- filter_spec$check_filter_spec(spec, data)
    expect_length(result$errors, 0)
  })
})
