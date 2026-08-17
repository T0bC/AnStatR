box::use(
  testthat[describe, expect_equal, expect_true, expect_false, it],
)

box::use(
  app/logic/shared/error_handling,
  app/logic/statistics/parameter_ranking,
)

# =============================================================================
# Fixtures
# =============================================================================

# Two measures, two comparisons, parametric (Tukey/Cohen.d) schema.
make_two_measure_fixture <- function() {
  list(
    Asfc = data.frame(
      Interaction = c("A vs. B", "A vs. C"),
      Tukey.p.value = c(0.001, 0.20),
      Tukey.p.adjusted = c(0.002, 0.40),
      Cohen.d = c(1.5, 0.2),
      stringsAsFactors = FALSE
    ),
    epLsar = data.frame(
      Interaction = c("A vs. B", "A vs. C"),
      Tukey.p.value = c(0.30, 0.005),
      Tukey.p.adjusted = c(0.60, 0.01),
      Cohen.d = c(0.1, 1.1),
      stringsAsFactors = FALSE
    )
  )
}

# =============================================================================
# rank_parameters_by_comparison
# =============================================================================

describe("rank_parameters_by_comparison", {
  it("ranks measures per comparison by ascending raw p-value", {
    result <- parameter_ranking$rank_parameters_by_comparison(
      make_two_measure_fixture(), top_n = 1, p_column = "raw"
    )
    ab <- result$ranking[result$ranking$Interaction == "A vs. B", ]
    expect_equal(ab$parameter[ab$rank == 1], "Asfc")

    ac <- result$ranking[result$ranking$Interaction == "A vs. C", ]
    expect_equal(ac$parameter[ac$rank == 1], "epLsar")
  })

  it("marks exactly top_n per comparison when there are no ties", {
    result <- parameter_ranking$rank_parameters_by_comparison(
      make_two_measure_fixture(), top_n = 1, p_column = "raw"
    )
    for (comp in unique(result$ranking$Interaction)) {
      part <- result$ranking[result$ranking$Interaction == comp, ]
      expect_equal(sum(part$is_top), 1L)
    }
  })

  it("breaks ties on larger |effect - effect_null|", {
    posthoc <- list(
      X = data.frame(
        Interaction = "A vs. B",
        Tukey.p.value = 0.01,
        Tukey.p.adjusted = 0.02,
        Cohen.d = 0.3,
        stringsAsFactors = FALSE
      ),
      Y = data.frame(
        Interaction = "A vs. B",
        Tukey.p.value = 0.01,
        Tukey.p.adjusted = 0.02,
        Cohen.d = 0.9,
        stringsAsFactors = FALSE
      )
    )
    result <- parameter_ranking$rank_parameters_by_comparison(
      posthoc, top_n = 1, p_column = "raw"
    )
    top <- result$ranking[result$ranking$is_top, ]
    expect_equal(top$parameter, "Y")
  })

  it("strips the _normalized suffix in recommended and parameter columns", {
    posthoc <- list(
      Asfc_normalized = data.frame(
        Interaction = "A vs. B",
        Tukey.p.value = 0.001,
        Tukey.p.adjusted = 0.002,
        Cohen.d = 1.2,
        stringsAsFactors = FALSE
      )
    )
    result <- parameter_ranking$rank_parameters_by_comparison(
      posthoc, top_n = 3, p_column = "raw"
    )
    expect_equal(result$recommended, "Asfc")
    expect_equal(result$ranking$parameter, "Asfc")
    expect_equal(result$ranking$measure, "Asfc_normalized")
  })

  it("deduplicates the recommended union across comparisons", {
    result <- parameter_ranking$rank_parameters_by_comparison(
      make_two_measure_fixture(), top_n = 2, p_column = "raw"
    )
    expect_equal(length(result$recommended), length(unique(result$recommended)))
    expect_true(all(c("Asfc", "epLsar") %in% result$recommended))
  })

  it("skips app_error measures and records the reason", {
    posthoc <- make_two_measure_fixture()
    posthoc$Broken <- error_handling$simple_error("boom")
    result <- parameter_ranking$rank_parameters_by_comparison(
      posthoc, top_n = 1, p_column = "raw"
    )
    expect_true("Broken" %in% result$skipped$measure)
    expect_equal(
      result$skipped$reason[result$skipped$measure == "Broken"], "error"
    )
    expect_false("Broken" %in% result$ranking$measure)
  })

  it("skips empty-data-frame measures and records the reason", {
    posthoc <- make_two_measure_fixture()
    posthoc$Empty <- data.frame(
      Interaction = character(0), Tukey.p.value = numeric(0)
    )
    result <- parameter_ranking$rank_parameters_by_comparison(
      posthoc, top_n = 1, p_column = "raw"
    )
    expect_true("Empty" %in% result$skipped$measure)
    expect_equal(
      result$skipped$reason[result$skipped$measure == "Empty"], "empty"
    )
  })

  it("returns an app_error when every measure is skipped", {
    posthoc <- list(
      Broken1 = error_handling$simple_error("boom"),
      Broken2 = NULL
    )
    result <- parameter_ranking$rank_parameters_by_comparison(
      posthoc, top_n = 3, p_column = "raw"
    )
    expect_true(error_handling$is_app_error(result))
  })

  it("drops NA p-values instead of ranking them as smallest", {
    posthoc <- list(
      Asfc = data.frame(
        Interaction = "A vs. B",
        Tukey.p.value = NA_real_,
        Tukey.p.adjusted = NA_real_,
        Cohen.d = 0.5,
        stringsAsFactors = FALSE
      ),
      epLsar = data.frame(
        Interaction = "A vs. B",
        Tukey.p.value = 0.02,
        Tukey.p.adjusted = 0.04,
        Cohen.d = 0.5,
        stringsAsFactors = FALSE
      )
    )
    result <- parameter_ranking$rank_parameters_by_comparison(
      posthoc, top_n = 1, p_column = "raw"
    )
    expect_equal(nrow(result$ranking), 1L)
    expect_equal(result$ranking$parameter, "epLsar")
    expect_true("Asfc" %in% result$skipped$measure)
  })

  it("uses adjusted p-values when p_column = 'adjusted'", {
    posthoc <- list(
      Asfc = data.frame(
        Interaction = "A vs. B",
        Tukey.p.value = 0.03,
        Tukey.p.adjusted = 0.10,
        Cohen.d = 0.5,
        stringsAsFactors = FALSE
      ),
      epLsar = data.frame(
        Interaction = "A vs. B",
        Tukey.p.value = 0.02,
        Tukey.p.adjusted = 0.05,
        Cohen.d = 0.5,
        stringsAsFactors = FALSE
      )
    )
    result <- parameter_ranking$rank_parameters_by_comparison(
      posthoc, top_n = 1, p_column = "adjusted"
    )
    top <- result$ranking[result$ranking$is_top, ]
    expect_equal(top$parameter, "epLsar")
    expect_equal(result$p_column_used, "adjusted")
  })
})

# =============================================================================
# build_ranking_matrix
# =============================================================================

describe("build_ranking_matrix", {
  it("produces one row per parameter and one column per comparison", {
    result <- parameter_ranking$rank_parameters_by_comparison(
      make_two_measure_fixture(), top_n = 1, p_column = "raw"
    )
    mat <- parameter_ranking$build_ranking_matrix(result)
    expect_equal(nrow(mat), 2L)
    expect_true(all(c("A vs. B", "A vs. C") %in% names(mat)))
    expect_true("n_top" %in% names(mat))
  })

  it("agrees with ranking$is_top via the is_top attribute", {
    result <- parameter_ranking$rank_parameters_by_comparison(
      make_two_measure_fixture(), top_n = 1, p_column = "raw"
    )
    mat <- parameter_ranking$build_ranking_matrix(result)
    is_top_mat <- attr(mat, "is_top")

    for (i in seq_len(nrow(result$ranking))) {
      row <- result$ranking[i, ]
      expect_equal(
        is_top_mat[row$parameter, row$Interaction],
        row$is_top
      )
    }
  })
})
