box::use(
  stats[setNames],
  testthat[describe, expect_equal, expect_false, expect_true, it],
)

box::use(
  app/logic/median/design_balance,
)

# Two descriptive columns whose crossing is deliberately incomplete:
# SPECIES "C" only ever appears with TOOTH "M1".
sample_data <- function() {
  data.frame(
    SPECIES = c("A", "A", "A", "B", "B", "B", "B", "C"),
    TOOTH = c("M1", "M1", "M2", "M1", "M2", "M2", "M2", "M1"),
    Sq = c(0.1, NA, 0.3, 0.4, NA, NA, 0.7, 0.8),
    Sa = c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8),
    stringsAsFactors = FALSE
  )
}

describe("compute_group_balance", {
  it("counts observations for a single grouping column", {
    result <- design_balance$compute_group_balance(
      sample_data(), "SPECIES"
    )
    counts <- setNames(
      result$data$n, as.character(result$data$group_label)
    )
    expect_equal(unname(counts[["A"]]), 3)
    expect_equal(unname(counts[["B"]]), 4)
    expect_equal(unname(counts[["C"]]), 1)
  })

  it("counts observations for two grouping columns", {
    result <- design_balance$compute_group_balance(
      sample_data(), c("SPECIES", "TOOTH")
    )
    expect_equal(result$n_combinations, 5)
    expect_equal(sum(result$data$n), 8)
  })

  it("reports combinations that never occur", {
    result <- design_balance$compute_group_balance(
      sample_data(), c("SPECIES", "TOOTH")
    )
    # 3 species x 2 teeth = 6 possible, 5 observed.
    expect_equal(result$n_possible, 6)
    expect_equal(result$n_empty, 1)
    expect_equal(result$pct_coverage, round(100 * 5 / 6, 1))
  })

  it("handles three or more grouping columns", {
    df <- sample_data()
    df$SEX <- c("F", "F", "M", "M", "F", "F", "M", "M")
    result <- design_balance$compute_group_balance(
      df, c("SPECIES", "TOOTH", "SEX")
    )
    expect_equal(sum(result$data$n), 8)
    expect_equal(result$n_possible, 12)
    expect_true(result$n_combinations <= 8)
  })

  it("flags groups below the thin threshold", {
    result <- design_balance$compute_group_balance(
      sample_data(), "SPECIES", thin_threshold = 3
    )
    thin <- as.character(result$data$group_label[result$data$is_thin])
    expect_equal(thin, "C")
    expect_equal(result$n_thin, 1)
  })

  it("treats the threshold as exclusive", {
    # SPECIES "A" has exactly 3 observations, so n < 3 is FALSE.
    result <- design_balance$compute_group_balance(
      sample_data(), "SPECIES", thin_threshold = 3
    )
    is_thin <- result$data$is_thin[
      as.character(result$data$group_label) == "A"
    ]
    expect_false(is_thin)
  })

  it("counts NA as its own level rather than dropping rows", {
    df <- data.frame(
      SEX = c("F", NA, NA, "M"),
      Sq = c(1, 2, 3, 4),
      stringsAsFactors = FALSE
    )
    result <- design_balance$compute_group_balance(df, "SEX")
    expect_equal(sum(result$data$n), 4)
    expect_true("NA" %in% as.character(result$data$group_label))
  })

  it("returns an empty result when no grouping columns are given", {
    result <- design_balance$compute_group_balance(
      sample_data(), character(0)
    )
    expect_equal(result$n_combinations, 0)
    expect_equal(nrow(result$data), 0)
  })

  it("ignores grouping columns that are not in the data", {
    result <- design_balance$compute_group_balance(
      sample_data(), "NOT_A_COLUMN"
    )
    expect_equal(result$n_combinations, 0)
  })

  it("handles a zero-row data frame", {
    result <- design_balance$compute_group_balance(
      data.frame(SPECIES = character(0)), "SPECIES"
    )
    expect_equal(result$n_combinations, 0)
  })
})

describe("compute_group_missingness", {
  it("covers only measurement columns that have gaps", {
    result <- design_balance$compute_group_missingness(
      sample_data(), "SPECIES"
    )
    expect_equal(result$n_cols, 1)
    expect_equal(as.character(unique(result$data$column)), "Sq")
  })

  it("computes the missing percentage within each group", {
    result <- design_balance$compute_group_missingness(
      sample_data(), "SPECIES"
    )
    pct <- setNames(
      result$data$pct_missing, as.character(result$data$group_label)
    )
    # A: 1 of 3 missing, B: 2 of 4 missing, C: 0 of 1 missing.
    expect_equal(unname(pct[["A"]]), round(100 / 3, 1))
    expect_equal(unname(pct[["B"]]), 50)
    expect_equal(unname(pct[["C"]]), 0)
  })

  it("produces one row per group and affected column", {
    df <- sample_data()
    df$Sk <- c(NA, 2, 3, 4, 5, 6, 7, 8)
    result <- design_balance$compute_group_missingness(df, "SPECIES")
    expect_equal(result$n_groups, 3)
    expect_equal(result$n_cols, 2)
    expect_equal(nrow(result$data), 6)
  })

  it("returns nothing when no measurement column has gaps", {
    df <- data.frame(
      SPECIES = c("A", "B"),
      Sq = c(1.1, 2.2),
      stringsAsFactors = FALSE
    )
    result <- design_balance$compute_group_missingness(df, "SPECIES")
    expect_equal(nrow(result$data), 0)
    expect_equal(result$n_cols, 0)
  })

  it("accepts an explicit measurement column list", {
    result <- design_balance$compute_group_missingness(
      sample_data(), "SPECIES",
      measurement_cols = c("Sq", "Sa")
    )
    expect_equal(result$n_cols, 1)
  })

  it("returns an empty result when no grouping columns are given", {
    result <- design_balance$compute_group_missingness(
      sample_data(), character(0)
    )
    expect_equal(nrow(result$data), 0)
  })

  it("handles a zero-row data frame", {
    result <- design_balance$compute_group_missingness(
      data.frame(SPECIES = character(0), Sq = numeric(0)), "SPECIES"
    )
    expect_equal(nrow(result$data), 0)
  })
})
