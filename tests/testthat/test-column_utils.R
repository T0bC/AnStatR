box::use(
  testthat[describe, expect_equal, it],
)

box::use(
  app/logic/shared/column_utils,
)

# =============================================================================
# strip_normalized_suffix
# =============================================================================

describe("strip_normalized_suffix", {
  it("strips a trailing _normalized suffix", {
    expect_equal(
      column_utils$strip_normalized_suffix("Asfc_normalized"),
      "Asfc"
    )
  })

  it("is a no-op on names without the suffix", {
    expect_equal(
      column_utils$strip_normalized_suffix("Asfc"),
      "Asfc"
    )
  })

  it("leaves non-trailing occurrences of the suffix alone", {
    expect_equal(
      column_utils$strip_normalized_suffix("Asfc_normalized_x"),
      "Asfc_normalized_x"
    )
  })

  it("is vectorized over multiple column names", {
    expect_equal(
      column_utils$strip_normalized_suffix(
        c("Asfc_normalized", "epLsar", "Sq_normalized")
      ),
      c("Asfc", "epLsar", "Sq")
    )
  })
})
