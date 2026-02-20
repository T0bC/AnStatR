box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/lda/lda,
  app/logic/lda/lda_diagnostics,
)

# =============================================================================
# Helper: create test data and fit LDA
# =============================================================================

make_lda_result <- function(seed = 123) {
  set.seed(seed)
  data <- data.frame(
    species = rep(c("A", "B", "C"), each = 15),
    site = rep(c("X", "Y", "Z"), 15),
    m1 = c(
      rnorm(15, mean = 0), rnorm(15, mean = 3),
      rnorm(15, mean = 6)
    ),
    m2 = c(
      rnorm(15, mean = 0), rnorm(15, mean = 2),
      rnorm(15, mean = 4)
    ),
    m3 = rnorm(45),
    stringsAsFactors = FALSE
  )
  res <- lda$run_lda(
    data, c("m1", "m2", "m3"), "species",
    meta_cols = c("species", "site")
  )
  res$result
}

# =============================================================================
# compute_pooled_vc
# =============================================================================

describe("compute_pooled_vc", {
  it("returns a matrix with correct dimensions", {
    scores <- data.frame(x = rnorm(30), y = rnorm(30))
    groups <- factor(rep(c("A", "B", "C"), each = 10))
    vc <- lda_diagnostics$compute_pooled_vc(
      scores, groups
    )
    expect_equal(nrow(vc), 2)
    expect_equal(ncol(vc), 2)
  })

  it("returns a symmetric matrix", {
    scores <- data.frame(x = rnorm(30), y = rnorm(30))
    groups <- factor(rep(c("A", "B"), each = 15))
    vc <- lda_diagnostics$compute_pooled_vc(
      scores, groups
    )
    expect_equal(vc[1, 2], vc[2, 1])
  })

  it("has correct column and row names", {
    scores <- data.frame(
      LD1 = rnorm(20), LD2 = rnorm(20)
    )
    groups <- factor(rep(c("A", "B"), each = 10))
    vc <- lda_diagnostics$compute_pooled_vc(
      scores, groups
    )
    expect_equal(colnames(vc), c("LD1", "LD2"))
    expect_equal(rownames(vc), c("LD1", "LD2"))
  })

  it("skips groups with fewer than 2 observations", {
    scores <- data.frame(x = rnorm(21), y = rnorm(21))
    groups <- factor(c("A", rep("B", 10), rep("C", 10)))
    vc <- lda_diagnostics$compute_pooled_vc(
      scores, groups
    )
    expect_equal(nrow(vc), 2)
    expect_true(all(is.finite(vc)))
  })
})

# =============================================================================
# generate_ellipse_points
# =============================================================================

describe("generate_ellipse_points", {
  it("returns a data frame with x and y columns", {
    vc <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
    pts <- lda_diagnostics$generate_ellipse_points(
      vc, center = c(0, 0)
    )
    expect_true(is.data.frame(pts))
    expect_true("x" %in% names(pts))
    expect_true("y" %in% names(pts))
  })

  it("returns the requested number of points", {
    vc <- matrix(c(1, 0, 0, 1), nrow = 2)
    pts <- lda_diagnostics$generate_ellipse_points(
      vc, center = c(0, 0), n_points = 50
    )
    expect_equal(nrow(pts), 50)
  })

  it("centers the ellipse at the given point", {
    vc <- matrix(c(1, 0, 0, 1), nrow = 2)
    center <- c(5, 10)
    pts <- lda_diagnostics$generate_ellipse_points(
      vc, center = center, n_points = 200
    )
    expect_true(
      abs(mean(pts$x) - center[1]) < 0.1
    )
    expect_true(
      abs(mean(pts$y) - center[2]) < 0.1
    )
  })

  it("scales with n_std", {
    vc <- matrix(c(1, 0, 0, 1), nrow = 2)
    pts1 <- lda_diagnostics$generate_ellipse_points(
      vc, center = c(0, 0), n_std = 1
    )
    pts2 <- lda_diagnostics$generate_ellipse_points(
      vc, center = c(0, 0), n_std = 2
    )
    range1 <- max(pts1$x) - min(pts1$x)
    range2 <- max(pts2$x) - min(pts2$x)
    expect_true(range2 > range1)
  })
})

# =============================================================================
# create_class_ellipses_plot
# =============================================================================

describe("create_class_ellipses_plot", {
  it("returns success with a ggplot for valid LDA result", {
    lda_res <- make_lda_result()
    plot_res <- lda_diagnostics$create_class_ellipses_plot(
      lda_res, dim_x = "LD1", dim_y = "LD2"
    )
    expect_true(plot_res$success)
    expect_true(inherits(plot_res$result, "gg"))
  })

  it("fails for invalid dimension", {
    lda_res <- make_lda_result()
    plot_res <- lda_diagnostics$create_class_ellipses_plot(
      lda_res, dim_x = "LD1", dim_y = "LD99"
    )
    expect_true(!plot_res$success)
  })
})

# =============================================================================
# create_pooled_vc_plot
# =============================================================================

describe("create_pooled_vc_plot", {
  it("returns success with a ggplot for valid LDA result", {
    lda_res <- make_lda_result()
    plot_res <- lda_diagnostics$create_pooled_vc_plot(
      lda_res, dim_x = "LD1", dim_y = "LD2"
    )
    expect_true(plot_res$success)
    expect_true(inherits(plot_res$result, "gg"))
  })

  it("fails for invalid dimension", {
    lda_res <- make_lda_result()
    plot_res <- lda_diagnostics$create_pooled_vc_plot(
      lda_res, dim_x = "LD1", dim_y = "LD99"
    )
    expect_true(!plot_res$success)
  })
})
