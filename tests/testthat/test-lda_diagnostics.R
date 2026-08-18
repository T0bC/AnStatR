box::use(
  ggplot2,
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/lda/lda,
  app/logic/lda/lda_diagnostics,
  app/logic/lda/ld_plot[create_ld_plot, create_qda_plot],
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
# add_diagnostics_overlay
# =============================================================================

describe("add_diagnostics_overlay", {
  it("adds layers to an existing ggplot", {
    lda_res <- make_lda_result()
    base_res <- create_ld_plot(
      lda_res, dim_x = "LD1", dim_y = "LD2",
      show_diagnostics = FALSE
    )
    base_plot <- base_res$result
    n_layers_before <- length(base_plot$layers)

    groups <- as.factor(lda_res$meta[["species"]])
    p <- lda_diagnostics$add_diagnostics_overlay(
      base_plot, lda_res$scores, groups,
      "LD1", "LD2"
    )
    expect_true(inherits(p, "gg"))
    expect_true(length(p$layers) > n_layers_before)
  })

  it("works via create_ld_plot show_diagnostics flag", {
    lda_res <- make_lda_result()
    plot_res <- create_ld_plot(
      lda_res, dim_x = "LD1", dim_y = "LD2",
      show_diagnostics = TRUE
    )
    expect_true(plot_res$success)
    expect_true(inherits(plot_res$result, "gg"))
    # Should have subtitle when diagnostics enabled
    expect_true(
      !is.null(plot_res$result$labels$subtitle)
    )
  })

  it("does not add subtitle when diagnostics disabled", {
    lda_res <- make_lda_result()
    plot_res <- create_ld_plot(
      lda_res, dim_x = "LD1", dim_y = "LD2",
      show_diagnostics = FALSE
    )
    expect_true(plot_res$success)
    expect_true(
      is.null(plot_res$result$labels$subtitle)
    )
  })
})

# =============================================================================
# Helper: 2-group LDA (produces 1 LD axis)
# =============================================================================

make_lda_result_2group <- function(seed = 42) {
  set.seed(seed)
  data <- data.frame(
    species = rep(c("A", "B"), each = 20),
    site = rep(c("X", "Y"), 20),
    m1 = c(rnorm(20, mean = 0), rnorm(20, mean = 4)),
    m2 = c(rnorm(20, mean = 0), rnorm(20, mean = 2)),
    stringsAsFactors = FALSE
  )
  res <- lda$run_lda(
    data, c("m1", "m2"), "species",
    meta_cols = c("species", "site")
  )
  res$result
}

make_plsda_result <- function(seed = 123) {
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
  res <- lda$run_plsda(
    data, c("m1", "m2", "m3"), "species",
    ncomp = 2, meta_cols = c("species", "site")
  )
  res$result
}

make_plsda_result_2group <- function(seed = 42) {
  set.seed(seed)
  data <- data.frame(
    species = rep(c("A", "B"), each = 20),
    site = rep(c("X", "Y"), 20),
    m1 = c(rnorm(20, mean = 0), rnorm(20, mean = 4)),
    m2 = c(rnorm(20, mean = 0), rnorm(20, mean = 2)),
    stringsAsFactors = FALSE
  )
  res <- lda$run_plsda(
    data, c("m1", "m2"), "species",
    ncomp = 1, meta_cols = c("species", "site")
  )
  res$result
}

# =============================================================================
# add_boundaries_overlay (2D)
# =============================================================================

describe("add_boundaries_overlay", {
  it("adds tile and contour layers to a ggplot", {
    lda_res <- make_lda_result()
    base_res <- create_ld_plot(
      lda_res, dim_x = "LD1", dim_y = "LD2",
      show_boundaries = FALSE
    )
    base_plot <- base_res$result
    n_layers_before <- length(base_plot$layers)

    p <- lda_diagnostics$add_boundaries_overlay(
      base_plot, lda_res, "LD1", "LD2",
      grid_n = 20
    )
    expect_true(inherits(p, "gg"))
    expect_true(length(p$layers) > n_layers_before)
  })

  it("works via create_ld_plot show_boundaries flag", {
    lda_res <- make_lda_result()
    plot_res <- create_ld_plot(
      lda_res, dim_x = "LD1", dim_y = "LD2",
      show_boundaries = TRUE
    )
    expect_true(plot_res$success)
    expect_true(inherits(plot_res$result, "gg"))
    expect_true(grepl(
      "decision regions",
      plot_res$result$labels$subtitle
    ))
  })

  it("combines subtitles when both overlays active", {
    lda_res <- make_lda_result()
    plot_res <- create_ld_plot(
      lda_res, dim_x = "LD1", dim_y = "LD2",
      show_diagnostics = TRUE,
      show_boundaries = TRUE
    )
    expect_true(plot_res$success)
    sub <- plot_res$result$labels$subtitle
    expect_true(grepl("decision regions", sub))
    expect_true(grepl("per-group VC", sub))
  })

  it("adds tile and contour layers for PLS-DA (k-NN grid)", {
    plsda_res <- make_plsda_result()
    base_res <- create_ld_plot(
      plsda_res, dim_x = "Comp1", dim_y = "Comp2",
      show_boundaries = FALSE
    )
    base_plot <- base_res$result
    n_layers_before <- length(base_plot$layers)

    p <- lda_diagnostics$add_boundaries_overlay(
      base_plot, plsda_res, "Comp1", "Comp2",
      grid_n = 20
    )
    expect_true(inherits(p, "gg"))
    expect_true(length(p$layers) > n_layers_before)
  })

  it("works via create_ld_plot show_boundaries flag for sPLS-DA", {
    set.seed(123)
    data <- data.frame(
      species = rep(c("A", "B", "C"), each = 15),
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
    res <- lda$run_plsda(
      data, c("m1", "m2", "m3"), "species",
      ncomp = 2, sparse = TRUE, keep_x = c(2, 2)
    )
    splsda_res <- res$result

    plot_res <- create_ld_plot(
      splsda_res, dim_x = "Comp1", dim_y = "Comp2",
      show_boundaries = TRUE
    )
    expect_true(plot_res$success)
    expect_true(inherits(plot_res$result, "gg"))
    expect_true(grepl(
      "decision regions",
      plot_res$result$labels$subtitle
    ))
  })
})

# =============================================================================
# compute_1d_boundary
# =============================================================================

describe("compute_1d_boundary", {
  it("returns a finite numeric scalar", {
    lda_res <- make_lda_result_2group()
    boundary <- lda_diagnostics$compute_1d_boundary(
      lda_res
    )
    expect_true(is.numeric(boundary))
    expect_equal(length(boundary), 1)
    expect_true(is.finite(boundary))
  })

  it("boundary falls between group means on LD1", {
    lda_res <- make_lda_result_2group()
    boundary <- lda_diagnostics$compute_1d_boundary(
      lda_res
    )
    ld1 <- lda_res$scores$LD1
    groups <- lda_res$meta[["species"]]
    m_a <- mean(ld1[groups == "A"])
    m_b <- mean(ld1[groups == "B"])
    lo <- min(m_a, m_b)
    hi <- max(m_a, m_b)
    expect_true(boundary >= lo && boundary <= hi)
  })

  it("1D plot shows boundary via show_boundaries", {
    lda_res <- make_lda_result_2group()
    plot_res <- create_ld_plot(
      lda_res,
      show_boundaries = TRUE
    )
    expect_true(plot_res$success)
    expect_true(grepl(
      "decision boundary",
      plot_res$result$labels$subtitle
    ))
  })

  it("returns a finite numeric scalar for a 2-group PLS-DA (k-NN)", {
    plsda_res <- make_plsda_result_2group()
    boundary <- lda_diagnostics$compute_1d_boundary(
      plsda_res
    )
    expect_true(is.numeric(boundary))
    expect_equal(length(boundary), 1)
    expect_true(is.finite(boundary))
  })

  it("1D PLS-DA plot shows boundary via show_boundaries", {
    plsda_res <- make_plsda_result_2group()
    plot_res <- create_ld_plot(
      plsda_res,
      show_boundaries = TRUE
    )
    expect_true(plot_res$success)
    expect_true(grepl(
      "decision boundary",
      plot_res$result$labels$subtitle
    ))
  })
})

# =============================================================================
# Helper: QDA result (3 groups, has companion LDA)
# =============================================================================

make_qda_result <- function(seed = 99) {
  set.seed(seed)
  data <- data.frame(
    species = rep(c("A", "B", "C"), each = 20),
    site = rep(c("X", "Y"), 30),
    m1 = c(
      rnorm(20, mean = 0), rnorm(20, mean = 3),
      rnorm(20, mean = 6)
    ),
    m2 = c(
      rnorm(20, mean = 0), rnorm(20, mean = 2),
      rnorm(20, mean = 4)
    ),
    m3 = rnorm(60),
    stringsAsFactors = FALSE
  )
  res <- lda$run_qda(
    data, c("m1", "m2", "m3"), "species",
    meta_cols = c("species", "site")
  )
  res$result
}

# =============================================================================
# create_qda_plot (LD space)
# =============================================================================

describe("create_qda_plot in LD space", {
  it("produces a ggplot when using LD axes", {
    qda_res <- make_qda_result()
    plot_res <- create_qda_plot(
      qda_res, dim_x = "LD1", dim_y = "LD2",
      show_boundaries = FALSE
    )
    expect_true(plot_res$success)
    expect_true(inherits(plot_res$result, "gg"))
  })

  it("title indicates LDA projection", {
    qda_res <- make_qda_result()
    plot_res <- create_qda_plot(
      qda_res, dim_x = "LD1", dim_y = "LD2"
    )
    expect_true(grepl(
      "LDA projection",
      plot_res$result$labels$title
    ))
  })

  it("adds boundary layers when enabled", {
    qda_res <- make_qda_result()
    plot_no <- create_qda_plot(
      qda_res, dim_x = "LD1", dim_y = "LD2",
      show_boundaries = FALSE
    )
    plot_yes <- create_qda_plot(
      qda_res, dim_x = "LD1", dim_y = "LD2",
      show_boundaries = TRUE
    )
    expect_true(
      length(plot_yes$result$layers) >
        length(plot_no$result$layers)
    )
    expect_true(grepl(
      "QDA decision regions",
      plot_yes$result$labels$subtitle
    ))
  })
})

# =============================================================================
# create_qda_plot (original variable space)
# =============================================================================

describe("create_qda_plot in original space", {
  it("produces a ggplot when using original vars", {
    qda_res <- make_qda_result()
    plot_res <- create_qda_plot(
      qda_res, dim_x = "m1", dim_y = "m2",
      show_boundaries = FALSE
    )
    expect_true(plot_res$success)
    expect_true(inherits(plot_res$result, "gg"))
  })

  it("title indicates original variables", {
    qda_res <- make_qda_result()
    plot_res <- create_qda_plot(
      qda_res, dim_x = "m1", dim_y = "m2"
    )
    expect_true(grepl(
      "original variables",
      plot_res$result$labels$title
    ))
  })

  it("errors when mixing LD and original axes", {
    qda_res <- make_qda_result()
    plot_res <- create_qda_plot(
      qda_res, dim_x = "LD1", dim_y = "m2"
    )
    expect_true(!plot_res$success)
  })
})

# =============================================================================
# add_qda_boundaries_overlay
# =============================================================================

describe("add_qda_boundaries_overlay", {
  it("adds layers to an existing ggplot (LD space)", {
    qda_res <- make_qda_result()
    base_res <- create_qda_plot(
      qda_res, dim_x = "LD1", dim_y = "LD2",
      show_boundaries = FALSE
    )
    base_plot <- base_res$result
    n_before <- length(base_plot$layers)

    plot_data <- data.frame(
      x = qda_res$lda_scores$LD1,
      y = qda_res$lda_scores$LD2
    )
    p <- lda_diagnostics$add_qda_boundaries_overlay(
      base_plot, qda_res, "LD1", "LD2",
      plot_data, axis_type = "ld",
      grid_n = 20
    )
    expect_true(inherits(p, "gg"))
    expect_true(length(p$layers) > n_before)
  })
})
