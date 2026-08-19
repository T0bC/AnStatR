box::use(
  testthat[describe, expect_equal, expect_true, it],
)

box::use(
  app/logic/lda/lda[run_plsda],
  app/logic/lda/ld_plot[create_ld_plot],
)

# =============================================================================
# Decision-boundary rule selection for PLS-DA/sPLS-DA.
# mixOmics offers max.dist / centroids.dist / mahalanobis.dist; each must
# produce a valid background, and they must not all collapse to the same one.
# =============================================================================

make_data <- function(seed = 7, n = 60) {
  set.seed(seed)
  d <- data.frame(
    CLASS = rep(c("a", "b", "c"), each = n / 3),
    stringsAsFactors = FALSE
  )
  x <- matrix(rnorm(n * 12), n, 12)
  x[d$CLASS == "b", 1:4] <- x[d$CLASS == "b", 1:4] + 2.5
  x[d$CLASS == "c", 1:4] <- x[d$CLASS == "c", 1:4] + 5
  # elongate one group so mahalanobis differs from plain centroids
  x[d$CLASS == "a", 1] <- x[d$CLASS == "a", 1] * 4
  cols <- paste0("v", 1:12)
  d[cols] <- x
  d
}

measure_cols <- paste0("v", 1:12)

fit <- function() {
  res <- run_plsda(
    make_data(), measure_cols, "CLASS",
    ncomp = 2, sparse = FALSE, meta_cols = "CLASS"
  )
  expect_true(res$success)
  res$result
}

boundary_grid <- function(result, dist,
                          dim_x = "Comp1", dim_y = "Comp2") {
  plot_res <- create_ld_plot(
    result, dim_x, dim_y,
    show_boundaries = TRUE, boundary_dist = dist
  )
  expect_true(plot_res$success)
  for (layer in plot_res$result$layers) {
    dat <- tryCatch(layer$data, error = function(e) NULL)
    if (is.data.frame(dat) && "class" %in% names(dat) &&
          nrow(dat) > 1000) {
      return(dat)
    }
  }
  NULL
}


describe("decision boundary distance rules", {
  it("builds a background for every supported rule", {
    result <- fit()
    for (dist in c("max.dist", "centroids.dist", "mahalanobis.dist")) {
      grid <- boundary_grid(result, dist)
      expect_true(!is.null(grid))
      expect_true(nrow(grid) > 1000)
      expect_true(all(grid$class %in% c("a", "b", "c")))
    }
  })

  it("produces genuinely different regions per rule", {
    result <- fit()
    g_max <- boundary_grid(result, "max.dist")
    g_cen <- boundary_grid(result, "centroids.dist")
    g_mah <- boundary_grid(result, "mahalanobis.dist")

    expect_equal(nrow(g_max), nrow(g_cen))
    expect_equal(nrow(g_max), nrow(g_mah))
    # If these were identical the selector would be pointless.
    expect_true(sum(g_max$class != g_mah$class) > 0)
    expect_true(sum(g_max$class != g_cen$class) > 0)
  })

  it("defaults to max.dist when no rule is given", {
    result <- fit()
    default_grid <- boundary_grid(result, "max.dist")
    plot_res <- create_ld_plot(
      result, "Comp1", "Comp2", show_boundaries = TRUE
    )
    expect_true(plot_res$success)
    explicit <- NULL
    for (layer in plot_res$result$layers) {
      dat <- tryCatch(layer$data, error = function(e) NULL)
      if (is.data.frame(dat) && "class" %in% names(dat) &&
            nrow(dat) > 1000) {
        explicit <- dat
        break
      }
    }
    expect_true(!is.null(explicit))
    expect_equal(explicit$class, default_grid$class)
  })

  it("falls back gracefully for an unknown rule", {
    result <- fit()
    plot_res <- create_ld_plot(
      result, "Comp1", "Comp2",
      show_boundaries = TRUE, boundary_dist = "not.a.rule"
    )
    # Unknown rules fall back to max.dist rather than erroring.
    expect_true(plot_res$success)
    expect_equal(
      boundary_grid(result, "not.a.rule")$class,
      boundary_grid(result, "max.dist")$class
    )
  })
})

describe("boundary rules on non-leading component pairs", {
  # Regression test: the rule selector used to be wired only for the
  # Comp1/Comp2 branch. Any other pair fell through to a k-NN
  # background that ignored the rule, so switching it did nothing.
  fit4 <- function() {
    res <- run_plsda(
      make_data(n = 90), measure_cols, "CLASS",
      ncomp = 4, sparse = FALSE, meta_cols = "CLASS"
    )
    expect_true(res$success)
    res$result
  }

  it("changes the background when the rule changes on Comp3/Comp4", {
    result <- fit4()
    g_cen <- boundary_grid(result, "centroids.dist", "Comp3", "Comp4")
    g_mah <- boundary_grid(result, "mahalanobis.dist", "Comp3", "Comp4")
    expect_true(!is.null(g_cen))
    expect_true(sum(g_cen$class != g_mah$class) > 0)
  })

  it("handles reversed component order", {
    result <- fit4()
    g_cen <- boundary_grid(result, "centroids.dist", "Comp4", "Comp2")
    g_mah <- boundary_grid(result, "mahalanobis.dist", "Comp4", "Comp2")
    expect_true(!is.null(g_cen))
    expect_true(sum(g_cen$class != g_mah$class) > 0)
  })
})
