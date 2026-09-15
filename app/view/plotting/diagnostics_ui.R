box::use(
  bsicons,
  shiny,
)

box::use(
  app/logic/plotting/assumption_checks,
)

#' Build the diagnostics UI for one measurement column
#'
#' Creates a pivoted HTML table (groups as columns) with Shapiro-Wilk
#' results, Levene's test footer, and a recommendation banner.
#'
#' @param diag List from the diagnostics reactive
#' @return shiny tagList
#' @export
build_diagnostics_ui <- function(diag) {
  # =====================================================================
  # 1. Build the BANNER (always visible compact summary)
  # =====================================================================
  has_post <- isTRUE(diag$has_normalized) &&
    !is.null(diag$recommendation_post)

  banner_ui <- if (has_post) {
    build_comparison_banner(diag)
  } else {
    build_raw_banner(diag)
  }

  # Compact test summary lines (residuals + Levene's)
  # Use post-transformation results when available
  resid <- if (has_post) diag$residuals_post else diag$residuals_raw
  levene <- if (has_post) diag$levene_post else diag$levene_raw

  summary_lines <- shiny$tags$div(
    class = "mt-1",
    build_residuals_line(resid, label_prefix = "Residuals"),
    build_levene_line(levene)
  )

  # =====================================================================
  # 2. Build the DETAIL tables (hidden in collapsible)
  # =====================================================================
  detail_elements <- list()

  # Raw per-group table
  detail_elements[[length(detail_elements) + 1]] <-
    build_shapiro_table(
      diag$normality_raw,
      "Shapiro-Wilk Normality Test (per group)"
    )
  detail_elements[[length(detail_elements) + 1]] <-
    build_residuals_line(
      diag$residuals_raw,
      label_prefix = "Residuals"
    )
  detail_elements[[length(detail_elements) + 1]] <-
    build_levene_line(diag$levene_raw)

  # Post-transformation tables (if applicable)
  if (isTRUE(diag$has_normalized) &&
    !is.null(diag$normality_post)) {
    label <- if (!is.null(diag$transform_label)) {
      paste0("After Transformation (", diag$transform_label, ")")
    } else {
      "After Transformation"
    }
    detail_elements[[length(detail_elements) + 1]] <-
      shiny$tags$hr(class = "my-1")
    detail_elements[[length(detail_elements) + 1]] <-
      build_shapiro_table(diag$normality_post, label)
    detail_elements[[length(detail_elements) + 1]] <-
      build_residuals_line(
        diag$residuals_post,
        label_prefix = "Residuals"
      )
    detail_elements[[length(detail_elements) + 1]] <-
      build_levene_line(diag$levene_post)
  }

  # =====================================================================
  # 3. Assemble: banner + summary lines + collapsible details
  # =====================================================================
  shiny$tagList(
    banner_ui,
    summary_lines,
    shiny$tags$details(
      class = "mt-2 small",
      shiny$tags$summary(
        class = "text-muted",
        style = "cursor: pointer;",
        "Show detailed test results"
      ),
      shiny$tags$div(
        class = "mt-1 ps-1",
        do.call(shiny$tagList, detail_elements)
      )
    )
  )
}


#' Build comparison banner (when normalization was applied)
#'
#' Uses residual normality as the primary ANOVA criterion.
#' Per-group results are secondary context.
#'
#' @param diag Diagnostics list
#' @return shiny tag
build_comparison_banner <- function(diag) {
  n_bad_before <- diag$recommendation$n_non_normal
  n_bad_after <- diag$recommendation_post$n_non_normal
  n_groups <- diag$recommendation_post$n_groups

  resid_before <- diag$residuals_raw
  resid_after <- diag$residuals_post

  resid_normal_before <- !is.na(resid_before$normal) &&
    resid_before$normal == "yes"
  resid_normal_after <- !is.null(resid_after) &&
    !is.na(resid_after$normal) && resid_after$normal == "yes"

  # Primary criterion: residual normality
  if (resid_normal_after) {
    # Residuals are normal after transformation → ANOVA OK
    if (n_bad_after > 0) {
      text <- paste0(
        "Model residuals are normally distributed after ",
        "transformation — classical ANOVA is valid. ",
        "Per-group: ", n_bad_after, "/", n_groups,
        " groups non-normal (see details)."
      )
    } else {
      text <- paste0(
        "Transformation achieved normality ",
        "(residuals + all ", n_groups, " groups). ",
        "Classical ANOVA is valid."
      )
    }
    css <- "alert-success"
    icon <- "check-circle-fill"
  } else if (!resid_normal_after && resid_normal_before) {
    # Residuals were normal before but not after → worsened
    text <- paste0(
      "Transformation worsened residual normality. ",
      "Model residuals were normal before transformation ",
      "but are non-normal after. ",
      "Consider keeping raw data for this variable."
    )
    css <- "alert-danger"
    icon <- "x-circle-fill"
  } else if (!resid_normal_after && !resid_normal_before) {
    # Residuals non-normal before and after
    grp_improved <- n_bad_after < n_bad_before
    if (grp_improved) {
      text <- paste0(
        "Transformation improved per-group normality (",
        n_bad_before, " → ", n_bad_after,
        " non-normal groups) but residuals remain ",
        "non-normal. Consider robust/non-parametric tests."
      )
      css <- "alert-warning"
      icon <- "exclamation-triangle-fill"
    } else {
      text <- paste0(
        "Residuals remain non-normal after transformation. ",
        "Per-group: ", n_bad_after, "/", n_groups,
        " non-normal. ",
        "Consider robust or non-parametric tests."
      )
      css <- "alert-warning"
      icon <- "exclamation-triangle-fill"
    }
  } else {
    # Fallback
    text <- paste0(
      "Per-group: ", n_bad_after, "/", n_groups,
      " non-normal after transformation."
    )
    css <- "alert-info"
    icon <- "info-circle-fill"
  }

  shiny$tags$div(
    class = paste("alert", css, "py-1 px-2 small mb-0"),
    shiny$tags$div(
      bsicons$bs_icon(icon, class = "me-1"),
      text
    )
  )
}


#' Build raw recommendation banner (no normalization)
#'
#' Uses residual normality as the primary ANOVA criterion.
#' Per-group results provide supplementary context.
#'
#' @param diag Diagnostics list
#' @return shiny tag
build_raw_banner <- function(diag) {
  rec <- diag$recommendation
  resid <- diag$residuals_raw
  n_groups <- rec$n_groups
  n_bad <- rec$n_non_normal

  resid_normal <- !is.null(resid) && !is.na(resid$normal) &&
    resid$normal == "yes"

  if (resid_normal && n_bad == 0) {
    # Everything normal
    text <- paste0(
      "Assumptions met: residuals and all ", n_groups,
      " groups are normally distributed. ",
      "Classical ANOVA is valid."
    )
    css <- "alert-success"
    icon <- "check-circle-fill"
  } else if (resid_normal && n_bad > 0) {
    # Residuals OK but some groups flagged
    text <- paste0(
      "Model residuals are normally distributed ",
      "— classical ANOVA is valid. ",
      "Per-group: ", n_bad, "/", n_groups,
      " groups non-normal (see details)."
    )
    css <- "alert-success"
    icon <- "check-circle-fill"
  } else if (!resid_normal && n_bad == 0) {
    # Groups OK individually but residuals flagged
    text <- paste0(
      "Per-group normality OK, but model residuals ",
      "are non-normal. ANOVA may still be robust for ",
      "balanced designs. Consider checking QQ-plot."
    )
    css <- "alert-warning"
    icon <- "exclamation-triangle-fill"
  } else {
    # Both non-normal
    text <- paste0(
      n_bad, "/", n_groups,
      " groups non-normal. ",
      "Model residuals are also non-normal. ",
      "Enable 'Normalize data' in Data Processing to attempt ",
      "transformation, or use robust/non-parametric tests."
    )
    css <- "alert-danger"
    icon <- "x-circle-fill"
  }

  shiny$tags$div(
    class = paste0("alert ", css, " py-1 px-2 small mb-0"),
    shiny$tags$div(
      bsicons$bs_icon(icon, class = "me-1"),
      text
    )
  )
}


#' Build a pivoted Shapiro-Wilk HTML table
#'
#' Groups as columns, statistics (n, W, p, status) as rows.
#'
#' @param norm_df Data frame from check_normality()
#' @param title Character, table heading
#' @return shiny tag (HTML table)
build_shapiro_table <- function(norm_df, title) {
  if (is.null(norm_df) || nrow(norm_df) == 0) {
    return(shiny$tags$p(
      class = "text-muted small fst-italic",
      "No normality data available."
    ))
  }

  groups <- norm_df$group

  # Header row: empty cell + group names
  header_cells <- c(
    list(shiny$tags$th("")),
    lapply(groups, function(g) {
      shiny$tags$th(class = "text-center px-2", g)
    })
  )

  # n row
  n_cells <- c(
    list(shiny$tags$td(
      class = "fw-semibold text-muted", "n"
    )),
    lapply(norm_df$n, function(v) {
      shiny$tags$td(class = "text-center px-2", v)
    })
  )

  # W row
  w_cells <- c(
    list(shiny$tags$td(
      class = "fw-semibold text-muted", "W"
    )),
    lapply(norm_df$W, function(v) {
      shiny$tags$td(
        class = "text-center px-2",
        if (is.na(v)) "—" else format(round(v, 3), nsmall = 3)
      )
    })
  )

  # p row
  p_cells <- c(
    list(shiny$tags$td(
      class = "fw-semibold text-muted", "p"
    )),
    lapply(norm_df$p_value, function(v) {
      shiny$tags$td(
        class = "text-center px-2",
        assumption_checks$format_p(v)
      )
    })
  )

  # Status row (checkmark or X)
  status_cells <- c(
    list(shiny$tags$td(
      class = "fw-semibold text-muted", ""
    )),
    lapply(norm_df$normal, function(v) {
      if (is.na(v) || v == "identical values") {
        icon <- bsicons$bs_icon(
          "dash-circle",
          class = "text-muted"
        )
      } else if (v == "yes") {
        icon <- bsicons$bs_icon(
          "check-circle-fill",
          class = "text-success"
        )
      } else {
        icon <- bsicons$bs_icon(
          "x-circle-fill",
          class = "text-danger"
        )
      }
      shiny$tags$td(class = "text-center px-2", icon)
    })
  )

  shiny$tags$div(
    shiny$tags$div(
      class = "small fw-semibold text-muted mb-1", title
    ),
    shiny$tags$table(
      class = "table table-sm table-borderless mb-1 small",
      style = "font-size: 0.8rem;",
      shiny$tags$thead(
        do.call(shiny$tags$tr, header_cells)
      ),
      shiny$tags$tbody(
        do.call(shiny$tags$tr, n_cells),
        do.call(shiny$tags$tr, w_cells),
        do.call(shiny$tags$tr, p_cells),
        do.call(shiny$tags$tr, status_cells)
      )
    )
  )
}


#' Build Levene's test result line
#'
#' @param levene List from check_homogeneity()
#' @return shiny tag
build_levene_line <- function(levene) {
  if (is.null(levene) || is.na(levene$p_value)) {
    return(shiny$tags$div(
      class = "small text-muted fst-italic",
      "Levene's test: insufficient data."
    ))
  }

  icon <- if (levene$equal_variances == "yes") {
    bsicons$bs_icon(
      "check-circle-fill",
      class = "text-success me-1"
    )
  } else {
    bsicons$bs_icon(
      "x-circle-fill",
      class = "text-danger me-1"
    )
  }

  label <- if (levene$equal_variances == "yes") {
    "Equal variances"
  } else {
    "Unequal variances"
  }

  shiny$tags$div(
    class = "small text-muted",
    icon,
    paste0(
      "Levene's: F(", levene$df1, ", ", levene$df2, ") = ",
      format(round(levene$F_statistic, 2), nsmall = 2),
      ", p = ", assumption_checks$format_p(levene$p_value),
      "  — ", label
    )
  )
}


#' Build residual-based normality test result line
#'
#' @param resid_result List from check_normality_residuals()
#' @param label_prefix Character, prefix for the label (default "Residuals")
#' @return shiny tag
build_residuals_line <- function(resid_result,
                                 label_prefix = "Residuals") {
  if (is.null(resid_result) || is.na(resid_result$p_value)) {
    return(shiny$tags$div(
      class = "small text-muted fst-italic",
      paste0(label_prefix, " (model-based): insufficient data.")
    ))
  }

  icon <- if (resid_result$normal == "yes") {
    bsicons$bs_icon(
      "check-circle-fill",
      class = "text-success me-1"
    )
  } else {
    bsicons$bs_icon(
      "x-circle-fill",
      class = "text-danger me-1"
    )
  }

  verdict <- if (resid_result$normal == "yes") {
    "normal"
  } else {
    "non-normal"
  }

  shiny$tags$div(
    class = "small text-muted",
    icon,
    paste0(
      label_prefix, " (model-based): W = ",
      format(round(resid_result$W, 3), nsmall = 3),
      ", p = ", assumption_checks$format_p(resid_result$p_value),
      ", n = ", resid_result$n,
      "  — ", verdict
    )
  )
}
