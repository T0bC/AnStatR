box::use(
  bsicons,
  bslib,
  shiny,
)

box::use(
  app/logic/shared/error_handling,
  app/logic/statistics/parameter_ranking,
  app/view/shared/error_display,
)

# =============================================================================
# UI for the parameter screening ranking section (shown above the
# per-measure omnibus/post-hoc cards when the Plotting tab's "Disable
# plots" checkbox is active).
# =============================================================================

# --- Private helper: table markup shared with the rest of Statistics ---
build_matrix_table <- function(mat, comparisons, is_top_mat, value_fn) {
  shiny$tags$table(
    class = "table table-sm table-striped table-hover mb-0",
    shiny$tags$thead(
      shiny$tags$tr(
        shiny$tags$th(class = "small", "Parameter"),
        lapply(comparisons, function(comp) {
          shiny$tags$th(class = "small", comp)
        }),
        shiny$tags$th(class = "small", "Top count")
      )
    ),
    shiny$tags$tbody(
      lapply(seq_len(nrow(mat)), function(i) {
        row <- mat[i, ]
        shiny$tags$tr(
          shiny$tags$td(class = "small fw-semibold", row$parameter),
          lapply(comparisons, function(comp) {
            is_top <- isTRUE(is_top_mat[row$parameter, comp])
            shiny$tags$td(
              class = if (is_top) {
                "small fw-bold table-success"
              } else {
                "small text-muted"
              },
              value_fn(row, comp)
            )
          }),
          shiny$tags$td(
            class = "small",
            shiny$tags$span(
              class = "badge bg-secondary", row$n_top
            )
          )
        )
      })
    )
  )
}

#' Render the parameter screening ranking section
#'
#' @param ranking_result List from
#'   parameter_ranking$rank_parameters_by_comparison(), or a structured
#'   app_error if the ranking could not be computed
#' @param ns Namespace function from the parent module
#' @return Shiny tags object
#' @export
render_ranking_section <- function(ranking_result, ns) {
  if (is.null(ranking_result)) return(NULL)

  if (error_handling$is_app_error(ranking_result)) {
    return(bslib$card(
      class = "mb-3",
      bslib$card_header("Parameter Screening — Separation Ranking"),
      bslib$card_body(
        error_display$error_alert_structured(
          ranking_result, type = "warning"
        )
      )
    ))
  }

  mat <- parameter_ranking$build_ranking_matrix(ranking_result)
  is_top_mat <- attr(mat, "is_top")
  comparisons <- ranking_result$comparisons

  effect_label <- if (nzchar(ranking_result$effect_label)) {
    ranking_result$effect_label
  } else {
    "Effect size"
  }

  p_col_note <- if (identical(ranking_result$p_column_used, "raw")) {
    "raw"
  } else {
    "adjusted"
  }

  skipped <- ranking_result$skipped

  shiny$tagList(
    bslib$card(
      class = "mb-3",
      bslib$card_header(
        class = paste(
          "py-2 d-flex justify-content-between align-items-center"
        ),
        shiny$tags$span(
          bsicons$bs_icon("bar-chart-steps", class = "me-2"),
          "Parameter Screening — Separation Ranking"
        ),
        shiny$tags$span(
          class = "badge bg-primary",
          paste0(length(ranking_result$recommended), " recommended")
        )
      ),
      bslib$card_body(
        shiny$tags$p(
          class = "text-muted small mb-2",
          "Parameters are ranked per pairwise comparison by ",
          shiny$tags$strong(paste0(p_col_note, " p-value")),
          " (ascending); the conventional 0.05 threshold is deliberately ",
          "not applied — only the relative order within each ",
          "comparison matters. Ties are broken by larger effect-size ",
          "magnitude. The top ", ranking_result$top_n,
          " parameter(s) per comparison are highlighted."
        ),
        shiny$tags$div(
          class = "table-responsive mb-3",
          build_matrix_table(
            mat, comparisons, is_top_mat,
            value_fn = function(row, comp) row[[comp]]
          )
        ),
        shiny$tags$details(
          shiny$tags$summary(
            class = "small text-muted",
            style = "cursor: pointer;",
            paste0("Show effect sizes (", effect_label, ")")
          ),
          shiny$tags$div(
            class = "table-responsive mt-2",
            build_effect_table(ranking_result, comparisons, is_top_mat)
          )
        ),
        shiny$tags$div(
          class = "alert alert-success py-2 px-3 mt-3 mb-0",
          bsicons$bs_icon("check-circle", class = "me-1"),
          shiny$tags$strong("Recommended parameters: "),
          if (length(ranking_result$recommended) == 0) {
            "none ranked in the top comparisons."
          } else {
            shiny$tagList(
              lapply(ranking_result$recommended, function(p) {
                shiny$tags$code(class = "me-1", p)
              }),
              shiny$tags$p(
                class = "small mb-0 mt-1",
                "These parameters placed in the top ",
                ranking_result$top_n,
                " for at least one pairwise comparison. Use them in the ",
                "PCA and LDA tabs."
              )
            )
          }
        ),
        if (!is.null(skipped) && nrow(skipped) > 0) {
          shiny$tags$details(
            class = "mt-2",
            shiny$tags$summary(
              class = "small text-warning",
              style = "cursor: pointer;",
              bsicons$bs_icon("exclamation-triangle", class = "me-1"),
              paste0(nrow(skipped), " measurement(s) excluded from ranking")
            ),
            shiny$tags$ul(
              class = "list-unstyled mb-0 mt-2 ms-3 small",
              lapply(seq_len(nrow(skipped)), function(i) {
                shiny$tags$li(
                  shiny$tags$strong(skipped$measure[i]), ": ",
                  skipped$reason[i],
                  if (nzchar(skipped$detail[i])) paste0(" (", skipped$detail[i], ")")
                )
              })
            )
          )
        }
      )
    )
  )
}

# --- Private helper: effect-size matrix table ---
build_effect_table <- function(ranking_result, comparisons, is_top_mat) {
  ranking <- ranking_result$ranking
  parameters <- unique(ranking$parameter)

  effect_mat <- matrix(
    NA_character_, nrow = length(parameters), ncol = length(comparisons),
    dimnames = list(parameters, comparisons)
  )
  for (i in seq_len(nrow(ranking))) {
    row <- ranking[i, ]
    effect_mat[row$parameter, row$Interaction] <- if (is.na(row$effect)) {
      "—"
    } else {
      as.character(signif(row$effect, 3))
    }
  }

  mat <- data.frame(parameter = parameters, stringsAsFactors = FALSE)
  for (comp in comparisons) {
    mat[[comp]] <- ifelse(is.na(effect_mat[, comp]), "", effect_mat[, comp])
  }
  mat$n_top <- vapply(parameters, function(p) {
    sum(ranking$parameter == p & ranking$is_top)
  }, integer(1))

  build_matrix_table(
    mat, comparisons, is_top_mat,
    value_fn = function(row, comp) row[[comp]]
  )
}
