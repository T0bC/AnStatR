box::use(
  bsicons,
  bslib,
  shiny,
)

#' Format a flag's column list, truncating very long lists
#' @param columns Character vector of column names
#' @param max_shown Maximum number of names to print
#' @return Character string
format_columns <- function(columns, max_shown = 12) {
  if (length(columns) <= max_shown) {
    return(paste(columns, collapse = ", "))
  }
  paste0(
    paste(columns[seq_len(max_shown)], collapse = ", "),
    " (and ", length(columns) - max_shown, " more)"
  )
}

#' Create the data-quality banner shown above the Load tab panels
#'
#' Sits outside the accordion so structural problems cannot be collapsed
#' out of sight. Returns NULL when there is nothing to report: a
#' "nothing is wrong" banner would only push the actual content down.
#'
#' @param flag_result List returned by
#'   `data_overview$detect_quality_flags()`
#' @return A shiny tag, or NULL when no issues were found
#' @export
create_banner <- function(flag_result) {
  if (!isTRUE(flag_result$has_issues)) {
    return(NULL)
  }

  n_flags <- length(flag_result$flags)

  shiny$tags$div(
    class = "alert alert-warning py-2 mb-3",
    shiny$tags$div(
      class = "d-flex align-items-center mb-1",
      bsicons$bs_icon("exclamation-triangle-fill", class = "me-2"),
      shiny$tags$strong(
        if (n_flags == 1) "1 thing to check" else paste(n_flags, "things to check")
      )
    ),
    shiny$tags$ul(
      class = "mb-0 ps-4 small",
      lapply(flag_result$flags, function(flag) {
        shiny$tags$li(
          shiny$tags$span(flag$label),
          shiny$tags$br(),
          shiny$tags$code(
            class = "small",
            format_columns(flag$columns)
          )
        )
      })
    )
  )
}

#' Build a single KPI value box
#'
#' Deliberately understated: the `text-*` themes colour only the figure
#' rather than filling the whole card, and there is no showcase icon —
#' the title already says what the number is, so an icon would only
#' compete with it for the limited width.
#'
#' `min_height` rather than `max_height`: a cap silently clips whatever
#' does not fit, which is how the column breakdown lost its last entry.
#'
#' @param value Main figure
#' @param title Box title
#' @param subtitle Optional supporting content, a string or a shiny tag
#' @param theme bslib value box theme
#' @return A bslib value_box
kpi_box <- function(value, title, subtitle = NULL, theme) {
  bslib$value_box(
    title = shiny$tags$span(class = "small text-muted", title),
    value = shiny$tags$span(class = "fs-4", value),
    theme = theme,
    min_height = "65px",
    class = "border kpi-box",
    if (is.character(subtitle)) {
      shiny$tags$span(class = "small text-muted", subtitle)
    } else {
      subtitle
    }
  )
}

#' Render the column-type breakdown as wrapping pill badges
#'
#' Three counts on one run-on line either overflow the card or wrap
#' mid-separator. Badges wrap cleanly and stay readable at any card
#' width.
#'
#' @param stats List returned by `data_overview$compute_overview_stats()`
#' @return A shiny tag
column_breakdown <- function(stats) {
  parts <- list(
    list(n = stats$n_descriptive, label = "metadata"),
    list(n = stats$n_measurement, label = "measurement"),
    list(n = stats$n_ambiguous, label = "ambiguous")
  )
  parts <- Filter(function(p) p$n > 0, parts)

  shiny$tags$div(
    class = "d-flex flex-wrap gap-1",
    lapply(parts, function(p) {
      shiny$tags$span(
        class = "badge rounded-pill text-bg-light fw-normal",
        paste(p$n, p$label)
      )
    })
  )
}

#' Create the Overview KPI boxes
#'
#' Descriptive + measurement need not sum to the total column count:
#' uppercase-with-digit names are ambiguous under the app's naming
#' convention, so they are called out separately when present.
#'
#' @param stats List returned by `data_overview$compute_overview_stats()`
#' @return A bslib layout of value boxes
#' @export
create_kpi_boxes <- function(stats) {
  bslib$layout_column_wrap(
    width = 1 / 4,
    fill = FALSE,
    gap = "0.5rem",
    kpi_box(
      value = format(stats$n_rows, big.mark = ","),
      title = "Rows",
      theme = "text-secondary"
    ),
    kpi_box(
      value = format(stats$n_cols, big.mark = ","),
      title = "Columns",
      subtitle = column_breakdown(stats),
      theme = "text-secondary"
    ),
    kpi_box(
      value = paste0(stats$pct_missing_cells, "%"),
      title = "Cells missing",
      subtitle = paste0(
        format(stats$n_missing_cells, big.mark = ","), " of ",
        format(stats$n_cells, big.mark = ",")
      ),
      theme = if (stats$pct_missing_cells > 0) {
        "text-warning"
      } else {
        "text-success"
      }
    ),
    kpi_box(
      value = format(stats$n_complete_rows, big.mark = ","),
      title = "Complete rows",
      subtitle = paste0(stats$pct_complete_rows, "% of all rows"),
      theme = if (stats$pct_complete_rows < 100) {
        "text-warning"
      } else {
        "text-success"
      }
    )
  )
}
