box::use(
  bsicons,
  shiny,
)

# =============================================================================
# Save-time confirmation dialog for a model's training filter.
#
# When a model is fitted on a filtered subset, that subset is part of the
# model's contract: a model trained on "M1, buccal facet only" must not be
# applied to a test set containing every tooth and facet.
#
# But only some filtered columns can be reapplied. Structural columns
# (TOOTH, FACET) share a vocabulary with the unknown data; identity
# columns (SPECIMEN_ID, SPECIES) do not, and reapplying them would match
# zero rows. That distinction cannot be inferred reliably -- SPECIES is
# identity in one study and structural in another -- so the user confirms
# it here, against a heuristic default.
# =============================================================================

#' Build a Shiny download link styled as a button
#'
#' The bundle download controls are plain anchors carrying the
#' shiny-download-link class rather than downloadButton() calls, so the
#' same markup can be rendered either inline or inside the modal footer.
#'
#' @param ns Namespace function from the calling module
#' @param id Character, unnamespaced download output id
#' @param label Character, button text
#' @param class Character, CSS classes for the anchor
#' @param icon Character, Bootstrap icon name
#' @return A shiny tags$a element
#' @export
download_link <- function(ns, id, label,
                          class = "btn btn-outline-secondary",
                          icon = "file-earmark-code") {
  shiny$tags$a(
    id = ns(id),
    class = paste(class, "shiny-download-link"),
    href = "",
    target = "_blank",
    download = NA,
    bsicons$bs_icon(icon, class = "me-2"),
    label
  )
}

#' Build the training-filter confirmation modal
#'
#' @param constrained Character vector of columns the filter narrowed
#' @param suggested Character vector pre-ticked as reapplicable
#' @param filter_state Named list of selected values per column
#' @param level_counts Named integer vector: distinct levels available
#'   per column in the unfiltered data, shown so the user can tell a
#'   4-level TOOTH from a 480-level SPECIMEN_ID
#' @param ns Namespace function from the calling module
#' @param checkbox_id Character, unnamespaced id for the checkbox group
#' @param download_button UI element for the actual download control,
#'   placed in the footer (a downloadHandler cannot open a modal
#'   mid-download, so the button lives here instead)
#' @return A shiny modalDialog object
#' @export
create_modal <- function(constrained, suggested, filter_state,
                         level_counts, ns, checkbox_id,
                         download_button) {
  choices <- constrained
  names(choices) <- vapply(constrained, function(col) {
    selected <- as.character(filter_state[[col]])
    n_available <- level_counts[[col]]
    paste0(
      col, "  —  ", paste(selected, collapse = ", "),
      "  (", length(selected), " of ",
      if (is.null(n_available)) "?" else n_available,
      " levels)"
    )
  }, character(1))

  shiny$modalDialog(
    title = shiny$tags$span(
      bsicons$bs_icon("funnel-fill", class = "text-primary"),
      " Training filter"
    ),
    size = "l",
    easyClose = FALSE,
    footer = shiny$tagList(
      shiny$modalButton("Cancel"),
      download_button
    ),
    shiny$tags$p(
      "This model was fitted on a filtered subset of the data.",
      " Tick the columns that must also hold for unknown data, so the",
      " same subset is selected before predicting."
    ),
    shiny$tags$p(
      class = "text-muted small",
      shiny$tags$strong("Tick"),
      " columns that describe ", shiny$tags$em("where"),
      " a measurement was taken (tooth, facet, jaw) — unknown",
      " specimens have these too.",
      shiny$tags$br(),
      shiny$tags$strong("Leave unticked"),
      " columns that identify ", shiny$tags$em("which"),
      " specimens were studied (specimen ID, species, site) —",
      " unknown specimens have different values, so requiring them",
      " would match no rows."
    ),
    shiny$tags$hr(),
    shiny$checkboxGroupInput(
      inputId = ns(checkbox_id),
      label = "Require in unknown data:",
      choices = choices,
      selected = suggested
    )
  )
}
