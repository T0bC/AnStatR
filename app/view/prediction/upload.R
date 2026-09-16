box::use(
  bsicons,
  bslib,
  shiny,
)

box::use(
  app/logic/shared/filter_spec,
  app/view/components/sidebar_tabs,
)

#' @export
tab_ui <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "upload",
    tooltip_text = "Upload",
    value = "upload_tab",
    shiny$h6(class = "text-muted mb-3", "Upload"),
    # RDS bundle upload
    shiny$fileInput(
      inputId = ns("bundle_file"),
      label = shiny$tags$span(
        shiny$tags$strong("Model bundle (.rds) "),
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle",
            class = "text-muted"
          ),
          paste(
            "Upload a .rds bundle exported from the",
            "PCA, LDA, QDA, or MDA tab. This file",
            "contains the trained model and all",
            "preprocessing parameters."
          )
        )
      ),
      accept = ".rds",
      placeholder = "Select .rds file..."
    ),
    shiny$uiOutput(ns("bundle_summary")),
    shiny$tags$hr(),
    # Unknown data upload
    shiny$fileInput(
      inputId = ns("unknown_file"),
      label = shiny$tags$span(
        shiny$tags$strong("Unknown data "),
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle",
            class = "text-muted"
          ),
          paste(
            "Upload a CSV or Excel file containing",
            "the unknown samples to classify or",
            "project. Must have the same measurement",
            "columns as the training data."
          )
        )
      ),
      accept = c(".csv", ".xlsx"),
      placeholder = "Select CSV or Excel..."
    ),
    shiny$uiOutput(ns("unknown_summary"))
  )
}

#' @export
tab_server <- function(input, output, session,
                       bundle_reactive,
                       unknown_data_reactive,
                       validation_reactive) {
  ns <- session$ns

  # Bundle summary card
  output$bundle_summary <- shiny$renderUI({
    bundle <- bundle_reactive()
    if (is.null(bundle)) {
      return(NULL)
    }

    analysis_label <- switch(bundle$analysis_type,
      plsda = "PLS-DA",
      splsda = "sPLS-DA",
      toupper(bundle$analysis_type)
    )
    n_train <- nrow(bundle$used_data)
    n_vars <- length(bundle$numeric_cols)
    src <- bundle$data_source %||% "raw"
    created <- if (!is.null(bundle$created)) {
      format(bundle$created, "%Y-%m-%d %H:%M")
    } else {
      "unknown"
    }
    version <- bundle$app_version %||% "?"

    # Surfaced here rather than only on failure, so the user sees what
    # the model requires of the unknown data before uploading it.
    filter_line <- filter_spec$describe_filter_spec(bundle$filter_spec)

    shiny$tags$div(
      class = "alert alert-success py-2 px-2 small mb-2",
      bsicons$bs_icon(
        "check-circle-fill",
        class = "me-1"
      ),
      shiny$tags$strong(analysis_label),
      " bundle loaded",
      shiny$tags$br(),
      shiny$tags$span(
        class = "text-muted",
        paste0(
          n_train, " training obs, ",
          n_vars, " variables"
        ),
        shiny$tags$br(),
        paste0(
          "Source: ", src,
          " \u2022 v", version,
          " \u2022 ", created
        )
      ),
      if (!is.null(filter_line)) {
        shiny$tags$div(
          class = "mt-1",
          bsicons$bs_icon("funnel", class = "me-1"),
          shiny$tags$strong("Training filter: "),
          filter_line
        )
      }
    )
  })

  # Unknown data summary
  output$unknown_summary <- shiny$renderUI({
    unknown <- unknown_data_reactive()
    if (is.null(unknown)) {
      return(NULL)
    }

    val <- validation_reactive()

    n_rows <- nrow(unknown)
    n_cols <- ncol(unknown)

    # Status badge
    badge <- if (is.null(val)) {
      shiny$tags$span(
        class = "badge bg-secondary",
        "Not validated"
      )
    } else if (val$valid && length(val$warnings) == 0) {
      shiny$tags$span(
        class = "badge bg-success",
        "Ready"
      )
    } else if (val$valid) {
      shiny$tags$span(
        class = "badge bg-warning text-dark",
        paste0(length(val$warnings), " warning(s)")
      )
    } else {
      shiny$tags$span(
        class = "badge bg-danger",
        paste0(length(val$errors), " error(s)")
      )
    }

    # When the model carries a training filter, report how much of the
    # upload survives it -- a large drop should be visible here rather
    # than come as a surprise in the results table.
    match_line <- if (
      !is.null(val) &&
        !is.null(val$n_matching) &&
        !is.null(val$n_uploaded) &&
        val$n_matching < val$n_uploaded
    ) {
      shiny$tags$div(
        class = "mt-1",
        bsicons$bs_icon("funnel", class = "me-1"),
        paste0(
          val$n_matching, " of ", val$n_uploaded,
          " rows match the training filter"
        )
      )
    }

    content <- shiny$tagList(
      shiny$tags$div(
        class = "alert alert-info py-2 px-2 small mb-2",
        bsicons$bs_icon(
          "file-earmark-spreadsheet",
          class = "me-1"
        ),
        paste0(n_rows, " rows, ", n_cols, " columns "),
        badge,
        match_line
      )
    )

    # Show errors or warnings
    if (!is.null(val)) {
      if (length(val$errors) > 0) {
        content <- shiny$tagList(
          content,
          shiny$tags$div(
            class = paste(
              "alert alert-danger py-1",
              "px-2 small mb-1"
            ),
            shiny$tags$ul(
              class = "mb-0 ps-3",
              lapply(val$errors, function(e) {
                shiny$tags$li(e)
              })
            )
          )
        )
      }
      if (length(val$warnings) > 0) {
        content <- shiny$tagList(
          content,
          shiny$tags$div(
            class = paste(
              "alert alert-warning py-1",
              "px-2 small mb-1"
            ),
            shiny$tags$ul(
              class = "mb-0 ps-3",
              lapply(val$warnings, function(w) {
                shiny$tags$li(w)
              })
            )
          )
        )
      }
    }

    content
  })
}
