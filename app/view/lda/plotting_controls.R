box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
)

box::use(
  app/view/components/sidebar_tabs,
)

#' @export
tab_ui <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "palette",
    tooltip_text = "Plotting Controls",
    value = "plotting_tab",
    shiny$h6(
      class = "text-muted mb-3",
      "LDA Plotting Controls"
    ),
    # LD dimension selection
    shiny$fluidRow(
      shiny$column(
        4,
        shiny$selectizeInput(
          inputId = ns("ldDimX"),
          label = shiny$tags$span(
            "Dim.X ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              ),
              paste(
                "Select the linear discriminant",
                "for the x-axis of the LD plot."
              )
            )
          ),
          choices = c("LD1", "LD2"),
          selected = "LD1"
        )
      ),
      shiny$column(
        4,
        shiny$selectizeInput(
          inputId = ns("ldDimY"),
          label = shiny$tags$span(
            "Dim.Y ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              ),
              paste(
                "Select the linear discriminant",
                "for the y-axis of the LD plot."
              )
            )
          ),
          choices = c("LD1", "LD2"),
          selected = "LD2"
        )
      ),
      shiny$column(
        4,
        shiny$selectizeInput(
          inputId = ns("ldDimZ"),
          label = shiny$tags$span(
            "Dim.Z ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              ),
              paste(
                "Select the linear discriminant",
                "for the z-axis (reserved for",
                "future 3D plot)."
              )
            )
          ),
          choices = c("LD1", "LD2"),
          selected = "LD2"
        )
      )
    ),
    # Assumption diagnostics overlay toggle
    shiny$checkboxInput(
      inputId = ns("show_diagnostics"),
      label = shiny$tags$span(
        "Show Assumption Diagnostics ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle",
            class = "text-muted"
          ),
          paste(
            "Overlay per-group (solid) and pooled",
            "within-group (dashed) covariance",
            "ellipses on the LD Scores plot.",
            "If both match, the equal-covariance",
            "assumption holds."
          )
        )
      ),
      value = FALSE
    ),
    # Decision boundaries overlay toggle
    shiny$checkboxInput(
      inputId = ns("show_boundaries"),
      label = shiny$tags$span(
        "Show Decision Boundaries ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle",
            class = "text-muted"
          ),
          paste(
            "Overlay classification decision",
            "regions and boundary lines on the",
            "LD Scores plot. Shaded areas show",
            "which group the model predicts for",
            "each region of the LD space."
          )
        )
      ),
      value = FALSE
    ),
    shiny$tags$hr(),
    # Plot dimensions for export
    shiny$fluidRow(
      shiny$column(
        6,
        shiny$numericInput(
          inputId = ns("width"),
          label = shiny$tags$span(
            "Width (cm) ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              ),
              paste(
                "Set the width of the plot in cm",
                "for export. A value of 16 cm",
                "correlates with the page width",
                "in typical Word documents."
              )
            )
          ),
          value = 16,
          min = 1,
          max = 50
        )
      ),
      shiny$column(
        6,
        shiny$numericInput(
          inputId = ns("height"),
          label = shiny$tags$span(
            "Height (cm) ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              ),
              paste(
                "Set the height of the plot in cm",
                "for export. In combination with",
                "a width of 16 cm, a good value",
                "could be 10 cm."
              )
            )
          ),
          value = 10,
          min = 1,
          max = 50
        )
      )
    )
  )
}

#' Server logic for the LDA plotting controls sidebar tab
#'
#' Dynamically updates the dimension choices when a new
#' LDA or QDA result becomes available. For QDA, offers
#' both LD axes (from companion LDA) and original
#' numeric variables as axis options.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param lda_result Reactive returning the LDA/QDA result list
#' @export
tab_server <- function(input, output, session,
                       lda_result) {
  shiny$observeEvent(lda_result(), {
    res <- lda_result()
    if (is.null(res)) return()

    if (res$analysis_type == "lda") {
      # LDA: use LD scores
      if (
        is.null(res$scores) ||
        ncol(res$scores) == 0
      ) {
        return()
      }
      ld_names <- colnames(res$scores)
      n_ld <- length(ld_names)

      rhino$log$info(
        "plotting_controls: LDA — ",
        "{n_ld} LD axes available"
      )

      shiny$updateSelectizeInput(
        session, "ldDimX",
        choices = ld_names,
        selected = ld_names[1]
      )
      shiny$updateSelectizeInput(
        session, "ldDimY",
        choices = ld_names,
        selected = if (n_ld >= 2) {
          ld_names[2]
        } else {
          ld_names[1]
        }
      )
      shiny$updateSelectizeInput(
        session, "ldDimZ",
        choices = ld_names,
        selected = if (n_ld >= 3) {
          ld_names[3]
        } else {
          ld_names[min(n_ld, 2)]
        }
      )
    } else if (res$analysis_type == "qda") {
      # QDA: offer LD axes (companion) + original vars
      ld_names <- if (!is.null(res$lda_scores)) {
        colnames(res$lda_scores)
      } else {
        character(0)
      }
      orig_names <- res$columns

      # Build grouped choices list
      choices <- list()
      if (length(ld_names) > 0) {
        choices[["LD Axes (LDA projection)"]] <-
          ld_names
      }
      if (length(orig_names) > 0) {
        choices[["Original Variables"]] <- orig_names
      }

      n_ld <- length(ld_names)
      default_x <- if (n_ld >= 1) {
        ld_names[1]
      } else {
        orig_names[1]
      }
      default_y <- if (n_ld >= 2) {
        ld_names[2]
      } else if (length(orig_names) >= 2) {
        orig_names[2]
      } else {
        default_x
      }

      rhino$log$info(
        "plotting_controls: QDA — ",
        "{n_ld} LD axes + ",
        "{length(orig_names)} original vars"
      )

      shiny$updateSelectizeInput(
        session, "ldDimX",
        choices = choices,
        selected = default_x
      )
      shiny$updateSelectizeInput(
        session, "ldDimY",
        choices = choices,
        selected = default_y
      )
      shiny$updateSelectizeInput(
        session, "ldDimZ",
        choices = choices,
        selected = default_y
      )
    }
  }, ignoreNULL = TRUE)
}
