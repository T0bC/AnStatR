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
#' Dynamically updates the LD dimension choices when a new
#' LDA result becomes available.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param lda_result Reactive returning the LDA result list
#' @export
tab_server <- function(input, output, session,
                       lda_result) {
  shiny$observeEvent(lda_result(), {
    res <- lda_result()
    if (
      is.null(res) ||
      is.null(res$scores) ||
      ncol(res$scores) == 0
    ) {
      return()
    }

    ld_names <- colnames(res$scores)
    n_ld <- length(ld_names)

    rhino$log$info(
      "LDA plotting_controls: updating dims — ",
      "{n_ld} LD axes available"
    )

    # X defaults to LD1
    shiny$updateSelectizeInput(
      session, "ldDimX",
      choices = ld_names,
      selected = ld_names[1]
    )

    # Y defaults to LD2 (or LD1 if only 1)
    shiny$updateSelectizeInput(
      session, "ldDimY",
      choices = ld_names,
      selected = if (n_ld >= 2) ld_names[2] else ld_names[1]
    )

    # Z defaults to LD3 (or last available)
    shiny$updateSelectizeInput(
      session, "ldDimZ",
      choices = ld_names,
      selected = if (n_ld >= 3) {
        ld_names[3]
      } else {
        ld_names[min(n_ld, 2)]
      }
    )
  }, ignoreNULL = TRUE)
}
