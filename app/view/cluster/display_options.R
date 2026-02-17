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
    icon = "sliders",
    tooltip_text = "Display Options",
    value = "display_tab",
    shiny$h6(class = "text-muted mb-3", "Display Options"),
    # Display options row
    shiny$tags$div(
      class = "row g-2",
      shiny$tags$div(
        class = "col-6",
        shiny$checkboxInput(
          inputId = ns("horizDendro"),
          label = shiny$tags$span(
            "Horizontal Dend ",
            bslib$tooltip(
              bsicons$bs_icon("info-circle", class = "text-muted"),
              "Display the dendrogram horizontally."
            )
          ),
          value = FALSE
        )
      ),
      shiny$tags$div(
        class = "col-6",
        shiny$checkboxInput(
          inputId = ns("showLabels"),
          label = shiny$tags$span(
            "Show Labels ",
            bslib$tooltip(
              bsicons$bs_icon("info-circle", class = "text-muted"),
              "Show the labels on Plots"
            )
          ),
          value = FALSE
        )
      ),
      shiny$tags$div(
        class = "col-6",
        shiny$checkboxInput(
          inputId = ns("polarDend"),
          label = shiny$tags$span(
            "Polar Dend ",
            bslib$tooltip(
              bsicons$bs_icon("info-circle", class = "text-muted"),
              "Display the dendrogram in a polar layout."
            )
          ),
          value = FALSE
        )
      )
    ),
    shiny$tags$hr(),
    # Group Biplot
    shiny$selectizeInput(
      inputId = ns("groupBiplot"),
      label = shiny$tags$span(
        "Group Biplot ",
        bslib$tooltip(
          bsicons$bs_icon("info-circle", class = "text-muted"),
          paste0(
            "Select columns that potentially group your data into",
            "different clusters or categories. This will color code the",
            "Biplot according to the selected column."
          )
        )
      ),
      choices = NULL,
      multiple = TRUE,
      selected = "CLUSTER",
      options = list(
        placeholder = "Select grouping columns...",
        closeAfterSelect = FALSE
      )
    ),
    shiny$tags$hr(),
    # Plot export size
    shiny$helpText("Customize the plot size when clicking on the download button."),
    shiny$tags$div(
      class = "row g-2",
      shiny$tags$div(
        class = "col-6",
        shiny$numericInput(
          inputId = ns("width"),
          label = shiny$tags$span(
            "Width (cm) ",
            bslib$tooltip(
              bsicons$bs_icon("info-circle", class = "text-muted"),
              paste0(
                "Set the width of the plot in centimeters for export.",
                "A value of 16 cm of width correlates with the page",
                "with in typical Microsoft Word documents."
              )
            )
          ),
          value = 8,
          min = 1,
          max = 50
        )
      ),
      shiny$tags$div(
        class = "col-6",
        shiny$numericInput(
          inputId = ns("height"),
          label = shiny$tags$span(
            "Height (cm) ",
            bslib$tooltip(
              bsicons$bs_icon("info-circle", class = "text-muted"),
              paste0(
                "Set the height of the plot in centimeters for export.",
                "In combination with a width of 16 cm, a good value could",
                "be 10 cm. That make a nice ratio."
              )
            )
          ),
          value = 8,
          min = 1,
          max = 50
        )
      )
    )
  )
}

#' Server logic for the Cluster display options sidebar tab
#'
#' Handles display settings for plots and visualizations.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param input_data Reactive returning the current data frame
#' @param data_version Reactive returning the data version counter
#' @export
tab_server <- function(input, output, session,
                       input_data, data_version) {
  # Reset display options when new data is loaded
  shiny$observeEvent(data_version(), {
    rhino$log$info(
      "Cluster display_options: reset for new data"
    )
    # Reset to default values
    shiny$updateCheckboxInput(
      session, "horizDendro",
      value = FALSE
    )
    shiny$updateCheckboxInput(
      session, "showLabels",
      value = FALSE
    )
    shiny$updateCheckboxInput(
      session, "polarDend",
      value = FALSE
    )
    shiny$updateNumericInput(
      session, "width",
      value = 8
    )
    shiny$updateNumericInput(
      session, "height",
      value = 8
    )
  }, ignoreInit = TRUE)

  # GroupBiplot choices are updated in data_selection to avoid conflicts.
}
