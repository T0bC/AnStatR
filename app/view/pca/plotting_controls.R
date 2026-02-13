box::use(
  bsicons,
  bslib,
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
      "PCA Plotting Controls"
    ),
    # Biplot layer toggle
    shiny$radioButtons(
      inputId = ns("biplotLayer"),
      label = shiny$tags$span(
        "Biplot Layer ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Select which layers to display",
            "in the biplot: individual scores,",
            "variable loadings, or both combined."
          )
        )
      ),
      choices = c(
        "Individuals" = "individuals",
        "Variables (Loadings)" = "variables",
        "Combined" = "combined"
      ),
      selected = "combined",
      inline = TRUE
    ),
    shiny$tags$hr(),
    # Group Biplot selection
    shiny$selectizeInput(
      inputId = ns("GroupBiplot"),
      label = shiny$tags$span(
        "Group Biplot ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Select columns that potentially",
            "group your data into different",
            "clusters or categories. This will",
            "color code the Biplot according to",
            "the selected column."
          )
        )
      ),
      choices = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Select grouping columns..."
      )
    ),
    # Convex Hull checkbox
    shiny$checkboxInput(
      inputId = ns("showConvexHull"),
      label = shiny$tags$span(
        "Use Convex Hull ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Select if you want to show the",
            "convex hull instead of the",
            "95% ellipse."
          )
        )
      ),
      value = FALSE
    ),
    shiny$tags$hr(),
    # Point Alpha and Size
    shiny$fluidRow(
      shiny$column(
        6,
        shiny$selectInput(
          inputId = ns("pointAlpha"),
          label = shiny$tags$span(
            "Point Alpha ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              ),
              paste(
                "Select the alpha value for the",
                "points in the Biplot. The alpha",
                "value can be set to the",
                "contribution of the Individual",
                "to Dim.1. You may set it to a",
                "fixed value."
              )
            )
          ),
          choices = c(
            "Contrib." = "Contribution",
            "0.25" = 0.25,
            "0.5" = 0.5,
            "0.75" = 0.75,
            "1.0" = 1.0
          ),
          selected = "Contribution"
        )
      ),
      shiny$column(
        6,
        shiny$selectInput(
          inputId = ns("pointSize"),
          label = shiny$tags$span(
            "Point Size ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              ),
              paste(
                "Select the point size for the",
                "points in the Biplot. The point",
                "size can be set to the",
                "contribution of the Individual",
                "to Dim.1. You may set it to a",
                "fixed value."
              )
            )
          ),
          choices = c(
            "Contrib." = "Contribution",
            "1" = 1, "2" = 2, "3" = 3,
            "4" = 4, "5" = 5, "6" = 6,
            "7" = 7, "8" = 8, "9" = 9,
            "10" = 10
          ),
          selected = "Contribution"
        )
      )
    ),
    shiny$tags$hr(),
    # Dimension selection
    shiny$fluidRow(
      shiny$column(
        4,
        shiny$selectizeInput(
          inputId = ns("dimX"),
          label = shiny$tags$span(
            "Dim.X ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              ),
              paste(
                "Select the dimension for the",
                "x-axis of the Biplot."
              )
            )
          ),
          choices = c(
            "Dim.1", "Dim.2", "Dim.3"
          ),
          selected = "Dim.1"
        )
      ),
      shiny$column(
        4,
        shiny$selectizeInput(
          inputId = ns("dimY"),
          label = shiny$tags$span(
            "Dim.Y ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              ),
              paste(
                "Select the dimension for the",
                "y-axis of the Biplot."
              )
            )
          ),
          choices = c(
            "Dim.1", "Dim.2", "Dim.3"
          ),
          selected = "Dim.2"
        )
      ),
      shiny$column(
        4,
        shiny$selectizeInput(
          inputId = ns("dimZ"),
          label = shiny$tags$span(
            "Dim.Z ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              ),
              paste(
                "Select the dimension for the",
                "z-axis of the Biplot."
              )
            )
          ),
          choices = c(
            "Dim.1", "Dim.2", "Dim.3"
          ),
          selected = "Dim.3"
        )
      )
    ),
    shiny$tags$hr(),
    # Plot dimensions
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
          value = 8,
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
          value = 8,
          min = 1,
          max = 50
        )
      )
    ),
    # Show title checkbox
    shiny$checkboxInput(
      inputId = ns("title"),
      label = "Show Title",
      value = TRUE
    )
  )
}
