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
    icon = "table",
    tooltip_text = "Data Selection",
    value = "data_tab",
    shiny$h6(class = "text-muted mb-3", "Data Selection"),
    shiny$helpText(
      paste(
        "Select the correct columns for the PCA.",
        "Avoid columns with many empty cells.",
        "Rows with empty cells are deleted!"
      )
    ),
    # Metadata columns selection
    shiny$selectizeInput(
      inputId = ns("metaData"),
      label = shiny$tags$span(
        "Descriptive (metadata) columns ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Select columns that describe the",
            "data, such as the sample ID,",
            "treatment, etc., that are important",
            "for your analysis."
          )
        )
      ),
      choices = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Select descriptive columns..."
      )
    ),
    # Measurement columns selection
    shiny$selectizeInput(
      inputId = ns("measureVar"),
      label = shiny$tags$span(
        "Measurement columns ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Select columns that contain the",
            "actual measurements, such as texture",
            "or other parameters, that you want",
            "to include in the PCA analysis.",
            "Only select columns that contain",
            "numerical data!"
          )
        )
      ),
      choices = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Select measurement columns..."
      )
    ),
    shiny$tags$hr(),
    # Scale data checkbox
    shiny$checkboxInput(
      inputId = ns("scale_data"),
      label = shiny$tags$span(
        "Scale Data ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Select if you want to scale the data",
            "before performing the PCA. Scaling is",
            "important if the variables are",
            "measured in different magnitudes."
          )
        )
      ),
      value = TRUE
    )
  )
}
