#' PCA Data Selection Tab UI Component
#'
#' Creates the Data Selection tab for the PCA sidebar.
#'
#' @param ns Namespace function from parent module
#' @return A bslib::nav_panel element
#' @export
create_pca_data_selection_tab <- function(ns) {
    bslib::nav_panel(
        title = bslib::tooltip(
            bsicons::bs_icon("table", size = "1.2em"),
            "Data Selection"
        ),
        value = "data_tab",
        shiny::tags$div(
            class = "pt-3",
            shiny::h6(class = "text-muted mb-3", "Data Selection"),
            shiny::helpText(
                "Select the correct columns for the PCA. Avoid columns with many empty cells. Rows with empty cells are deleted!"
            ),
            # Metadata columns selection
            shiny::selectizeInput(
                inputId = ns("metaData"),
                label = shiny::tags$span(
                    "Descriptive (metadata) columns ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        "Select columns that describe the data, such as the sample ID, treatment, etc., that are important for your analysis."
                    )
                ),
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Select descriptive columns...")
            ),
            # Measurement columns selection
            shiny::selectizeInput(
                inputId = ns("measureVar"),
                label = shiny::tags$span(
                    "Measurement columns ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        "Select columns that contain the actual measurements, such as the texture or other parameters, that you want to include in the PCA analysis. Only select columns that contain numerical data!"
                    )
                ),
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Select measurement columns...")
            ),
            shiny::tags$hr(),
            # Scale data checkbox
            shiny::checkboxInput(
                inputId = ns("scale_data"),
                label = shiny::tags$span(
                    "Scale Data ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        "Select if you want to scale the data before performing the PCA. Scaling is important if the variables are measured in different magnitudes."
                    )
                ),
                value = TRUE
            )
        )
    )
}
