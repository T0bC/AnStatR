#' UI for the Plotting page
#'
#' @param id Module namespace ID
#' @return A bslib layout_sidebar UI element
UI_plotting <- function(id) {
    ns <- shiny::NS(id)

    bslib::layout_sidebar(
        sidebar = bslib::sidebar(
            title = "Plot Configuration",
            width = 350,

            # Column Selection Section
            shiny::h5("1. Select Columns"),
            shiny::fluidRow(
                shiny::column(
                    6,
                    shiny::selectizeInput(
                        inputId = ns("metaData"),
                        label = shiny::tags$span(
                            "Descriptive: ",
                            bslib::tooltip(
                                bsicons::bs_icon("info-circle", class = "text-muted"),
                                paste0(
                                    "Select columns that describe the data, such as the ",
                                    "sample ID, treatment, etc., that are important for your analysis. ",
                                    "You can then filter the data using the checkboxes. Columns ",
                                    "with more than 20 unique levels are excluded."
                                )
                            )
                        ),
                        choices = NULL,
                        multiple = TRUE,
                        options = list(placeholder = "Select...")
                    )
                ),
                shiny::column(
                    6,
                    shiny::selectizeInput(
                        inputId = ns("measureVar"),
                        label = shiny::tags$span(
                            "Measurement: ",
                            bslib::tooltip(
                                bsicons::bs_icon("info-circle", class = "text-muted"),
                                paste0(
                                    "Select columns that contain the actual measurements, ",
                                    "such as the texture or other parameters, that you want to plot."
                                )
                            )
                        ),
                        choices = NULL,
                        multiple = TRUE,
                        options = list(placeholder = "Select...")
                    )
                )
            ),
            shiny::selectizeInput(
                inputId = ns("hideCols"),
                label = shiny::tags$span(
                    "Hide columns from filtering: ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        paste0(
                            "Select columns to hide from filtering, like SPEC_ID, ",
                            "which might be useful for hover info but not for filtering."
                        )
                    )
                ),
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Select columns to hide...")
            ),

            # Data Filtering Section
            shiny::tags$hr(),
            shiny::h5("2. Filter Data"),
            shiny::uiOutput(ns("checkboxes")),

            # Trimming Section
            shiny::tags$hr(),
            shiny::h5("3. Data Trimming"),
            shiny::sliderInput(
                inputId = ns("trim_slider"),
                label = shiny::tags$span(
                    "Trimming Value: ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        paste0(
                            "Data trimming removes a percentage of the highest and lowest values ",
                            "to reduce the impact of outliers. Useful for non-normal distributions ",
                            "or when heteroscedasticity is present."
                        )
                    )
                ),
                min = 0,
                max = 100,
                value = 0,
                step = 1
            ),

            # Download Section
            shiny::tags$hr(),
            shiny::downloadButton(
                outputId = ns("downloadData"),
                label = "Download Filtered Data",
                class = "btn-primary btn-sm w-100"
            )
        ),

        # Main content area - plots will be rendered here
        shiny::uiOutput(ns("plots"))
    )
}
