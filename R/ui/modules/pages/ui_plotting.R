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

            # Step 1: Select Descriptive Columns (always visible)
            shiny::h5("1. Select Descriptive Columns"),
            shiny::selectizeInput(
                inputId = ns("metaData"),
                label = shiny::tags$span(
                    "Descriptive: ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        paste0(
                            "Select columns that describe the data, such as the ",
                            "sample ID, treatment, etc., that are important for your analysis. ",
                            "You can then filter the data using the checkboxes."
                        )
                    )
                ),
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Select descriptive columns...")
            ),
            
            # Step 2: Configure Plot Options (hidden until descriptive selected)
            shiny::div(
                id = ns("step2_container"),
                style = "display: none;",
                shiny::tags$hr(),
                shiny::h5("2. Configure Plot Options"),
                shiny::fluidRow(
                    shiny::column(
                        6,
                        shiny::selectizeInput(
                            inputId = ns("measureVar"),
                            label = shiny::tags$span(
                                "Measurement: ",
                                bslib::tooltip(
                                    bsicons::bs_icon("info-circle", class = "text-muted"),
                                    "Select columns containing measurements to plot."
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
                            inputId = ns("hideCols"),
                            label = shiny::tags$span(
                                "Hide from filter: ",
                                bslib::tooltip(
                                    bsicons::bs_icon("info-circle", class = "text-muted"),
                                    "Hide columns from filtering but keep for hover info."
                                )
                            ),
                            choices = NULL,
                            multiple = TRUE,
                            options = list(placeholder = "Select...")
                        )
                    )
                ),
                shiny::fluidRow(
                    shiny::column(
                        6,
                        shiny::selectizeInput(
                            inputId = ns("xAxis"),
                            label = shiny::tags$span(
                                "X-Axis: ",
                                bslib::tooltip(
                                    bsicons::bs_icon("info-circle", class = "text-muted"),
                                    "Select up to 3 columns for the X-Axis. Also used in statistics."
                                )
                            ),
                            choices = NULL,
                            multiple = TRUE,
                            options = list(placeholder = "Select...", maxItems = 3)
                        )
                    ),
                    shiny::column(
                        6,
                        shiny::selectizeInput(
                            inputId = ns("tooltip"),
                            label = shiny::tags$span(
                                "Tooltip info: ",
                                bslib::tooltip(
                                    bsicons::bs_icon("info-circle", class = "text-muted"),
                                    "Select columns to display when hovering over plot points."
                                )
                            ),
                            choices = NULL,
                            multiple = TRUE,
                            options = list(placeholder = "Select...")
                        )
                    )
                )
            ),
            
            # Step 3: Filter Data (hidden until descriptive selected)
            shiny::div(
                id = ns("step3_container"),
                style = "display: none;",
                shiny::tags$hr(),
                shiny::h5("3. Filter Data"),
                shiny::uiOutput(ns("checkboxes"))
            ),

            # Step 4: Trimming Section (hidden until descriptive selected)
            shiny::div(
                id = ns("step4_container"),
                style = "display: none;",
                shiny::tags$hr(),
                shiny::h5("4. Data Trimming"),
                shiny::sliderInput(
                    inputId = ns("trim_slider"),
                    label = shiny::tags$span(
                        "Trimming Value: ",
                        bslib::tooltip(
                            bsicons::bs_icon("info-circle", class = "text-muted"),
                            paste0(
                                "Data trimming removes a percentage of the highest and lowest values ",
                                "to reduce the impact of outliers."
                            )
                        )
                    ),
                    min = 0,
                    max = 100,
                    value = 0,
                    step = 1
                )
            ),

            # Download Section (hidden until descriptive selected)
            shiny::div(
                id = ns("download_container"),
                style = "display: none;",
                shiny::tags$hr(),
                shiny::downloadButton(
                    outputId = ns("downloadData"),
                    label = "Download Filtered Data",
                    class = "btn-primary btn-sm w-100"
                )
            ),
            
            # Placeholder message when no selection (hidden by default when sections show)
            shiny::tags$p(
                id = ns("placeholder_message"),
                class = "text-muted fst-italic small",
                style = "display: none;",
                "Select at least one descriptive column to continue..."
            )
        ),

        # Main content area - plots will be rendered here
        shiny::uiOutput(ns("plots"))
    )
}
