#' Data Selection Tab UI Component
#'
#' Creates the Data Selection tab for the plotting sidebar.
#'
#' @param ns Namespace function from parent module
#' @return A bslib::nav_panel element
create_data_selection_tab <- function(ns) {
    bslib::nav_panel(
        title = bslib::tooltip(
            bsicons::bs_icon("table", size = "1.2em"),
            "Data Selection"
        ),
        value = "data_tab",
        shiny::tags$div(
            class = "pt-3",
            shiny::h6(class = "text-muted mb-3", "Data Selection"),
            # Step 1: Descriptive columns (always visible)
            shiny::selectizeInput(
                inputId = ns("metaData"),
                label = shiny::tags$span(
                    "Descriptive columns ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        "Select columns that describe the data (sample ID, treatment, etc.) for filtering and grouping."
                    )
                ),
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Select descriptive columns...")
            ),
            # Step 2: Measurement columns (shown when metaData selected)
            shiny::conditionalPanel(
                condition = "input.metaData && input.metaData.length > 0",
                ns = ns,
                shiny::selectizeInput(
                    inputId = ns("measureVar"),
                    label = shiny::tags$span(
                        "Measurement columns (Y-Axis) ",
                        bslib::tooltip(
                            bsicons::bs_icon("info-circle", class = "text-muted"),
                            "Select columns containing measurements to plot. One plot per column."
                        )
                    ),
                    choices = NULL,
                    multiple = TRUE,
                    options = list(placeholder = "Select measurement columns...")
                ),
                # Step 3: X-Axis and Tooltip (shown when measureVar selected)
                shiny::conditionalPanel(
                    condition = "input.measureVar && input.measureVar.length > 0",
                    ns = ns,
                    shiny::tags$hr(),
                    shiny::fluidRow(
                        shiny::column(
                            6,
                            shiny::selectizeInput(
                                inputId = ns("xAxis"),
                                label = shiny::tags$span(
                                    "X-Axis ",
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
                                    "Tooltip ",
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
                )
            )
        )
    )
}
