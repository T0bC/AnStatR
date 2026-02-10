#' PCA Plotting Controls Tab UI Component
#'
#' Creates the Plotting Controls tab for the PCA sidebar.
#'
#' @param ns Namespace function from parent module
#' @return A bslib::nav_panel element
#' @export
create_pca_plotting_controls_tab <- function(ns) {
    bslib::nav_panel(
        title = bslib::tooltip(
            bsicons::bs_icon("palette", size = "1.2em"),
            "Plotting Controls"
        ),
        value = "plotting_tab",
        shiny::tags$div(
            class = "pt-3",
            shiny::h6(class = "text-muted mb-3", "PCA Plotting Controls"),
            # Group Biplot selection
            shiny::selectizeInput(
                inputId = ns("GroupBiplot"),
                label = shiny::tags$span(
                    "Group Biplot ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        "Select columns that potentially group your data into different clusters or categories. This will color code the Biplot according to the selected column."
                    )
                ),
                choices = NULL,
                multiple = TRUE,
                options = list(placeholder = "Select grouping columns...")
            ),
            # Convex Hull checkbox
            shiny::checkboxInput(
                inputId = ns("showConvexHull"),
                label = shiny::tags$span(
                    "Use Convex Hull ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        "Select if you want to show the convex hull instead of the 95% ellipse."
                    )
                ),
                value = FALSE
            ),
            shiny::tags$hr(),
            # Point Alpha and Size
            shiny::fluidRow(
                shiny::column(
                    6,
                    shiny::selectInput(
                        inputId = ns("pointAlpha"),
                        label = shiny::tags$span(
                            "Point Alpha ",
                            bslib::tooltip(
                                bsicons::bs_icon("info-circle", class = "text-muted"),
                                "Select the alpha value for the points in the Biplot. The alpha value can be set to the contribution of the Individual to Dim.1. You may set it to a fixed value."
                            )
                        ),
                        choices = c("Contrib." = "Contribution", "0.25" = 0.25, "0.5" = 0.5, "0.75" = 0.75, "1.0" = 1.0),
                        selected = "Contribution"
                    )
                ),
                shiny::column(
                    6,
                    shiny::selectInput(
                        inputId = ns("pointSize"),
                        label = shiny::tags$span(
                            "Point Size ",
                            bslib::tooltip(
                                bsicons::bs_icon("info-circle", class = "text-muted"),
                                "Select the point size for the points in the Biplot. The point size can be set to the contribution of the Individual to Dim.1. You may set it to a fixed value."
                            )
                        ),
                        choices = c("Contrib." = "Contribution", "1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7, "8" = 8, "9" = 9, "10" = 10),
                        selected = "Contribution"
                    )
                )
            ),
            shiny::tags$hr(),
            # Dimension selection
            shiny::fluidRow(
                shiny::column(
                    4,
                    shiny::selectizeInput(
                        inputId = ns("dimX"),
                        label = shiny::tags$span(
                            "Dim.X ",
                            bslib::tooltip(
                                bsicons::bs_icon("info-circle", class = "text-muted"),
                                "Select the dimension for the x-axis of the Biplot."
                            )
                        ),
                        choices = c("Dim.1", "Dim.2", "Dim.3"),
                        selected = "Dim.1"
                    )
                ),
                shiny::column(
                    4,
                    shiny::selectizeInput(
                        inputId = ns("dimY"),
                        label = shiny::tags$span(
                            "Dim.Y ",
                            bslib::tooltip(
                                bsicons::bs_icon("info-circle", class = "text-muted"),
                                "Select the dimension for the y-axis of the Biplot."
                            )
                        ),
                        choices = c("Dim.1", "Dim.2", "Dim.3"),
                        selected = "Dim.2"
                    )
                ),
                shiny::column(
                    4,
                    shiny::selectizeInput(
                        inputId = ns("dimZ"),
                        label = shiny::tags$span(
                            "Dim.Z ",
                            bslib::tooltip(
                                bsicons::bs_icon("info-circle", class = "text-muted"),
                                "Select the dimension for the z-axis of the Biplot."
                            )
                        ),
                        choices = c("Dim.1", "Dim.2", "Dim.3"),
                        selected = "Dim.3"
                    )
                )
            ),
            shiny::tags$hr(),
            # Plot dimensions
            shiny::fluidRow(
                shiny::column(
                    6,
                    shiny::numericInput(
                        inputId = ns("width"),
                        label = shiny::tags$span(
                            "Width (cm) ",
                            bslib::tooltip(
                                bsicons::bs_icon("info-circle", class = "text-muted"),
                                "Set the width of the plot in centimeters for export. A value of 16 cm of width correlates with the page width in typical Microsoft Word documents."
                            )
                        ),
                        value = 8,
                        min = 1,
                        max = 50
                    )
                ),
                shiny::column(
                    6,
                    shiny::numericInput(
                        inputId = ns("height"),
                        label = shiny::tags$span(
                            "Height (cm) ",
                            bslib::tooltip(
                                bsicons::bs_icon("info-circle", class = "text-muted"),
                                "Set the height of the plot in centimeters for export. In combination with a width of 16 cm, a good value could be 10 cm. That makes a nice ratio."
                            )
                        ),
                        value = 8,
                        min = 1,
                        max = 50
                    )
                )
            ),
            # Show title checkbox
            shiny::checkboxInput(
                inputId = ns("title"),
                label = "Show Title",
                value = TRUE
            )
        )
    )
}
