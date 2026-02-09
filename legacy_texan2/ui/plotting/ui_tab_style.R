#' Style Tab UI Component
#'
#' Creates the Plot Style tab for the plotting sidebar.
#' Contains accordion panels for points, legend/grid, median/SD, axis, colors, and export.
#'
#' @param ns Namespace function from parent module
#' @return A bslib::nav_panel element
#' @export
create_style_tab <- function(ns) {
    bslib::nav_panel(
        title = bslib::tooltip(
            bsicons::bs_icon("palette", size = "1.2em"),
            "Plot Style"
        ),
        value = "style_tab",
        shiny::tags$div(
            class = "pt-3",
            shiny::h6(class = "text-muted mb-3", "Plot Style"),
            
            bslib::accordion(
                id = ns("style_accordion"),
                open = "points",
                
                create_points_accordion_panel(ns),
                create_legend_grid_accordion_panel(ns),
                create_median_sd_accordion_panel(ns),
                create_axis_accordion_panel(ns),
                create_colors_accordion_panel(ns),
                create_export_accordion_panel(ns)
            )
        )
    )
}


#' Points Accordion Panel
#' @param ns Namespace function
#' @return A bslib::accordion_panel element
#' @export
create_points_accordion_panel <- function(ns) {
    bslib::accordion_panel(
        title = "Points",
        value = "points",
        icon = bsicons::bs_icon("circle-fill"),
        shiny::fluidRow(
            shiny::column(
                4,
                shiny::numericInput(
                    ns("pointSize"), 
                    bslib::tooltip(
                        shiny::span("Size ", bsicons::bs_icon("info-circle", class = "text-muted")),
                        "Size of the plotted points"
                    ),
                    value = 4, min = 1, max = 20
                )
            ),
            shiny::column(
                4,
                shiny::numericInput(
                    ns("pointSpread"), 
                    bslib::tooltip(
                        shiny::span("Jitter ", bsicons::bs_icon("info-circle", class = "text-muted")),
                        "Amount of horizontal spread (jittering) applied to points"
                    ),
                    value = 0.15, step = 0.05, min = 0, max = 2
                )
            ),
            shiny::column(
                4,
                shiny::numericInput(
                    ns("transparency"), 
                    bslib::tooltip(
                        shiny::span("Alpha ", bsicons::bs_icon("info-circle", class = "text-muted")),
                        "Transparency: 0 = fully transparent, 1 = fully opaque"
                    ),
                    value = 0.6, step = 0.05, min = 0, max = 1
                )
            )
        ),
        shiny::fluidRow(
            shiny::column(
                6,
                shiny::selectizeInput(
                    ns("pointShape"), 
                    bslib::tooltip(
                        shiny::span("Shape by ", bsicons::bs_icon("info-circle", class = "text-muted")),
                        "Column(s) to determine point shapes (max 6 unique combinations)"
                    ),
                    choices = NULL,
                    multiple = TRUE,
                    options = list(placeholder = "None", maxItems = 3)
                )
            ),
            shiny::column(
                6,
                shiny::selectizeInput(
                    ns("pointColor"), 
                    bslib::tooltip(
                        shiny::span("Color by ", bsicons::bs_icon("info-circle", class = "text-muted")),
                        "Column(s) to determine point colors"
                    ),
                    choices = NULL,
                    multiple = TRUE,
                    options = list(placeholder = "X-Axis default")
                )
            )
        )
    )
}


#' Legend & Grid Accordion Panel
#' @param ns Namespace function
#' @return A bslib::accordion_panel element
#' @export
create_legend_grid_accordion_panel <- function(ns) {
    bslib::accordion_panel(
        title = "Legend & Grid",
        value = "legend_grid",
        icon = bsicons::bs_icon("grid-3x3"),
        shiny::selectInput(
            ns("legendPosition"), 
            "Legend Position",
            choices = c("none", "right", "top", "bottom", "left"),
            selected = "none"
        ),
        shiny::fluidRow(
            shiny::column(
                6,
                shiny::checkboxGroupInput(
                    ns("gridOptions"),
                    "Grid Lines",
                    choices = c(
                        "Horizontal" = "hGrid",
                        "Vertical" = "vGrid",
                        "Top/Right" = "topRightBorders"
                    ),
                    selected = c("hGrid", "vGrid", "topRightBorders")
                )
            ),
            shiny::column(
                6,
                shiny::checkboxGroupInput(
                    ns("statOptions"),
                    "Statistics",
                    choices = c(
                        "Median" = "showMedian",
                        "SD" = "showSD",
                        "Aspect Ratio" = "aspectRatio"
                    ),
                    selected = c("showMedian", "showSD")
                )
            )
        )
    )
}


#' Median & SD Lines Accordion Panel
#' @param ns Namespace function
#' @return A bslib::accordion_panel element
#' @export
create_median_sd_accordion_panel <- function(ns) {
    bslib::accordion_panel(
        title = "Median & SD Lines",
        value = "median_sd",
        icon = bsicons::bs_icon("dash-lg"),
        shiny::fluidRow(
            shiny::column(
                6,
                shiny::numericInput(
                    ns("medianThickness"), 
                    "Median Thickness",
                    value = 0.5, min = 0.1, max = 5, step = 0.1
                )
            ),
            shiny::column(
                6,
                shiny::numericInput(
                    ns("medianWidth"), 
                    "Median Width",
                    value = 0.15, min = 0.1, max = 1, step = 0.1
                )
            )
        ),
        shiny::fluidRow(
            shiny::column(
                6,
                shiny::numericInput(
                    ns("sdThickness"), 
                    "SD Thickness",
                    value = 0.5, min = 0.1, max = 5, step = 0.1
                )
            ),
            shiny::column(
                6,
                shiny::numericInput(
                    ns("sdWidth"), 
                    "SD Width",
                    value = 0.15, min = 0.1, max = 1, step = 0.1
                )
            )
        )
    )
}


#' Axis Settings Accordion Panel
#' @param ns Namespace function
#' @return A bslib::accordion_panel element
#' @export
create_axis_accordion_panel <- function(ns) {
    bslib::accordion_panel(
        title = "Axis Settings",
        value = "axis",
        icon = bsicons::bs_icon("arrows-angle-expand"),
        shiny::fluidRow(
            shiny::column(
                6,
                shiny::numericInput(
                    ns("axisTickLength"), 
                    "Tick Length",
                    value = 0.15, min = 0.1, max = 1, step = 0.1
                )
            ),
            shiny::column(
                6,
                shiny::numericInput(
                    ns("axisLineThickness"), 
                    "Line Thickness",
                    value = 0.5, min = 0.1, max = 5, step = 0.1
                )
            )
        )
    )
}


#' Colors Accordion Panel
#' @param ns Namespace function
#' @return A bslib::accordion_panel element
#' @export
create_colors_accordion_panel <- function(ns) {
    bslib::accordion_panel(
        title = "Custom Colors",
        value = "colors",
        icon = bsicons::bs_icon("palette"),
        # Dynamic color pickers rendered by server
        shiny::uiOutput(ns("colorPickers"))
    )
}


#' Export Settings Accordion Panel
#' @param ns Namespace function
#' @return A bslib::accordion_panel element
#' @export
create_export_accordion_panel <- function(ns) {
    bslib::accordion_panel(
        title = "Export Settings",
        value = "export",
        icon = bsicons::bs_icon("download"),
        shiny::fluidRow(
            shiny::column(
                6,
                shiny::numericInput(
                    ns("exportWidth"),
                    bslib::tooltip(
                        shiny::span("Width (cm) ", bsicons::bs_icon("info-circle", class = "text-muted")),
                        "Plot width in cm for SVG export. 16 cm fits typical Word documents."
                    ),
                    value = 16, min = 1, max = 50
                )
            ),
            shiny::column(
                6,
                shiny::numericInput(
                    ns("exportHeight"),
                    bslib::tooltip(
                        shiny::span("Height (cm) ", bsicons::bs_icon("info-circle", class = "text-muted")),
                        "Plot height in cm for SVG export. 10 cm with 16 cm width gives a nice ratio."
                    ),
                    value = 10, min = 1, max = 50
                )
            )
        ),
        shiny::tags$p(
            class = "small text-muted mt-2",
            "Use the download button on each plot card to export as SVG."
        )
    )
}
