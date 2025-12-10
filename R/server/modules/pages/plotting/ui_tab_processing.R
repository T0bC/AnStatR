#' Processing Tab UI Component
#'
#' Creates the Processing tab for the plotting sidebar.
#' Contains outlier detection and trimming options.
#'
#' @param ns Namespace function from parent module
#' @return A bslib::nav_panel element
create_processing_tab <- function(ns) {
    bslib::nav_panel(
        title = bslib::tooltip(
            bsicons::bs_icon("sliders", size = "1.2em"),
            "Data Processing"
        ),
        value = "processing_tab",
        shiny::tags$div(
            class = "pt-3",
            shiny::h6(class = "text-muted mb-3", "Data Processing"),
            
            # Outlier Detection (first - outliers are excluded before trimming)
            shiny::tags$label(class = "small fw-semibold", "1. Outlier Detection"),
            shiny::checkboxInput(
                inputId = ns("enableOutlierDetection"),
                label = shiny::tags$span(
                    "Enable ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        "Univariate outlier detection - removes outliers per measurement column independently."
                    )
                ),
                value = FALSE
            ),
            
            # Outlier options (shown when enabled)
            shiny::conditionalPanel(
                condition = "input.enableOutlierDetection == true",
                ns = ns,
                create_outlier_method_radio(ns),
                # Factor slider (for factor-based methods)
                shiny::conditionalPanel(
                    condition = "['IQR', 'zscore', 'modified_zscore', 'adjusted_boxplot', 'bootstrap'].includes(input.detectOutlier)",
                    ns = ns,
                    shiny::sliderInput(
                        inputId = ns("standardFactor"),
                        label = "Factor:",
                        value = 1.5, min = 0.5, max = 10, step = 0.1
                    )
                ),
                # Probability slider (for probability-based methods)
                shiny::conditionalPanel(
                    condition = "['kde', 'isolation_forest', 'lof'].includes(input.detectOutlier)",
                    ns = ns,
                    shiny::sliderInput(
                        inputId = ns("probabilityFactor"),
                        label = "Threshold:",
                        value = 0.05, min = 0.05, max = 1, step = 0.05
                    )
                ),
                # Bootstrap samples
                shiny::conditionalPanel(
                    condition = "input.detectOutlier == 'bootstrap'",
                    ns = ns,
                    shiny::numericInput(
                        inputId = ns("bootstrapSamples"),
                        label = "Samples:",
                        value = 1000, min = 100, max = 10000, step = 100
                    )
                )
            ),
            
            shiny::tags$hr(),
            
            # Trimming (second - applied to non-outlier data, used by WRS2)
            shiny::tags$label(class = "small fw-semibold", "2. Trimming"),
            shiny::sliderInput(
                inputId = ns("trim_slider"),
                label = shiny::tags$span(
                    "Trim % ",
                    bslib::tooltip(
                        bsicons::bs_icon("info-circle", class = "text-muted"),
                        "Percentage trimmed from each end for robust statistics (WRS2). Applied after outlier removal."
                    )
                ),
                min = 0,
                max = 50,
                value = 0,
                step = 1
            )
        )
    )
}


#' Create Outlier Method Radio Buttons
#'
#' Helper function to create the outlier detection method radio buttons.
#'
#' @param ns Namespace function from parent module
#' @return A div containing radio button inputs
create_outlier_method_radio <- function(ns) {
    shiny::tags$div(
        class = "form-group shiny-input-radiogroup shiny-input-container",
        id = ns("detectOutlier"),
        shiny::tags$label(class = "control-label small", `for` = ns("detectOutlier"), "Method:"),
        shiny::tags$div(
            class = "shiny-options-group",
            # IQR
            shiny::tags$div(class = "radio", shiny::tags$label(
                shiny::tags$input(type = "radio", name = ns("detectOutlier"), value = "IQR", checked = "checked"),
                shiny::tags$span("IQR ", bslib::tooltip(bsicons::bs_icon("info-circle", class = "text-muted small"), "Tukey's method. Best for symmetric data. Factor: 1.5-3.0"))
            )),
            # Z-Score
            shiny::tags$div(class = "radio", shiny::tags$label(
                shiny::tags$input(type = "radio", name = ns("detectOutlier"), value = "zscore"),
                shiny::tags$span("Z-Score ", bslib::tooltip(bsicons::bs_icon("info-circle", class = "text-muted small"), "Mean/SD method. Best for normal data. Factor: 2.0-3.0"))
            )),
            # Modified Z-Score
            shiny::tags$div(class = "radio", shiny::tags$label(
                shiny::tags$input(type = "radio", name = ns("detectOutlier"), value = "modified_zscore"),
                shiny::tags$span("Modified Z-Score ", bslib::tooltip(bsicons::bs_icon("info-circle", class = "text-muted small"), "Median/MAD method. Robust for skewed data. Factor: 3.5-4.5"))
            )),
            # Adjusted Boxplot
            shiny::tags$div(class = "radio", shiny::tags$label(
                shiny::tags$input(type = "radio", name = ns("detectOutlier"), value = "adjusted_boxplot"),
                shiny::tags$span("Adjusted Boxplot ", bslib::tooltip(bsicons::bs_icon("info-circle", class = "text-muted small"), "Skewness-adjusted IQR. Factor: 1.5-3.0"))
            )),
            # KDE
            shiny::tags$div(class = "radio", shiny::tags$label(
                shiny::tags$input(type = "radio", name = ns("detectOutlier"), value = "kde"),
                shiny::tags$span("KDE ", bslib::tooltip(bsicons::bs_icon("info-circle", class = "text-muted small"), "Kernel Density. Best for multimodal data. Threshold: 0.05-0.2"))
            )),
            # Isolation Forest
            shiny::tags$div(class = "radio", shiny::tags$label(
                shiny::tags$input(type = "radio", name = ns("detectOutlier"), value = "isolation_forest"),
                shiny::tags$span("Isolation Forest ", bslib::tooltip(bsicons::bs_icon("info-circle", class = "text-muted small"), "Tree-based. Best for large datasets. Threshold: 0.05-0.2"))
            )),
            # LOF
            shiny::tags$div(class = "radio", shiny::tags$label(
                shiny::tags$input(type = "radio", name = ns("detectOutlier"), value = "lof"),
                shiny::tags$span("LOF ", bslib::tooltip(bsicons::bs_icon("info-circle", class = "text-muted small"), "Local Outlier Factor. Best for density clusters. Threshold: 0.05-0.2"))
            )),
            # Bootstrap
            shiny::tags$div(class = "radio", shiny::tags$label(
                shiny::tags$input(type = "radio", name = ns("detectOutlier"), value = "bootstrap"),
                shiny::tags$span("Bootstrap ", bslib::tooltip(bsicons::bs_icon("info-circle", class = "text-muted small"), "Resampling method. Best for small samples. Samples: 1000-10000"))
            ))
        )
    )
}
