#' UI for the Plotting page
#'
#' @param id Module namespace ID
#' @return A bslib layout_sidebar UI element
UI_plotting <- function(id) {
    ns <- shiny::NS(id)

    shiny::tagList(
        # Initialize window size reporting with namespaced IDs
        shiny::tags$script(shiny::HTML(sprintf(
            "$(document).on('shiny:connected', function() { initializeWindowSize('%s', '%s'); });",
            ns("plots"),
            ns("windowSize")
        ))),
        bslib::layout_sidebar(
        sidebar = bslib::sidebar(
            title = NULL,  # No title - tabs serve as navigation
            class = "plotting-sidebar",
            
            # Horizontal tabs with icons only
            bslib::navset_tab(
                id = ns("sidebar_tabs"),
                
                # ===== TAB 1: Data Selection =====
                bslib::nav_panel(
                    title = bslib::tooltip(
                        bsicons::bs_icon("table", size = "1.2em"),
                        "Data Selection"
                    ),
                    value = "data_tab",
                    shiny::tags$div(
                        class = "pt-3",
                        shiny::h6(class = "text-muted mb-3", "Data Selection"),
                        # Descriptive columns
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
                        # Measurement columns
                        shiny::selectizeInput(
                            inputId = ns("measureVar"),
                            label = shiny::tags$span(
                                "Measurement columns ",
                                bslib::tooltip(
                                    bsicons::bs_icon("info-circle", class = "text-muted"),
                                    "Select columns containing measurements to plot. One plot per column."
                                )
                            ),
                            choices = NULL,
                            multiple = TRUE,
                            options = list(placeholder = "Select measurement columns...")
                        ),
                        shiny::tags$hr(),
                        # X-Axis and Tooltip in a row
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
                ),
                
                # ===== TAB 2: Filter =====
                bslib::nav_panel(
                    title = bslib::tooltip(
                        bsicons::bs_icon("funnel", size = "1.2em"),
                        "Filter Data"
                    ),
                    value = "filter_tab",
                    shiny::tags$div(
                        class = "pt-3",
                        shiny::h6(class = "text-muted mb-3", "Filter Data"),
                        # Hide from filter option
                        shiny::selectizeInput(
                            inputId = ns("hideCols"),
                            label = shiny::tags$span(
                                "Hide columns ",
                                bslib::tooltip(
                                    bsicons::bs_icon("info-circle", class = "text-muted"),
                                    "Hide selected descriptive columns from filtering but keep them for tooltips."
                                )
                            ),
                            choices = NULL,
                            multiple = TRUE,
                            options = list(placeholder = "Optional...")
                        ),
                        shiny::tags$hr(),
                        # Filter checkboxes
                        shiny::uiOutput(ns("checkboxes"))
                    )
                ),
                
                # ===== TAB 3: Processing =====
                bslib::nav_panel(
                    title = bslib::tooltip(
                        bsicons::bs_icon("sliders", size = "1.2em"),
                        "Data Processing"
                    ),
                    value = "processing_tab",
                    shiny::tags$div(
                        class = "pt-3",
                        shiny::h6(class = "text-muted mb-3", "Data Processing"),
                        # Trimming
                        shiny::tags$label(class = "small fw-semibold", "Trimming"),
                        shiny::sliderInput(
                            inputId = ns("trim_slider"),
                            label = shiny::tags$span(
                                "Trim % ",
                                bslib::tooltip(
                                    bsicons::bs_icon("info-circle", class = "text-muted"),
                                    "Remove a percentage of highest and lowest values to reduce outlier impact."
                                )
                            ),
                            min = 0,
                            max = 100,
                            value = 0,
                            step = 1
                        ),
                        shiny::tags$hr(),
                        # Outlier Detection
                        shiny::tags$label(class = "small fw-semibold", "Outlier Detection"),
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
                            # Detection method radio buttons with tooltips
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
                            ),
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
                        )
                    )
                ),
                
                # ===== TAB 4: Plot Options =====
                bslib::nav_panel(
                    title = bslib::tooltip(
                        bsicons::bs_icon("palette", size = "1.2em"),
                        "Plot Style"
                    ),
                    value = "style_tab",
                    shiny::tags$div(
                        class = "pt-3",
                        shiny::h6(class = "text-muted mb-3", "Plot Style"),
                        shiny::tags$p(class = "text-muted small fst-italic", 
                            "Plot styling options coming soon...")
                        # Future: titles, legends, colors, etc.
                    )
                )
            ),
            
            # Download button at bottom (always visible)
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
    )
}
