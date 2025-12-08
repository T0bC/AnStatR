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
                        
                        # Point Settings
                        bslib::accordion(
                            id = ns("style_accordion"),
                            open = "points",
                            
                            # === Points Section ===
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
                                                "Column to determine point shapes"
                                            ),
                                            choices = NULL,
                                            multiple = FALSE,
                                            options = list(placeholder = "None")
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
                            ),
                            
                            # === Legend & Grid Section ===
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
                                shiny::checkboxGroupInput(
                                    ns("gridOptions"),
                                    "Grid Lines",
                                    choices = c(
                                        "Horizontal Grid" = "hGrid",
                                        "Vertical Grid" = "vGrid",
                                        "Top/Right Borders" = "topRightBorders"
                                    ),
                                    selected = c("hGrid", "vGrid", "topRightBorders")
                                ),
                                shiny::checkboxGroupInput(
                                    ns("statOptions"),
                                    "Statistics",
                                    choices = c(
                                        "Show Median" = "showMedian",
                                        "Show SD" = "showSD",
                                        "Fixed Aspect Ratio" = "aspectRatio"
                                    ),
                                    selected = c("showMedian", "showSD")
                                )
                            ),
                            
                            # === Median & SD Lines Section ===
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
                            ),
                            
                            # === Axis Settings Section ===
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
                            ),
                            
                            # === Colors Section ===
                            bslib::accordion_panel(
                                title = "Custom Colors",
                                value = "colors",
                                icon = bsicons::bs_icon("palette"),
                                # Dynamic color pickers rendered by server
                                shiny::uiOutput(ns("colorPickers"))
                            ),
                            
                            # === Export Settings Section ===
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
                        )
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
