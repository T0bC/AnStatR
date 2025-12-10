#' UI for the Summary Statistics page
#'
#' Orchestrates all summary statistics UI components.
#' Uses bslib::layout_sidebar with sidebar controls and main content cards.
#'
#' @param id Module namespace ID
#' @return A bslib layout_sidebar UI element
UI_summary_stats <- function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
        bslib::layout_sidebar(
            sidebar = bslib::sidebar(
                title = NULL,
                class = "summary-stats-sidebar",
                
                # Instructions
                shiny::tags$div(
                    class = "mb-3",
                    shiny::tags$h6(
                        bsicons::bs_icon("info-circle"),
                        " Instructions"
                    ),
                    shiny::tags$p(
                        class = "small text-muted",
                        "Calculate summary statistics for your measurement columns. ",
                        "Choose how to sort/group the results and optionally test for normality."
                    )
                ),
                
                shiny::tags$hr(),
                
                # Shapiro-Wilk test checkbox
                shiny::checkboxInput(
                    inputId = ns("shapiro"),
                    label = shiny::tags$span(
                        "Test for Normality",
                        bsicons::bs_icon("question-circle", class = "ms-1 text-muted")
                    ),
                    value = FALSE
                ),
                bslib::tooltip(
                    shiny::tags$span(id = ns("shapiro_tooltip")),
                    paste0(
                        "Include Shapiro-Wilk test for normality. ",
                        "If p-value < 0.05, data is not normally distributed. ",
                        "Test is performed for each measurement column."
                    )
                ),
                
                shiny::tags$hr(),
                
                # Sorting options
                shiny::selectizeInput(
                    inputId = ns("sorting_options"),
                    label = "Sort by:",
                    choices = c("Measurement"),
                    selected = "Measurement",
                    multiple = TRUE
                ),
                
                # Filter options (dynamically shown)
                shiny::uiOutput(ns("filter_options_ui")),
                
                shiny::tags$hr(),
                
                # Download all button
                shiny::downloadButton(
                    outputId = ns("download_all"),
                    label = "Download All Tables",
                    class = "btn-primary btn-sm w-100"
                )
            ),
            
            # Main content area - summary tables rendered here
            shiny::uiOutput(ns("summary_tables"))
        )
    )
}
