#' UI for the Summary Statistics page
#'
#' Orchestrates all summary statistics UI components.
#' Uses bslib::layout_sidebar with sidebar controls and main content cards.
#' Summary statistics are always grouped by measurement, with filter options
#' to select grouping columns (defaults to X-axis selection from Plotting tab).
#'
#' @param id Module namespace ID
#' @return A bslib layout_sidebar UI element
#' @export
UI_summary_stats <- function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
        bslib::layout_sidebar(
            fillable = FALSE,  # Allow natural content height, enable page scrolling
            sidebar = bslib::sidebar(
                title = NULL,
                class = "summary-stats-sidebar",
                
                # Instructions
                shiny::tags$div(
                    shiny::tags$p(
                        class = "small text-muted",
                        "Summary statistics for each selected measurement column. ",
                        "Uses the same data filtering, outlier detection, and trimming as the Plotting tab."
                    )
                ),
                
                # Filter options (grouping columns)
                shiny::uiOutput(ns("filter_options_ui")),
                
                # Shapiro-Wilk test checkbox with tooltip on help icon
                shiny::checkboxInput(
                    inputId = ns("shapiro"),
                    label = shiny::tags$span(
                        "Test for Normality ",
                        bslib::tooltip(
                            bsicons::bs_icon("question-circle", class = "text-muted"),
                            "Performs the Shapiro-Wilk normality test for each measurement. A p-value < 0.05 indicates non-normal distribution."
                        )
                    ),
                    value = FALSE
                ),
                
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
