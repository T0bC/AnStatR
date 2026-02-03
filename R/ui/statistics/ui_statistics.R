#' UI for the Statistics page
#'
#' Orchestrates all statistics UI components.
#' Tab components are sourced from R/ui/statistics/
#'
#' @param id Module namespace ID
#' @return A bslib layout_sidebar UI element
#' @export
UI_statistics <- function(id) {
    ns <- shiny::NS(id)
    
    # Import UI tab components using box
    box::use(./ui_tab_options)
    box::use(./ui_tab_bootstrap)
    box::use(./ui_tab_adjustments)

    shiny::tagList(
        bslib::layout_sidebar(
            fillable = FALSE,  # Allow natural content height, enable page scrolling
            sidebar = bslib::sidebar(
                title = NULL,
                class = "statistics-sidebar",
                
                # Horizontal tabs with icons only
                bslib::navset_tab(
                    id = ns("sidebar_tabs"),
                    ui_tab_options$create_options_tab(ns),
                    ui_tab_bootstrap$create_bootstrap_tab(ns),
                    ui_tab_adjustments$create_adjustments_tab(ns)
                ),
                
                # Compute button at bottom (always visible)
                shiny::tags$hr(),
                shiny::actionButton(
                    inputId = ns("compute_button"),
                    label = "Compute Statistics",
                    class = "btn-primary btn-sm w-100",
                    icon = shiny::icon("calculator")
                ),
                shiny::tags$div(
                    class = "small text-muted mt-2",
                    "Computation may take some time. Check the console for progress."
                )
            ),

            # Main content area - results will be rendered here
            shiny::uiOutput(ns("statistics_output"))
        )
    )
}
