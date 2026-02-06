#' UI for the {TabName} page
#'
#' Orchestrates all {TabName} UI components.
#' Tab components are sourced from R/ui/{tabname}/
#'
#' @param id Module namespace ID
#' @return A bslib layout_sidebar UI element
#'
#' USAGE: Copy this file to R/ui/{tabname}/ui_{tabname}.R
#'        Replace all {tabname} with your tab name (lowercase)
#'        Replace all {TabName} with your tab name (TitleCase)
UI_{tabname} <- function(id) {
    ns <- shiny::NS(id)
    
    # Source UI tab components
    source("R/ui/{tabname}/ui_tab_section1.R", local = TRUE)
    source("R/ui/{tabname}/ui_tab_section2.R", local = TRUE)

    shiny::tagList(
        # Optional: Initialize window size reporting for responsive plots
        shiny::tags$script(shiny::HTML(sprintf(
            "$(document).on('shiny:connected', function() { initializeWindowSize('%s', '%s'); });",
            ns("{tabname}_results"),
            ns("windowSize{TabName}")
        ))),
        bslib::layout_sidebar(
            sidebar = bslib::sidebar(
                title = NULL,
                class = "{tabname}-sidebar",  # CRITICAL: Must match CSS selectors
                
                # Horizontal tabs with icons only
                bslib::navset_tab(
                    id = ns("sidebar_tabs"),
                    create_{tabname}_section1_tab(ns),
                    create_{tabname}_section2_tab(ns)
                ),
                
                # Action button at bottom (always visible)
                shiny::tags$hr(),
                shiny::actionButton(
                    inputId = ns("action_button"),
                    label = "Run Action",
                    class = "btn-primary btn-sm w-100"
                )
            ),

            # Main content area - results will be rendered here
            shiny::uiOutput(ns("{tabname}_results"))
        )
    )
}
