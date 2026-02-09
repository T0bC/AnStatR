#' UI for the Plotting page
#'
#' Orchestrates all plotting UI components.
#' Tab components are sourced from R/ui/plotting/
#'
#' @param id Module namespace ID
#' @return A bslib layout_sidebar UI element
#' @export
UI_plotting <- function(id) {
    ns <- shiny::NS(id)
    
    # Import UI tab components using box
    box::use(./ui_tab_data_selection)
    box::use(./ui_tab_filter)
    box::use(./ui_tab_processing)
    box::use(./ui_tab_style)

    shiny::tagList(
        # Initialize window size reporting with namespaced IDs
        shiny::tags$script(shiny::HTML(sprintf(
            "$(document).on('shiny:connected', function() { initializeWindowSize('%s', '%s'); });",
            ns("plots"),
            ns("windowSize")
        ))),
        bslib::layout_sidebar(
            sidebar = bslib::sidebar(
                title = NULL,
                class = "plotting-sidebar",
                
                # Horizontal tabs with icons only
                bslib::navset_tab(
                    id = ns("sidebar_tabs"),
                    ui_tab_data_selection$create_data_selection_tab(ns),
                    ui_tab_filter$create_filter_tab(ns),
                    ui_tab_processing$create_processing_tab(ns),
                    ui_tab_style$create_style_tab(ns)
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
