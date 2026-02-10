#' UI for the PCA page
#'
#' Orchestrates all PCA UI components.
#' Tab components are sourced from R/ui/pca/
#'
#' @param id Module namespace ID
#' @return A bslib layout_sidebar UI element
#' @export
UI_pca <- function(id) {
    ns <- shiny::NS(id)
    
    # Import UI tab components using box
    box::use(./ui_tab_data_selection)
    box::use(./ui_tab_plotting_controls)
    box::use(./ui_tab_actions)

    shiny::tagList(
        # Initialize window size reporting with namespaced IDs
        shiny::tags$script(shiny::HTML(sprintf(
            "$(document).on('shiny:connected', function() { initializeWindowSize('%s', '%s'); });",
            ns("pca_results"),
            ns("windowSizePCA")
        ))),
        bslib::layout_sidebar(
            sidebar = bslib::sidebar(
                title = NULL,
                class = "pca-sidebar",
                
                # Horizontal tabs with icons only
                bslib::navset_tab(
                    id = ns("sidebar_tabs"),
                    ui_tab_data_selection$create_pca_data_selection_tab(ns),
                    ui_tab_plotting_controls$create_pca_plotting_controls_tab(ns),
                    ui_tab_actions$create_pca_actions_tab(ns)
                ),
                
                # Compute PCA button at bottom (always visible)
                shiny::tags$hr(),
                shiny::actionButton(
                    inputId = ns("compute_pca_button"),
                    label = "Compute PCA",
                    class = "btn-primary btn-sm w-100"
                )
            ),

            # Main content area - PCA results will be rendered here
            shiny::uiOutput(ns("pca_results"))
        )
    )
}
