#' UI for the PCA page
#'
#' Orchestrates all PCA UI components.
#' Tab components are sourced from R/server/modules/pages/pca/
#'
#' @param id Module namespace ID
#' @return A bslib layout_sidebar UI element
UI_pca <- function(id) {
    ns <- shiny::NS(id)
    
    # Source UI tab components
    source("R/server/modules/pages/pca/ui_tab_data_selection.R", local = TRUE)
    source("R/server/modules/pages/pca/ui_tab_plotting_controls.R", local = TRUE)
    source("R/server/modules/pages/pca/ui_tab_actions.R", local = TRUE)

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
                    create_pca_data_selection_tab(ns),
                    create_pca_plotting_controls_tab(ns),
                    create_pca_actions_tab(ns)
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
