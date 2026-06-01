box::use(
  bsicons,
  bslib,
  shiny,
)

#' Create a sidebar tab (nav_panel) with an icon-only tooltip title
#'
#' @param icon Character, Bootstrap icon name
#' @param tooltip_text Character, tooltip text shown on hover
#' @param value Character, unique value for this tab
#' @param ... UI elements to place inside the tab
#' @return A bslib nav_panel element
#' @export
create_tab <- function(icon, tooltip_text, value, ...) {
  bslib$nav_panel(
    title = bslib$tooltip(
      bsicons$bs_icon(icon, size = "1.2em"),
      tooltip_text
    ),
    value = value,
    shiny$tags$div(
      class = "pt-3",
      ...
    )
  )
}

#' Build a standard sidebar + main content layout
#'
#' Every tab module in the app uses the same pattern:
#' left sidebar with icon-only navset_tabs, right main content area.
#' This function generates that layout with a unified CSS class
#' so all sidebar styling is controlled from one place in main.scss.
#'
#' @param ns Namespace function from the calling module
#' @param sidebar_id Character, input ID for the navset_tab
#' @param tabs List of tab panels created via create_tab()
#' @param main_content UI element(s) for the main content area
#' @param action_button Optional action button UI (placed below tabs)
#' @param enable_responsive_plots Logical, if TRUE injects window size
#'   reporting JS for responsive plot sizing
#' @param results_id Character, namespaced output ID for the main content
#'   container (used by window size JS). Only needed when
#'   enable_responsive_plots = TRUE.
#' @return A shiny tagList with layout_sidebar
#' @export
tab_layout <- function(
  ns,
  sidebar_id,
  tabs,
  main_content,
  action_button = NULL,
  enable_responsive_plots = FALSE,
  results_id = NULL
) {
  # Build navset_tab from the list of tabs
  navset_args <- c(
    list(id = ns(sidebar_id)),
    tabs
  )
  sidebar_navset <- do.call(bslib$navset_tab, navset_args)

  # Build sidebar content: navset + optional action button

  sidebar_elements <- list(sidebar_navset)
  if (!is.null(action_button)) {
    sidebar_elements <- c(
      sidebar_elements,
      list(shiny$tags$hr(), action_button)
    )
  }

  # Build the sidebar
  sidebar_ui <- do.call(
    bslib$sidebar,
    c(
      list(title = NULL, class = "anstatr-sidebar"),
      sidebar_elements
    )
  )

  # Build the layout
  layout <- bslib$layout_sidebar(
    sidebar = sidebar_ui,
    main_content
  )

  # Optionally wrap with window size JS
  if (enable_responsive_plots && !is.null(results_id)) {
    shiny$tagList(
      window_size_script(ns, results_id),
      layout
    )
  } else {
    layout
  }
}

#' Generate the window size reporting script tag
#'
#' Injects JS that calls initializeWindowSize() on shiny:connected.
#' The initializeWindowSize function must be defined in app/js/index.js.
#'
#' @param ns Namespace function
#' @param results_id Character, the un-namespaced output ID for the
#'   main content container
#' @return A shiny tags$script element
#' @export
window_size_script <- function(ns, results_id) {
  shiny$tags$script(shiny$HTML(sprintf(
    paste0(
      "$(document).on('shiny:connected', function() {",
      " initializeWindowSize('%s', '%s');",
      " });"
    ),
    ns(results_id),
    ns("windowSize")
  )))
}
