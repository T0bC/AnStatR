#' @export
UI_median <- function(id) {
    ns <- shiny::NS(id)

    bslib::layout_sidebar(
            sidebar = bslib::sidebar(
                title = "Data Filtering",
                shiny::actionButton(ns("helpButton"), "Help", class = "btn-primary btn-sm"),
                
                # Grouping section
                shiny::tags$hr(),
                shiny::h5("1. Define Sample Structure"),
                shiny::uiOutput(ns("grouping_ui")),
                
                # Quality filter section
                shiny::tags$hr(),
                shiny::h5("2. Quality Filtering"),
                shiny::uiOutput(ns("quality_filter_ui"))
                
                # Median calculation section (placeholder for now)
                # shiny::tags$hr(),
                # shiny::h5("3. Calculate Median")
            ),
        # Main content area
        shiny::uiOutput(ns("filteringMessage")),
        DT::dataTableOutput(ns("medianTable"))
    )
}
