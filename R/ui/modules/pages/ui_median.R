UI_median <- function(id) {
    ns <- shiny::NS(id)

    bslib::layout_sidebar(
            sidebar = bslib::sidebar(
                title = "Median Analysis",
                shiny::actionButton(ns("helpButton"), "Help", class = "btn-primary"),
                shiny::h4("Filter Data"),
                shiny::includeMarkdown("docs/median_calculation/MEDIAN_filter.md"),
                shiny::uiOutput(ns("filterData1")),
                shiny::uiOutput(ns("filterData2")),
                shiny::h4("Calculate Median"),
                shiny::includeMarkdown("docs/median_calculation/MEDIAN_instructions.md")
            ),
        DT::dataTableOutput(ns("medianTable")),
        shiny::uiOutput(ns("filteringMessage2"))
    )
}
