UI_median <- function(id) {
    ns <- shiny::NS(id)

    shiny::tagList(
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                shiny::tags$style(shiny::HTML("
          .btn-primary {
            background-color: #336699;
          }
        ")),
                shiny::actionButton(ns("helpButton"), "Help", class = "btn-primary"),
                shiny::h4("Filter Data"),
                shiny::includeMarkdown("docs/median_calculation/MEDIAN_filter.md"),
                shiny::uiOutput(ns("filterData1")),
                shiny::uiOutput(ns("filterData2")),
                shiny::h4("Calculate Median"),
                shiny::includeMarkdown("docs/median_calculation/MEDIAN_instructions.md")
            ),
            shiny::mainPanel(
                DT::dataTableOutput(ns("medianTable")),
                shiny::uiOutput(ns("filteringMessage2"))
            )
        )
    )
}
