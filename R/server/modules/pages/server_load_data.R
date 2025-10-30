server_load_data <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Shared reactive value for loaded data
    loaded_data <- shiny::reactiveVal(NULL)

    # Conditional main content rendering
    output$main_content <- shiny::renderUI({
      if (is.null(loaded_data())) {
        # Welcome screen when no data is loaded
        shiny::tagList(
          shiny::includeMarkdown("docs/load_data/welcome.md"),
          shiny::tags$div(
            style = "max-width: 100%; text-align: center; margin-top: 20px;",
            shiny::tags$img(
              src = "images/Beckr_intro_Diamant.png",
              alt = "Example image from a 3D-ST Scan.",
              style = "max-width: 100%; height: auto;"
            )
          )
        )
      } else {
        # Data panels when data is loaded
        shinyBS::bsCollapse(
          id = ns("data_preview_collapse"),
          open = ns("data_preview_panel"),
          shinyBS::bsCollapsePanel(
            title = "Data preview",
            value = ns("data_preview_panel"),
            shiny::includeMarkdown("docs/load_data/data_preview.md"),
            shiny::div(
              class = "table-responsive",
              DT::dataTableOutput(ns("data_preview"))
            )
          ),
          shinyBS::bsCollapsePanel(
            title = "Missing Values",
            value = ns("missing_values_panel"),
            shiny::includeMarkdown("docs/load_data/missing_values.md"),
            shiny::div(
              shiny::plotOutput(ns("missing_values_plot"), height = "800px")
            )
          ),
          shinyBS::bsCollapsePanel(
            title = "Data Summary",
            value = ns("data_summary_panel"),
            shiny::includeMarkdown("docs/load_data/data_summary.md"),
            shiny::div(
              shiny::htmlOutput(ns("data_summary"))
            )
          )
        )
      }
    })
    
    # Source modular components
    # All sourced files have access to: input, output, session, loaded_data
    source("R/server/modules/pages/load_data/file_upload.R", local = TRUE)
    source("R/server/modules/pages/load_data/data_preview.R", local = TRUE)
    source("R/server/modules/pages/load_data/missing_values_plot.R", local = TRUE)
    source("R/server/modules/pages/load_data/data_summary.R", local = TRUE)

    # Return reactive with loaded data
    shiny::reactive({
      loaded_data()
    })
  })
}
