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
    
    # Source modular component functions
    source("R/server/modules/pages/load_data/file_upload.R", local = TRUE)
    source("R/server/modules/pages/load_data/data_preview.R", local = TRUE)
    source("R/server/modules/pages/load_data/missing_values_plot.R", local = TRUE)
    source("R/server/modules/pages/load_data/data_summary.R", local = TRUE)
    
    # Initialize modular components with explicit parameters
    # File upload handler - requires file input and CSV settings
    handle_file_upload(
      data_file_input = input$data_file,
      csv_has_header = input$csv_has_header,
      csv_delimiter = input$csv_delimiter,
      csv_quote = input$csv_quote,
      loaded_data = loaded_data
    )
    
    # Data preview renderer - requires output object and loaded data
    render_data_preview(
      output = output,
      output_id = "data_preview",
      loaded_data = loaded_data
    )
    
    # Missing values plot renderer - requires output object and loaded data
    render_missing_values_plot(
      output = output,
      output_id = "missing_values_plot",
      loaded_data = loaded_data
    )
    
    # Data summary renderer - requires output object and loaded data
    render_data_summary(
      output = output,
      output_id = "data_summary",
      loaded_data = loaded_data
    )

    # Return reactive with loaded data
    shiny::reactive({
      loaded_data()
    })
  })
}
