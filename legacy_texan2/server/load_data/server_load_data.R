# Server module for the "Load Data" tab panel
#
# This module handles file uploads (CSV/XLSX) and displays data previews.
# It serves as the entry point for data into the application.
#
# @param id Character string. The module namespace ID, must match the ID used
#   in the corresponding UI function call (e.g., UI_load_data("load_data_id"))
#
# @return A reactive expression containing the loaded data frame, or NULL if
#   no data is loaded. This reactive is consumed by downstream modules.
#
# @details
# ## Shiny Module Architecture
# 
# The `moduleServer()` function provides three implicit objects:
# - `input`: A reactive list containing all UI inputs namespaced to this module.
#     For example, `input$data_file` corresponds to the fileInput with
#     inputId = ns("data_file") defined in `R/ui/load_data/ui_load_data.R`.
# - `output`: A reactive list for rendering outputs (tables, plots, etc.)
# - `session`: The Shiny session object, used for namespace management via `session$ns`
#
#' These objects are scoped to this module's namespace, meaning `input$data_file`
#' here only sees the input from UI_load_data(), not from other modules.
#'
#' ## Component Architecture
#' 
#' This module uses explicit dependency injection for its sub-components.
#' Each component function receives the specific objects it needs as parameters,
#' rather than relying on implicit scoping. See `docs/architecture/explicit_dependency_injection.md`.
#'
#' @export
# Import sub-modules using box
box::use(./file_upload)
box::use(./data_preview)
box::use(./missing_values_plot)
box::use(./data_summary)

#' @export
server_load_data <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # `input`, `output`, `session` are provided by moduleServer() - see @details above
    ns <- session$ns

    # Shared reactive value for loaded data
    loaded_data <- shiny::reactiveVal(NULL)
    
    # Data version counter - increments each time new data is loaded
    # Downstream modules can observe this to reset their state
    data_version <- shiny::reactiveVal(0)

    # Conditional main content rendering
    output$main_content <- shiny::renderUI({
      if (is.null(loaded_data())) {
        # Welcome screen when no data is loaded
        shiny::tagList(
          shiny::includeMarkdown("docs/load_data/welcome.md"),
          shiny::tags$div(
            class = "welcome-image",
            style = "max-width: 100%; text-align: center; margin-top: 20px;",
            shiny::tags$img(
              src = "www/images/Beckr_intro_Diamant.png",
              alt = "Example image from a 3D-ST Scan.",
              style = "max-width: 100%; height: auto; background: transparent;"
            )
          )
        )
      } else {
        # Data panels when data is loaded - using collapsible accordion panels
        # Accordion allows multiple panels to be open/closed independently
        bslib::accordion(
          id = ns("data_panels_accordion"),
          open = "data_preview",
          multiple = TRUE,
          bslib::accordion_panel(
            title = "Data Preview",
            value = "data_preview",
            icon = bsicons::bs_icon("table"),
            shiny::includeMarkdown("docs/load_data/data_preview.md"),
            shiny::div(
              class = "table-responsive",
              DT::dataTableOutput(ns("data_preview"))
            )
          ),
          bslib::accordion_panel(
            title = "Missing Values",
            value = "missing_values",
            icon = bsicons::bs_icon("bar-chart"),
            shiny::includeMarkdown("docs/load_data/missing_values.md"),
            shiny::plotOutput(ns("missing_values_plot"), height = "800px")
          ),
          bslib::accordion_panel(
            title = "Data Summary",
            value = "data_summary",
            icon = bsicons::bs_icon("list-ul"),
            shiny::includeMarkdown("docs/load_data/data_summary.md"),
            shiny::htmlOutput(ns("data_summary"))
          )
        )
      }
    })
    
    # Initialize modular components with explicit dependency injection.
    # We pass the module's `input` object so the component can reactively
    # access inputs defined in ui_load_data.R (e.g., input$data_file, input$csv_delimiter).
    # See function documentation in R/server/load_data/file_upload.R
    file_upload$handle_file_upload(
      input = input,  # Module input object from moduleServer() above
      loaded_data = loaded_data,
      session = session,  # For showing column validation modal
      data_version = data_version  # Increment on new data load
    )
    
    # Data preview renderer - requires output object and loaded data
    data_preview$render_data_preview(
      output = output,
      output_id = "data_preview",
      loaded_data = loaded_data
    )
    
    # Missing values plot renderer - requires output object and loaded data
    missing_values_plot$render_missing_values_plot(
      output = output,
      output_id = "missing_values_plot",
      loaded_data = loaded_data
    )
    
    # Data summary renderer - requires output object and loaded data
    data_summary$render_data_summary(
      output = output,
      output_id = "data_summary",
      loaded_data = loaded_data
    )

    # Return list with data and version for downstream modules
    # Modules should observe data_version to reset state on new data load
    list(
      data = shiny::reactive({ loaded_data() }),
      version = shiny::reactive({ data_version() })
    )
  })
}
