server_load_data <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Shared reactive value for loaded data
    loaded_data <- shiny::reactiveVal(NULL)

    # Source modular components
    # All sourced files have access to: input, output, session, loaded_data
    source("R/server/modules/pages/load_data/excel_upload.R", local = TRUE)
    source("R/server/modules/pages/load_data/data_preview.R", local = TRUE)
    source("R/server/modules/pages/load_data/missing_values_plot.R", local = TRUE)
    source("R/server/modules/pages/load_data/data_summary.R", local = TRUE)

    # Return reactive with loaded data
    shiny::reactive({
      loaded_data()
    })
  })
}
