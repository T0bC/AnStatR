source_directory <- function(path) {
  files <- list.files(
    path,
    pattern = "\\.[Rr]$",
    recursive = TRUE,
    full.names = TRUE
  )
  invisible(lapply(files, source))
}

helper_dirs <- c("R/global", "R/utils", "R/ui", "R/server")
invisible(lapply(helper_dirs, source_directory))

if (!requireNamespace("shinyBS", quietly = TRUE)) {
  stop("Package 'shinyBS' is required for this app.", call. = FALSE)
}

app_ui <- shiny::fluidPage(
  shiny::titlePanel("TexAn 2.0"),
  shiny::tabsetPanel(
    shiny::tabPanel("Load Data", UI_load_data("load_data_id")),
    shiny::tabPanel("Median Analysis", "TODO: Add median analysis UI."),
    shiny::tabPanel("Plotting", "TODO: Add plotting UI."),
    shiny::tabPanel("Reporting", "TODO: Add reporting UI.")
  )
)

app_server <- function(input, output, session) {
  server_load_data("load_data_id")
  # TODO: Register additional module servers.
}

shiny::shinyApp(ui = app_ui, server = app_server)
