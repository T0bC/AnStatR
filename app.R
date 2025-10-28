source("R/ui/modules/pages/ui_load_data.R")
source("R/server/modules/pages/server_load_data.R")

library(shiny)
library(shinyBS)

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
