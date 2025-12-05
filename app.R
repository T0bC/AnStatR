# Source UI modules
source("R/ui/modules/pages/ui_load_data.R")
source("R/ui/modules/pages/ui_median.R")

# Source server modules
source("R/server/modules/pages/server_load_data.R")
source("R/server/modules/pages/server_median.R")

# Load required packages
library(shiny)
library(bslib)
library(openxlsx)
library(DT)
library(DataExplorer)
library(summarytools)

# Prevent Rplots.pdf creation by setting default PDF device to null
# This avoids file clutter when DataExplorer or other packages open a default device
pdf(NULL)

# Register www folder for static resources
shiny::addResourcePath("www", "www")

app_ui <- bslib::page_navbar(
  id = "active_page",
  title = "TexAn 2.0",
  bg = "#336699",
  inverse = TRUE,
  header = shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/css/styles.css")
  ),
  bslib::nav_panel(title = "Load Data", UI_load_data("load_data_id")),
  bslib::nav_panel(title = "Median Analysis", UI_median("median_id")),
  bslib::nav_panel(title = "Plotting", shiny::p("TODO: Add plotting UI.")),
  bslib::nav_panel(title = "Reporting", shiny::p("TODO: Add reporting UI."))
)

app_server <- function(input, output, session) {
  # Register module servers
  loaded_data <- server_load_data("load_data_id")
  server_median("median_id", loaded_data = loaded_data)

  # TODO: Register additional module servers.
}

shiny::shinyApp(ui = app_ui, server = app_server)
