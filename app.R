# Source utilities first (before modules)
source("R/utils/logging.R")
source("R/utils/error_handling.R")

# Source UI modules
source("R/ui/modules/pages/ui_load_data.R")
source("R/ui/modules/pages/ui_admin.R")

# Source server modules
source("R/server/modules/pages/server_load_data.R")
source("R/server/modules/pages/server_admin.R")

# Load required packages
library(shiny)
library(shinyBS)
library(openxlsx)
library(DT)

# Initialize logging system
init_logging(
  log_dir = "logs",
  log_level = "INFO",
  console_log = TRUE
)

# Log application startup
log_app_startup()

# Setup global error handler (from error_handling.R)
setup_global_error_handler()

app_ui <- shiny::fluidPage(
  # Add custom CSS for better error display
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      .error-details summary {
        cursor: pointer;
        color: #337ab7;
        font-weight: bold;
      }
      .error-details summary:hover {
        text-decoration: underline;
      }
    "))
  ),
  
  shiny::titlePanel("TexAn 2.0"),
  shiny::tabsetPanel(
    shiny::tabPanel("Load Data", UI_load_data("load_data_id")),
    shiny::tabPanel("Median Analysis", "TODO: Add median analysis UI."),
    shiny::tabPanel("Plotting", "TODO: Add plotting UI."),
    shiny::tabPanel("Reporting", "TODO: Add reporting UI."),
    shiny::tabPanel(
      shiny::tagList(shiny::icon("tools"), "Admin"),
      UI_admin("admin_id")
    )
  )
)

app_server <- function(input, output, session) {
  # Setup session logging
  setup_session_logging(session)
  
  # Register module servers
  server_load_data("load_data_id")
  server_admin("admin_id")
  
  # TODO: Register additional module servers.
}

shiny::shinyApp(ui = app_ui, server = app_server)
