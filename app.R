# Source UI modules
source("R/ui/modules/pages/ui_load_data.R")
source("R/ui/modules/pages/ui_median.R")
source("R/ui/modules/pages/ui_plotting.R")

# Source component modules
source("R/ui/modules/components/settings_modal.R")

# Source server modules
source("R/server/modules/pages/server_load_data.R")
source("R/server/modules/pages/server_median.R")
source("R/server/modules/pages/server_plotting.R")

# Load required packages
library(shiny)
library(bslib)
library(bsicons)

library(openxlsx)
library(DT)
library(DataExplorer)
library(summarytools)

# Prevent Rplots.pdf creation by setting default PDF device to null
# This avoids file clutter when DataExplorer or other packages open a default device
pdf(NULL)

# Register www folder for static resources (needed for proper image/CSS loading)
shiny::addResourcePath("www", "www")

app_ui <- bslib::page_navbar(
  id = "active_page",
  title = "TexAn 2.0",
  theme = get_default_theme(),
  header = shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/css/styles.css"),
    # Include selectize dependencies to fix DT column filter compatibility issue
    htmltools::findDependencies(shiny::selectizeInput("__selectize_dep__", NULL, choices = NULL))
  ),
  bslib::nav_panel(title = "Load Data", value = "load_data", UI_load_data("load_data_id")),
  bslib::nav_panel(title = "Median Analysis", value = "median", UI_median("median_id")),
  bslib::nav_panel(title = "Plotting", value = "plotting", UI_plotting("plotting_id")),
  bslib::nav_panel(title = "Reporting", value = "reporting", shiny::p("TODO: Add reporting UI.")),
  bslib::nav_spacer(),
  bslib::nav_item(
    shiny::actionLink(
      inputId = "settings_btn",
      label = bsicons::bs_icon("gear"),
      title = "Settings"
    )
  )
)

app_server <- function(input, output, session) {
  # Register module servers
  # server_load_data returns list with $data (reactive) and $version (reactive)
  load_data_result <- server_load_data("load_data_id")
  
  # Pass both data and version to downstream modules for state reset on new data
  median_result <- server_median("median_id", 
                loaded_data = load_data_result$data, 
                data_version = load_data_result$version)
  
  # Pass median data to plotting module
  # median_result is a reactive containing the median results
  server_plotting("plotting_id",
                  median_data = median_result,
                  data_version = load_data_result$version)

  # Initialize settings modal
  settings_modal_server(input, session)
  
  # Hide/show nav panels based on data availability
  shiny::observe({
    has_data <- !is.null(load_data_result$data())
    
    # Tabs that require data to be loaded
    data_dependent_tabs <- c("median", "plotting", "reporting")
    
    for (tab in data_dependent_tabs) {
      if (has_data) {
        bslib::nav_show("active_page", target = tab)
      } else {
        bslib::nav_hide("active_page", target = tab)
      }
    }
  })

  # Uncomment the line below during development to enable live theme editor
  # bslib::bs_themer()
}

shiny::shinyApp(ui = app_ui, server = app_server)
