# Source utility functions (used across multiple modules)
source("R/utils/column_utils.R")
source("R/utils/data_utils.R")

# Source UI modules
source("R/ui/modules/pages/ui_load_data.R")
source("R/ui/modules/pages/ui_median.R")
source("R/ui/modules/pages/ui_plotting.R")
source("R/ui/modules/pages/ui_summary_stats.R")

# Source component modules
source("R/ui/modules/components/settings_modal.R")

# Source server modules
source("R/server/modules/pages/server_load_data.R")
source("R/server/modules/pages/server_median.R")
source("R/server/modules/pages/server_plotting.R")
source("R/server/modules/pages/server_summary_stats.R")

# Source server sub-modules: Load Data
source("R/server/modules/pages/load_data/file_upload.R")
source("R/server/modules/pages/load_data/data_preview.R")
source("R/server/modules/pages/load_data/missing_values_plot.R")
source("R/server/modules/pages/load_data/data_summary.R")

# Source server sub-modules: Median
source("R/server/modules/pages/median/help_modal.R")
source("R/server/modules/pages/median/grouping_ui.R")
source("R/server/modules/pages/median/quality_filter_ui.R")
source("R/server/modules/pages/median/quality_filter_logic.R")
source("R/server/modules/pages/median/median_table.R")

# Source server sub-modules: Plotting
source("R/server/modules/pages/plotting/plot_scatter.R")
source("R/server/modules/pages/plotting/plot_renderer.R")

# Source server sub-modules: Summary Stats
source("R/server/modules/pages/summary_stats/summary_utils.R")

# Load required packages
library(shiny)
library(bslib)
library(bsicons)

library(dplyr)
library(openxlsx)
library(DT)
library(DataExplorer)
library(summarytools)
library(ggiraph)
library(ggh4x)
library(colourpicker)
library(scales)
library(shinycssloaders)
library(purrr)
library(rlang)

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
    shiny::tags$script(src = "www/js/plot_resize.js"),
    # Include selectize dependencies to fix DT column filter compatibility issue
    htmltools::findDependencies(shiny::selectizeInput("__selectize_dep__", NULL, choices = NULL))
  ),
  bslib::nav_panel(title = "Load Data", value = "load_data", UI_load_data("load_data_id")),
  bslib::nav_panel(title = "Median Analysis", value = "median", UI_median("median_id")),
  bslib::nav_panel(title = "Plotting", value = "plotting", UI_plotting("plotting_id")),
  bslib::nav_panel(title = "Summary Stats", value = "summary_stats", UI_summary_stats("summary_stats_id")),
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
  # plotting_result contains processed_data (with {col}_outlier and {col}_trimmed flags)
  # and selected_measures - Plotting tab is the source of truth for downstream modules
  plotting_result <- server_plotting("plotting_id",
                                     median_data = median_result,
                                     data_version = load_data_result$version)
  
  # Pass plotting-processed data to summary stats module
  # processed_data has {col}_outlier and {col}_trimmed columns for each selected measurement
  server_summary_stats("summary_stats_id",
                       processed_data = plotting_result$processed_data,
                       selected_measures = plotting_result$selected_measures,
                       data_version = load_data_result$version)

  # Initialize settings modal
  settings_modal_server(input, session)
  
  # Hide/show nav panels based on data availability
  shiny::observe({
    has_data <- !is.null(load_data_result$data())
    
    # Tabs that require data to be loaded
    data_dependent_tabs <- c("median", "plotting", "summary_stats", "reporting")
    
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
