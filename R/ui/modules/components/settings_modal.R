# Settings Modal UI Component
#
# This component provides the settings modal dialog for the app.
# It handles theme selection and other global app settings.

# Available themes for the app
available_themes <- list(
  "Default (Light)" = bslib::bs_theme(preset = "bootstrap"),
  "Flatly (Light)" = bslib::bs_theme(preset = "flatly"),
  "Cosmo (Light)" = bslib::bs_theme(preset = "cosmo"),
  "Lumen (Light)" = bslib::bs_theme(preset = "lumen"),
  "Darkly (Dark)" = bslib::bs_theme(preset = "darkly"),
  "Cyborg (Dark)" = bslib::bs_theme(preset = "cyborg"),
  "Slate (Dark)" = bslib::bs_theme(preset = "slate"),
  "Solar (Dark)" = bslib::bs_theme(preset = "solar")
)

# Default theme name (must match a key in available_themes)
default_theme_name <- "Cosmo (Light)"

# Get the default theme object
get_default_theme <- function() {
  available_themes[[default_theme_name]]
}

# Settings modal UI
settings_modal_ui <- function() {
  shiny::modalDialog(
    title = "App Settings",
    size = "m",
    easyClose = TRUE,
    shiny::selectInput(
      inputId = "theme_selector",
      label = "Choose Theme",
      choices = names(available_themes),
      selected = default_theme_name
    ),
    shiny::hr(),
    shiny::helpText("More settings can be added here in the future."),
    footer = shiny::tagList(
      shiny::modalButton("Close")
    )
  )
}

# Settings modal server logic
# @param input Shiny input object
# @param session Shiny session object
settings_modal_server <- function(input, session) {
  # Open settings modal when gear icon is clicked
  shiny::observeEvent(input$settings_btn, {
    shiny::showModal(settings_modal_ui())
  })

  # Apply theme when user selects a different one
  shiny::observeEvent(input$theme_selector, {
    selected_theme <- available_themes[[input$theme_selector]]
    session$setCurrentTheme(selected_theme)
  })
}
