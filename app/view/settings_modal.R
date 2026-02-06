box::use(
  bsicons,
  rhino,
  shiny,
)

box::use(
  app/logic/settings,
)

#' Settings gear icon for placement in the navbar
#' @param id Character, module namespace id
#' @return A shiny actionLink with a gear icon
#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$actionLink(
    inputId = ns("settings_btn"),
    label = bsicons$bs_icon("gear"),
    title = "Settings"
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {

    # Open settings modal when gear icon is clicked
    shiny$observeEvent(input$settings_btn, {
      rhino$log$debug("Settings modal opened")
      shiny$showModal(
        shiny$modalDialog(
          title = "App Settings",
          size = "m",
          easyClose = TRUE,
          shiny$selectInput(
            inputId = session$ns("theme_selector"),
            label = "Choose Theme",
            choices = settings$get_theme_names(),
            selected = settings$default_theme_name
          ),
          shiny$hr(),
          shiny$helpText(
            "More settings can be added here in the future."
          ),
          footer = shiny$tagList(
            shiny$modalButton("Close")
          )
        )
      )
    })

    # Apply theme when user selects a different one
    shiny$observeEvent(input$theme_selector, {
      theme <- settings$get_theme(input$theme_selector)
      session$setCurrentTheme(theme)
      rhino$log$info("Theme changed to: '{input$theme_selector}'")
    })
  })
}
