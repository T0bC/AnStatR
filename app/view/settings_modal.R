box::use(
  bsicons,
  markdown,
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
          shiny$hr(),
          shiny$actionButton(
            inputId = session$ns("show_changelog"),
            label = "View Changelog",
            icon = bsicons$bs_icon("journal-text"),
            class = "btn-outline-secondary btn-sm"
          ),
          footer = shiny$tagList(
            shiny$helpText(paste("Version:", settings$get_version_string())),
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

    # Show changelog modal
    shiny$observeEvent(input$show_changelog, {
      rhino$log$debug("Changelog modal opened")
      changelog_html <- markdown$mark(settings$get_changelog_markdown())

      shiny$showModal(
        shiny$modalDialog(
          title = "Changelog",
          size = "l",
          easyClose = TRUE,
          shiny$div(
            style = "max-height: 60vh; overflow-y: auto; padding: 10px;",
            shiny$HTML(changelog_html)
          ),
          footer = shiny$modalButton("Close")
        )
      )
    })
  })
}
