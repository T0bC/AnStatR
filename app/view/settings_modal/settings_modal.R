box::use(
  bsicons,
  markdown,
  rhino,
  shiny,
)

box::use(
  app/logic/shared/settings,
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
          title = "About TexAn",
          size = "m",
          easyClose = TRUE,
          shiny$actionButton(
            inputId = session$ns("show_changelog"),
            label = "View Changelog",
            icon = bsicons$bs_icon("journal-text"),
            class = "btn-outline-secondary btn-sm"
          ),
          shiny$tags$a(
            href = "https://github.com/T0bC/TexAn2.0/issues/new/choose",
            target = "_blank",
            rel = "noopener noreferrer",
            class = "btn btn-outline-danger btn-sm",
            style = "margin-left: 8px;",
            bsicons$bs_icon("bug"),
            " Report Issue"
          ),
          footer = shiny$tagList(
            shiny$div(
              style = "text-align: left; flex: 1;",
              shiny$helpText(paste("Version:", settings$get_version_string())),
              shiny$helpText(paste("Session ID:", substr(session$token, 1, 8)))
            ),
            shiny$modalButton("Close")
          )
        )
      )
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
