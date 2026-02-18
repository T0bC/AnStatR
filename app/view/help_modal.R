box::use(
  bsicons,
  shiny,
)

#' Help question-mark icon for placement in the navbar
#' @param id Character, module namespace id
#' @return A shiny actionLink with a question-mark icon
#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$actionLink(
    inputId = ns("help_btn"),
    label = bsicons$bs_icon("patch-question"),
    title = "Help"
  )
}

#' Offcanvas panel HTML — must be placed in the page body (outside the navbar)
#' @param id Character, module namespace id (same as ui)
#' @return A shiny tag for the offcanvas sidebar
#' @export
panel <- function(id) {
  ns <- shiny$NS(id)

  shiny$tags$div(
    id = ns("help_panel"),
    class = "offcanvas offcanvas-end",
    tabindex = "-1",
    `data-bs-scroll` = "true",
    `data-bs-backdrop` = "false",
    shiny$tags$div(
      class = "offcanvas-header",
      shiny$tags$h5(class = "offcanvas-title", "Help"),
      shiny$tags$button(
        type = "button",
        class = "btn-close",
        `data-bs-dismiss` = "offcanvas",
        `aria-label` = "Close"
      )
    ),
    shiny$tags$div(
      class = "offcanvas-body",
      shiny$uiOutput(ns("help_content"))
    )
  )
}

# Help markdown files live in docs/help/{tab_value}.md
# To add help for a new module, create docs/help/{tab_value}.md — no code changes needed.
help_dir <- "docs/help"

#' @param id Character, module namespace id
#' @param active_page Reactive string returning the currently selected tab value
#' @export
server <- function(id, active_page) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny$observeEvent(input$help_btn, {
      panel_id <- ns("help_panel")
      shiny$insertUI(
        selector = "body",
        where = "beforeEnd",
        ui = shiny$tags$script(shiny$HTML(sprintf(
          "var el = document.getElementById('%s');
           if (el) {
             var instance = bootstrap.Offcanvas.getOrCreateInstance(el);
             instance.toggle();
           }",
          panel_id
        ))),
        immediate = TRUE
      )
    })

    output$help_content <- shiny$renderUI({
      tab <- active_page()
      help_file <- file.path(help_dir, paste0(tab, ".md"))

      if (file.exists(help_file)) {
        shiny$includeMarkdown(help_file)
      } else {
        shiny$tags$p(
          class = "text-muted",
          paste0("No help available yet for this section.")
        )
      }
    })
  })
}
