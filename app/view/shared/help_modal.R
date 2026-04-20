box::use(
  bsicons,
  bslib,
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
    class = "offcanvas offcanvas-end help-offcanvas-resizable",
    tabindex = "-1",
    `data-bs-scroll` = "true",
    `data-bs-backdrop` = "false",
    shiny$tags$div(
      class = "help-resize-handle",
      title = "Drag to resize"
    ),
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

# Help markdown files live in docs/help/{tab_value}/{section}.md
# Supported sections: overview.md, details.md, faq.md
# Tabs are shown dynamically based on which files exist.

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

    get_help_file <- function(tab, section) {
      box::file("..", "..", "docs", "help", tab, paste0(section, ".md"))
    }

    render_section <- function(file_path) {
      if (file.exists(file_path)) {
        shiny$includeMarkdown(file_path)
      } else {
        shiny$tags$p(
          class = "text-muted",
          "No content available yet for this section."
        )
      }
    }

    output$help_content <- shiny$renderUI({
      tab <- active_page()
      if (is.null(tab) || tab == "") {
        return(shiny$tags$p(class = "text-muted", "Select a tab to view help."))
      }

      overview_file <- get_help_file(tab, "overview")
      details_file <- get_help_file(tab, "details")
      faq_file <- get_help_file(tab, "faq")

      has_overview <- file.exists(overview_file)
      has_details <- file.exists(details_file)
      has_faq <- file.exists(faq_file)

      if (!has_overview && !has_details && !has_faq) {
        return(shiny$tags$p(
          class = "text-muted",
          "No help available yet for this section."
        ))
      }

      tab_panels <- list()

      if (has_overview) {
        tab_panels <- c(tab_panels, list(bslib$nav_panel(
          title = shiny$tagList(bsicons$bs_icon("info-circle"), " Overview"),
          value = "overview",
          shiny$div(class = "help-section-content pt-3", render_section(overview_file))
        )))
      }

      if (has_details) {
        tab_panels <- c(tab_panels, list(bslib$nav_panel(
          title = shiny$tagList(bsicons$bs_icon("book"), " Details"),
          value = "details",
          shiny$div(class = "help-section-content pt-3", render_section(details_file))
        )))
      }

      if (has_faq) {
        tab_panels <- c(tab_panels, list(bslib$nav_panel(
          title = shiny$tagList(bsicons$bs_icon("question-circle"), " FAQ"),
          value = "faq",
          shiny$div(class = "help-section-content pt-3", render_section(faq_file))
        )))
      }

      if (length(tab_panels) == 1) {
        file_to_show <- if (has_overview) {
          overview_file
        } else if (has_details) {
          details_file
        } else {
          faq_file
        }
        return(shiny$div(class = "help-section-content", render_section(file_to_show)))
      }

      do.call(
        bslib$navset_pill,
        c(tab_panels, list(id = ns("help_tabs")))
      )
    })
  })
}
