#' Admin Panel UI Module
#'
#' Provides an interface for viewing application logs and system diagnostics.

#' Admin Panel UI
#'
#' @param id Module namespace ID
#' @return Shiny UI elements
UI_admin <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::h3(
          shiny::icon("tools"),
          "Admin Panel - Application Logs"
        ),
        shiny::hr()
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::wellPanel(
          shiny::h4("Log File Selection"),
          shiny::selectInput(
            inputId = ns("log_file_select"),
            label = "Select log file:",
            choices = NULL,
            width = "100%"
          ),
          shiny::numericInput(
            inputId = ns("n_lines"),
            label = "Number of lines to display:",
            value = 100,
            min = 10,
            max = 10000,
            step = 50,
            width = "100%"
          ),
          shiny::actionButton(
            inputId = ns("refresh_logs"),
            label = "Refresh",
            icon = shiny::icon("sync"),
            class = "btn-primary",
            width = "100%"
          ),
          shiny::hr(),
          shiny::actionButton(
            inputId = ns("download_log"),
            label = "Download Current Log",
            icon = shiny::icon("download"),
            class = "btn-secondary",
            width = "100%"
          ),
          shiny::hr(),
          shiny::h4("Log Cleanup"),
          shiny::numericInput(
            inputId = ns("days_to_keep"),
            label = "Days to keep:",
            value = 30,
            min = 1,
            max = 365,
            width = "100%"
          ),
          shiny::actionButton(
            inputId = ns("cleanup_logs"),
            label = "Clean Up Old Logs",
            icon = shiny::icon("trash"),
            class = "btn-warning",
            width = "100%"
          )
        )
      ),
      
      shiny::column(
        width = 9,
        shiny::wellPanel(
          shiny::h4("Log Contents"),
          shiny::verbatimTextOutput(
            outputId = ns("log_contents"),
            placeholder = TRUE
          ) %>%
            shiny::tagAppendAttributes(
              style = "max-height: 600px; overflow-y: auto; font-family: monospace; font-size: 12px; background-color: #1e1e1e; color: #d4d4d4; padding: 15px;"
            )
        ),
        
        shiny::wellPanel(
          shiny::h4("Log Statistics"),
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::div(
                class = "well text-center",
                shiny::h5("Total Lines"),
                shiny::h3(shiny::textOutput(ns("total_lines"), inline = TRUE))
              )
            ),
            shiny::column(
              width = 3,
              shiny::div(
                class = "well text-center",
                shiny::h5("Errors"),
                shiny::h3(
                  shiny::textOutput(ns("error_count"), inline = TRUE),
                  style = "color: #d9534f;"
                )
              )
            ),
            shiny::column(
              width = 3,
              shiny::div(
                class = "well text-center",
                shiny::h5("Warnings"),
                shiny::h3(
                  shiny::textOutput(ns("warning_count"), inline = TRUE),
                  style = "color: #f0ad4e;"
                )
              )
            ),
            shiny::column(
              width = 3,
              shiny::div(
                class = "well text-center",
                shiny::h5("Info"),
                shiny::h3(
                  shiny::textOutput(ns("info_count"), inline = TRUE),
                  style = "color: #5bc0de;"
                )
              )
            )
          )
        )
      )
    )
  )
}
