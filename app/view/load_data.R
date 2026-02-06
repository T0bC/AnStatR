box::use(
  bslib,
  DT,
  shiny,
)

box::use(
  app/logic/load_data,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  bslib$layout_sidebar(
    sidebar = bslib$sidebar(
      title = "Instructions",
      shiny$fileInput(
        inputId = ns("data_file"),
        label = "Upload dataset",
        multiple = FALSE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv",
          ".xlsx"
        )
      ),
      shiny$helpText("Accepted formats: CSV or XLSX (single file)."),
      bslib$accordion(
        id = ns("csv_settings_collapse"),
        open = FALSE,
        bslib$accordion_panel(
          title = "CSV settings",
          value = "csv_settings_panel",
          shiny$helpText(
            "The following settings only apply to CSV uploads.",
            "XLSX files ignore these options."
          ),
          shiny$checkboxInput(
            inputId = ns("csv_has_header"),
            label = "CSV includes header row",
            value = TRUE
          ),
          shiny$radioButtons(
            inputId = ns("csv_delimiter"),
            label = "Delimiter",
            choices = c(
              ", (comma)" = ",",
              "; (semicolon)" = ";",
              "Tab" = "\t"
            ),
            selected = ","
          ),
          shiny$radioButtons(
            inputId = ns("csv_quote"),
            label = "Quote character",
            choices = c(
              "None" = "",
              "Double quote (\"\")" = '"',
              "Single quote (')" = "'"
            ),
            selected = '"'
          )
        )
      )
    ),
    shiny$uiOutput(ns("main_content"))
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    loaded_data <- shiny$reactiveVal(NULL)
    data_version <- shiny$reactiveVal(0)

    # Handle file upload
    shiny$observeEvent(input$data_file, {
      shiny$req(input$data_file)

      file_info <- input$data_file

      # Step 1: Validate extension (pure logic)
      ext_check <- load_data$validate_file_extension(file_info$name)
      if (!ext_check$valid) {
        loaded_data(NULL)
        shiny$showNotification(
          "Only CSV and XLSX files are supported.",
          type = "warning"
        )
        return()
      }

      # Step 2: Normalize quote char (pure logic)
      quote <- load_data$normalize_quote_char(input$csv_quote)

      # Step 3: Read file (pure logic)
      result <- load_data$read_data_file(
        path = file_info$datapath,
        ext = ext_check$ext,
        header = input$csv_has_header,
        delimiter = input$csv_delimiter,
        quote_char = quote
      )

      if (!result$success) {
        loaded_data(NULL)
        shiny$showNotification(
          paste("Error reading file:", result$error),
          type = "error",
          duration = 5
        )
        return()
      }

      # Step 4: Validate data (pure logic)
      validation <- load_data$validate_data(result$data)
      if (!validation$valid) {
        loaded_data(NULL)
        shiny$showNotification(validation$message, type = "error")
        return()
      }

      # Success — update reactives
      data_version(data_version() + 1)
      loaded_data(result$data)
      shiny$showNotification(
        paste0(
          "Data loaded successfully! (",
          nrow(result$data), " rows, ",
          ncol(result$data), " columns)"
        ),
        type = "message",
        duration = 3
      )
    })

    # Main content: welcome screen or data panels
    output$main_content <- shiny$renderUI({
      if (is.null(loaded_data())) {
        shiny$tags$div(
          class = "d-flex align-items-center justify-content-center",
          style = "min-height: 400px;",
          shiny$tags$p(
            class = "text-muted",
            "Upload a CSV or XLSX file to get started."
          )
        )
      } else {
        bslib$accordion(
          id = ns("data_panels_accordion"),
          open = "data_preview",
          multiple = TRUE,
          bslib$accordion_panel(
            title = "Data Preview",
            value = "data_preview",
            shiny$tags$div(
              class = "table-responsive",
              DT$dataTableOutput(ns("data_preview"))
            )
          )
        )
      }
    })

    # Data preview table
    output$data_preview <- DT$renderDataTable({
      shiny$req(loaded_data())
      DT$datatable(
        loaded_data(),
        filter = "top",
        options = list(
          pageLength = 10,
          lengthMenu = list(
            c(10, 25, 50, 100, -1),
            c("10", "25", "50", "100", "All")
          ),
          scrollX = TRUE,
          dom = "ltip"
        ),
        rownames = FALSE
      )
    })

    # Return for downstream modules
    list(
      data = shiny$reactive({ loaded_data() }),
      version = shiny$reactive({ data_version() })
    )
  })
}
