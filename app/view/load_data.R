box::use(
  bsicons,
  bslib,
  DataExplorer,
  DT,
  ggplot2,
  rhino,
  shiny,
  summarytools,
)

box::use(
  app/logic/column_utils,
  app/logic/error_handling,
  app/logic/load_data,
  app/view/components/column_validation_modal,
  app/view/components/sidebar_tabs,
  app/view/error_display,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  sidebar_tabs$tab_layout(
    ns = ns,
    sidebar_id = "sidebar_tabs",
    tabs = list(
      sidebar_tabs$create_tab(
        icon = "file-earmark-arrow-up",
        tooltip_text = "Load Data",
        value = "upload_tab",
        shiny$h6(class = "text-muted mb-3", "Upload Dataset"),
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
        shiny$helpText(
          "Accepted formats: CSV or XLSX (single file)."
        )
      ),
      sidebar_tabs$create_tab(
        icon = "gear",
        tooltip_text = "CSV Settings",
        value = "csv_settings_tab",
        shiny$h6(class = "text-muted mb-3", "CSV Settings"),
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
    ),
    main_content = shiny$uiOutput(ns("main_content"))
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    loaded_data <- shiny$reactiveVal(NULL)
    data_version <- shiny$reactiveVal(0)
    last_error <- shiny$reactiveVal(NULL)

    # Handle file upload
    shiny$observeEvent(input$data_file, {
      shiny$req(input$data_file)

      file_info <- input$data_file
      rhino$log$info("File upload received: '{file_info$name}'")

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
        last_error(result$error)
        return()
      }

      # Step 4: Validate data (pure logic)
      validation <- load_data$validate_data(result$data)
      if (!validation$valid) {
        loaded_data(NULL)
        last_error(validation$error)
        return()
      }

      # Success — update reactives
      last_error(NULL)
      rhino$log$info(
        "Load complete: '{file_info$name}' "
      )
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

      # Check column naming conventions
      col_validation <- column_utils$validate_column_naming(
        result$data
      )
      if (!col_validation$valid) {
        rhino$log$warn(
          "Ambiguous columns: {paste(col_validation$ambiguous_cols, collapse = ', ')}"
        )
        shiny$showModal(
          column_validation_modal$create_modal(
            col_validation
          )
        )
      }
    })

    # Main content: welcome screen, error, or data panels
    output$main_content <- shiny$renderUI({
      err <- last_error()
      if (error_handling$is_app_error(err)) {
        error_display$error_alert_structured(err, type = "danger")
      } else if (is.null(loaded_data())) {
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
            icon = bsicons$bs_icon("table"),
            shiny$tags$div(
              class = "table-responsive",
              DT$dataTableOutput(ns("data_preview"))
            )
          ),
          bslib$accordion_panel(
            title = "Missing Values",
            value = "missing_values",
            icon = bsicons$bs_icon("bar-chart"),
            shiny$plotOutput(ns("missing_values_plot"))
          ),
          bslib$accordion_panel(
            title = "Data Summary",
            value = "data_summary",
            icon = bsicons$bs_icon("list-ul"),
            shiny$uiOutput(ns("data_summary"))
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

    # Missing values plot
    output$missing_values_plot <- shiny$renderPlot({
      shiny$req(loaded_data())
      DataExplorer$plot_missing(
        loaded_data(),
        ggtheme = ggplot2$theme_classic(base_size = 16)
      )
    })

    # Data summary (summarytools::dfSummary rendered as HTML)
    output$data_summary <- shiny$renderUI({
      shiny$req(loaded_data())
      summary_obj <- summarytools$dfSummary(
        loaded_data(),
        max.distinct.values = 25
      )
      summary_html <- utils::capture.output(
        print(
          summary_obj,
          method = "render",
          plain.ascii = FALSE,
          varnumbers = FALSE,
          valid.col = FALSE,
          graph.magnif = 0.5,
          style = "grid",
          footnote = ""
        )
      )
      summary_html <- paste(summary_html, collapse = "\n")
      shiny$HTML(summary_html)
    })

    # Return for downstream modules
    list(
      data = shiny$reactive({ loaded_data() }),
      version = shiny$reactive({ data_version() })
    )
  })
}
