box::use(
  bsicons,
  bslib,
  DT,
  openxlsx,
  rhino,
  shiny,
)

box::use(
  app/logic/column_utils,
  app/logic/error_handling,
  app/logic/summary,
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
        icon = "sliders",
        tooltip_text = "Configuration",
        value = "config_tab",
        # Instructions
        shiny$tags$p(
          class = "small text-muted",
          "Summary statistics for each selected",
          " measurement column. Uses the same data",
          " filtering, outlier detection, and trimming",
          " as the Plotting tab."
        ),
        # Group-by selector (populated dynamically)
        shiny$uiOutput(ns("filter_options_ui")),
        # Shapiro-Wilk normality test toggle
        shiny$checkboxInput(
          inputId = ns("shapiro"),
          label = shiny$tags$span(
            "Test for Normality ",
            bslib$tooltip(
              bsicons$bs_icon(
                "question-circle", class = "text-muted"
              ),
              paste(
                "Performs the Shapiro-Wilk normality",
                "test for each measurement.",
                "A p-value < 0.05 indicates",
                "non-normal distribution."
              )
            )
          ),
          value = FALSE
        ),
        shiny$tags$hr(),
        # Download all tables button
        shiny$downloadButton(
          outputId = ns("download_all"),
          label = "Download All Tables",
          class = "btn-primary btn-sm w-100"
        )
      )
    ),
    main_content = shiny$uiOutput(ns("main_content"))
  )
}

#' @export
server <- function(id, input_data, data_version) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    last_error <- shiny$reactiveVal(NULL)
    summary_dfs <- shiny$reactiveVal(NULL)

    # --- Reset state on new data ---
    shiny$observeEvent(data_version(), {
      summary_dfs(NULL)
      last_error(NULL)
      shiny$updateCheckboxInput(
        session, "shapiro", value = FALSE
      )
      shiny$updateSelectizeInput(
        session, "filter_options_select",
        choices = character(0),
        selected = character(0)
      )
      rhino$log$info("Summary: state reset for new data")
    }, ignoreInit = TRUE)

    # --- Descriptive columns reactive ---
    descriptive_cols <- shiny$reactive({
      data <- input_data()
      shiny$req(data)
      column_utils$get_descriptive_cols(data)
    })

    # --- Filter options UI (group-by selector) ---
    output$filter_options_ui <- shiny$renderUI({
      desc_cols <- descriptive_cols()
      shiny$req(length(desc_cols) > 0)

      # Retain valid current selection
      current <- shiny$isolate(
        input$filter_options_select
      )
      valid_current <- if (
        !is.null(current) && length(current) > 0
      ) {
        current[current %in% desc_cols]
      } else {
        character(0)
      }

      # Default to first descriptive column
      selected <- if (length(valid_current) > 0) {
        valid_current
      } else if (length(desc_cols) > 0) {
        desc_cols[1]
      } else {
        character(0)
      }

      shiny$selectizeInput(
        inputId = ns("filter_options_select"),
        label = "Group by:",
        choices = desc_cols,
        selected = selected,
        multiple = TRUE
      )
    })

    # --- Debounced computation inputs ---
    debounced_inputs <- shiny$reactive({
      shiny$req(input_data())
      shiny$req(input$filter_options_select)
      list(
        grouping_vars = input$filter_options_select,
        shapiro       = input$shapiro %||% FALSE
      )
    }) |> shiny$debounce(400)

    # --- Run computation when inputs change ---
    shiny$observeEvent(debounced_inputs(), {
      params <- debounced_inputs()
      shiny$req(params)
      data <- shiny$isolate(input_data())
      shiny$req(data)

      last_error(NULL)

      result <- summary$run_summary(
        data         = data,
        grouping_vars = params$grouping_vars,
        shapiro_test = params$shapiro
      )

      if (!result$success) {
        last_error(result$error)
        summary_dfs(NULL)
        return()
      }

      summary_dfs(result$result)
    }, ignoreNULL = TRUE, ignoreInit = FALSE)

    # --- Dynamic DT table outputs + per-table downloads ---
    shiny$observe({
      summaries <- summary_dfs()
      shiny$req(summaries)

      lapply(summaries, function(item) {
        local({
          local_item <- item
          safe_col <- gsub(
            "[^a-zA-Z0-9]", "_", local_item$col
          )
          table_id <- paste0("table_", safe_col)
          dl_id <- paste0("download_", safe_col)

          # Render DT table
          output[[table_id]] <- DT$renderDataTable({
            n_rows <- nrow(local_item$df)
            dom <- if (n_rows <= 10) "t" else "tip"

            DT$datatable(
              local_item$df,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = dom,
                language = list(
                  paginate = list(
                    previous = "Previous",
                    `next` = "Next"
                  )
                )
              ),
              rownames = FALSE
            )
          })

          # Per-table XLSX download
          output[[dl_id]] <- shiny$downloadHandler(
            filename = function() {
              paste0(
                "summary_stats_", safe_col, ".xlsx"
              )
            },
            content = function(file) {
              openxlsx$write.xlsx(
                local_item$df, file, rowNames = FALSE
              )
              rhino$log$info(
                "Summary: downloaded table '{local_item$col}'"
              )
            }
          )
        })
      })
    })

    # --- Main content: placeholder, error, or summary cards ---
    output$main_content <- shiny$renderUI({
      data <- input_data()

      # No data loaded
      if (is.null(data)) {
        return(
          bslib$card(
            bslib$card_header("No Data"),
            bslib$card_body(
              shiny$tags$div(
                class = "text-center text-muted py-5",
                bsicons$bs_icon("table", size = "3rem"),
                shiny$tags$h5(
                  class = "mt-3", "No data available"
                ),
                shiny$tags$p(
                  "Load and process data to view",
                  " summary statistics."
                )
              )
            )
          )
        )
      }

      # Error state
      err <- last_error()
      if (error_handling$is_app_error(err)) {
        return(
          error_display$error_alert_structured(
            err, type = "danger"
          )
        )
      }

      # No summaries yet
      summaries <- summary_dfs()
      if (is.null(summaries) || length(summaries) == 0) {
        return(
          bslib$card(
            bslib$card_header("Summary Statistics"),
            bslib$card_body(
              shiny$tags$div(
                class = "text-center text-muted py-5",
                bsicons$bs_icon(
                  "hourglass-split", size = "3rem"
                ),
                shiny$tags$h5(
                  class = "mt-3", "Waiting for input"
                ),
                shiny$tags$p(
                  "Select grouping options to generate",
                  " summary statistics."
                )
              )
            )
          )
        )
      }

      # Render one card per measurement
      table_cards <- lapply(summaries, function(item) {
        col <- item$col
        safe_col <- gsub("[^a-zA-Z0-9]", "_", col)
        table_id <- paste0("table_", safe_col)
        dl_id <- paste0("download_", safe_col)

        bslib$card(
          class = "mb-3",
          fill = FALSE,
          bslib$card_header(
            class = paste(
              "d-flex justify-content-between",
              "align-items-center"
            ),
            shiny$tags$span(
              bsicons$bs_icon("table"), " ", col
            ),
            shiny$tags$a(
              id = ns(dl_id),
              class = paste(
                "shiny-download-link text-primary"
              ),
              href = "",
              target = "_blank",
              download = NA,
              title = "Download table (XLSX)",
              style = "font-size: 1.2rem;",
              bsicons$bs_icon("box-arrow-down")
            )
          ),
          bslib$card_body(
            fillable = FALSE,
            class = "p-2",
            DT$dataTableOutput(ns(table_id))
          )
        )
      })

      do.call(shiny$tagList, table_cards)
    })

    # --- Download all: multi-sheet XLSX ---
    output$download_all <- shiny$downloadHandler(
      filename = function() {
        paste0(
          "summary_statistics_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".xlsx"
        )
      },
      content = function(file) {
        summaries <- summary_dfs()
        shiny$req(summaries)

        wb <- openxlsx$createWorkbook()

        for (item in summaries) {
          # Sanitize sheet name (max 31 chars)
          sheet <- gsub("[^a-zA-Z0-9 ]", "_", item$col)
          sheet <- substr(sheet, 1, 31)

          # Ensure unique sheet names
          existing <- names(wb)
          if (sheet %in% existing) {
            n <- sum(grepl(sheet, existing)) + 1
            sheet <- paste0(
              substr(sheet, 1, 28), "_", n
            )
          }

          openxlsx$addWorksheet(wb, sheet)
          openxlsx$writeData(wb, sheet, item$df)
          openxlsx$setColWidths(
            wb, sheet,
            cols = seq_len(ncol(item$df)),
            widths = "auto"
          )
        }

        openxlsx$saveWorkbook(wb, file, overwrite = TRUE)
        rhino$log$info(
          "Summary: downloaded all tables",
          " ({length(summaries)} sheets)"
        )
      }
    )

    # Return for downstream modules
    invisible(NULL)
  })
}
