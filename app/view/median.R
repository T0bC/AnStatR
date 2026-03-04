box::use(
  bslib,
  DT,
  openxlsx,
  rhino,
  shiny,
)

box::use(
  app/logic/column_utils,
  app/logic/error_handling,
  app/logic/median/compute,
  app/logic/median/quality_analysis,
  app/logic/median/quality_filter,
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
        tooltip_text = "Settings",
        value = "settings_tab",
        # --- Grouping ---
        shiny$h6(class = "text-muted mb-3", "Grouping"),
        shiny$tags$p(
          class = "text-muted small",
          "Select columns that define your sample",
          " structure for median calculation."
        ),
        shiny$selectizeInput(
          inputId = ns("grouping_columns"),
          label = NULL,
          choices = NULL,
          selected = NULL,
          multiple = TRUE,
          options = list(
            placeholder = "Select grouping columns..."
          )
        ),
        shiny$uiOutput(ns("grouping_info")),
        # --- Quality filter ---
        shiny$tags$hr(),
        shiny$h6(
          class = "text-muted mb-3", "Quality Filter"
        ),
        shiny$tags$p(
          class = "text-muted small",
          "Optional: select a column that indicates",
          " measurement quality."
        ),
        shiny$selectizeInput(
          inputId = ns("quality_column"),
          label = NULL,
          choices = c(
            "None (no quality filtering)" = "None"
          ),
          selected = "None",
          multiple = FALSE,
          options = list(
            placeholder = "Select quality column..."
          )
        ),
        shiny$uiOutput(ns("quality_filter_options"))
      )
    ),
    main_content = shiny$uiOutput(ns("main_content")),
    action_button = shiny$downloadButton(
      outputId = ns("downloadData"),
      label = "Download Filtered Data",
      class = "btn-primary btn-sm w-100"
    )
  )
}

#' @export
server <- function(id, input_data, data_version) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Reactive state ---
    filtered_data <- shiny$reactiveVal(NULL)
    filter_message <- shiny$reactiveVal(NULL)
    median_results <- shiny$reactiveVal(NULL)
    removed_cols <- shiny$reactiveVal(NULL)
    last_error <- shiny$reactiveVal(NULL)
    quality_col_info <- shiny$reactiveVal(
      list(type = "none")
    )
    cached_params <- shiny$reactiveVal(NULL)

    # --- Reset on new data ---
    shiny$observeEvent(data_version(), {
      filtered_data(NULL)
      filter_message(NULL)
      median_results(NULL)
      removed_cols(NULL)
      last_error(NULL)
      quality_col_info(list(type = "none"))
      cached_params(NULL)

      data <- input_data()
      if (!is.null(data)) {
        new_cols <- column_utils$get_descriptive_cols(data)

        # Smart retention: keep selections that still exist
        current_grp <- shiny$isolate(
          input$grouping_columns
        )
        retained_grp <- if (!is.null(current_grp)) {
          intersect(current_grp, new_cols)
        } else {
          character(0)
        }

        current_qc <- shiny$isolate(input$quality_column)
        retained_qc <- if (!is.null(current_qc) &&
            current_qc != "None" &&
            current_qc %in% new_cols) {
          current_qc
        } else {
          "None"
        }

        shiny$updateSelectizeInput(
          session, "grouping_columns",
          choices = new_cols,
          selected = retained_grp
        )
        shiny$updateSelectizeInput(
          session, "quality_column",
          choices = c(
            "None (no quality filtering)" = "None",
            new_cols
          ),
          selected = retained_qc
        )
      } else {
        shiny$updateSelectizeInput(
          session, "grouping_columns",
          choices = character(0),
          selected = character(0)
        )
        shiny$updateSelectizeInput(
          session, "quality_column",
          choices = c(
            "None (no quality filtering)" = "None"
          ),
          selected = "None"
        )
      }
      rhino$log$info("Median: state reset for new data")
    }, ignoreInit = TRUE)

    # --- Populate choices on first data load ---
    shiny$observeEvent(input_data(), {
      data <- input_data()
      shiny$req(data)
      cols <- column_utils$get_descriptive_cols(data)
      shiny$updateSelectizeInput(
        session, "grouping_columns",
        choices = cols, selected = character(0)
      )
      shiny$updateSelectizeInput(
        session, "quality_column",
        choices = c(
          "None (no quality filtering)" = "None", cols
        ),
        selected = "None"
      )
    }, once = TRUE)

    # --- Grouping info ---
    output$grouping_info <- shiny$renderUI({
      data <- input_data()
      shiny$req(data)
      group_cols <- input$grouping_columns

      if (is.null(group_cols) ||
          length(group_cols) == 0) {
        return(shiny$tags$p(
          class = "text-muted small fst-italic",
          "No grouping selected.",
          " Filtering will apply to entire dataset."
        ))
      }

      n_groups <- nrow(
        unique(data[, group_cols, drop = FALSE])
      )
      n_rows <- nrow(data)
      avg <- round(n_rows / n_groups, 1)

      shiny$tags$div(
        class = "alert alert-info py-1 px-2 small",
        shiny$tags$strong(n_groups),
        " unique groups identified",
        shiny$tags$br(),
        shiny$tags$span(
          class = "text-muted",
          paste0(
            "(~", avg, " rows per group on average)"
          )
        )
      )
    })

    # --- Quality column analysis ---
    shiny$observeEvent(input$quality_column, {
      data <- input_data()
      shiny$req(data)
      info <- quality_analysis$analyze_quality_column(
        data, input$quality_column
      )
      quality_col_info(info)
    })

    # --- Quality filter dynamic UI ---
    output$quality_filter_options <- shiny$renderUI({
      info <- quality_col_info()
      if (info$type == "none") return(NULL)

      shiny$tagList(
        shiny$tags$p(
          class = "text-muted small fst-italic",
          info$hint
        ),
        if (info$type == "categorical") {
          shiny$selectizeInput(
            inputId = ns("bad_quality_values"),
            label = "Select BAD quality values:",
            choices = info$unique_values,
            selected = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "Select values to exclude..."
            )
          )
        } else {
          default_thr <- if (
            info$type == "percentage_decimal"
          ) {
            0.8
          } else if (info$type == "percentage_100") {
            80
          } else {
            info$min + (info$max - info$min) * 0.5
          }

          shiny$numericInput(
            inputId = ns("quality_threshold"),
            label = "Minimum quality threshold (>=):",
            value = default_thr,
            min = info$min,
            max = info$max,
            step = if (
              info$type == "percentage_decimal"
            ) 0.05 else 1
          )
        }
      )
    })

    # --- Unified debounced params (grouping + quality) ---
    # Collects all inputs into one reactive with a single
    # debounce so the computation fires only once after all
    # inputs have settled.
    build_quality_settings <- function() {
      info <- quality_col_info()
      if (is.null(input$quality_column) ||
          input$quality_column == "None") {
        list(
          enabled = FALSE, column = NULL, type = "none"
        )
      } else if (info$type == "categorical") {
        list(
          enabled = TRUE,
          column = input$quality_column,
          type = "categorical",
          bad_values = input$bad_quality_values
        )
      } else {
        list(
          enabled = TRUE,
          column = input$quality_column,
          type = info$type,
          threshold = input$quality_threshold
        )
      }
    }

    null_to_str <- function(x) {
      if (is.null(x)) "NULL" else x
    }

    make_fingerprint <- function(params) {
      paste(
        paste(
          null_to_str(params$grouping_cols),
          collapse = ":"
        ),
        params$quality_settings$enabled,
        null_to_str(params$quality_settings$column),
        params$quality_settings$type,
        paste(
          null_to_str(params$quality_settings$bad_values),
          collapse = ":"
        ),
        null_to_str(params$quality_settings$threshold),
        sep = "|"
      )
    }

    debounced_inputs <- shiny$reactive({
      shiny$req(input_data())
      list(
        grouping_cols = input$grouping_columns,
        quality_settings = build_quality_settings()
      )
    }) |> shiny$debounce(400)

    shiny$observe({
      new_params <- debounced_inputs()
      shiny$req(new_params)
      current <- cached_params()
      new_fp <- make_fingerprint(new_params)
      old_fp <- if (!is.null(current)) {
        make_fingerprint(current)
      } else {
        ""
      }
      if (new_fp != old_fp) {
        cached_params(new_params)
      }
    })

    # --- Run computation when params change ---
    shiny$observeEvent(cached_params(), {
      params <- cached_params()
      shiny$req(params)
      data <- shiny$isolate(input_data())
      shiny$req(data)

      last_error(NULL)
      grouping_cols <- params$grouping_cols
      q_settings <- params$quality_settings

      # Step 1: Apply quality filter
      filter_result <- quality_filter$apply_quality_filter(
        data, q_settings, grouping_cols
      )
      filtered_data(filter_result$data)
      filter_message(filter_result$message)

      # Step 2: Compute medians
      quality_col_name <- if (
        q_settings$enabled &&
          !is.null(q_settings$column)
      ) {
        q_settings$column
      } else {
        NULL
      }

      result <- compute$compute_medians(
        filter_result$data,
        grouping_cols,
        quality_col = quality_col_name
      )

      if (!result$success) {
        last_error(result$error)
        median_results(NULL)
        removed_cols(NULL)
        return()
      }

      median_results(result$result)
      removed_cols(result$removed_cols)
      rhino$log$info("Median: calculation complete")
    }, ignoreNULL = TRUE, ignoreInit = FALSE)

    # --- Main content ---
    output$main_content <- shiny$renderUI({
      err <- last_error()
      if (error_handling$is_app_error(err)) {
        return(
          error_display$error_alert_structured(
            err, type = "danger"
          )
        )
      }

      if (is.null(median_results())) {
        return(shiny$tags$div(
          class = paste0(
            "d-flex align-items-center ",
            "justify-content-center"
          ),
          style = "min-height: 400px;",
          shiny$tags$div(
            class = "text-center text-muted",
            shiny$tags$h4("Median Calculation"),
            shiny$tags$p(
              "Select grouping columns to begin."
            )
          )
        ))
      }

      shiny$tagList(
        render_summary_ui(
          filter_message(),
          cached_params()$grouping_cols,
          removed_cols()
        ),
        shiny$tags$div(
          class = "table-responsive",
          DT$dataTableOutput(ns("median_table"))
        )
      )
    })

    # --- Median results table ---
    output$median_table <- DT$renderDataTable({
      shiny$req(median_results())
      display_df <- median_results()

      # Cast metadata columns to factor for dropdown filters,
      # but skip the quality column so it stays filterable
      # by numeric value as before.
      quality_col <- cached_params()$quality_settings$column
      desc_cols <- column_utils$get_descriptive_cols(
        display_df
      )
      for (col in desc_cols) {
        if (!identical(col, quality_col)) {
          display_df[[col]] <- as.factor(display_df[[col]])
        }
      }

      DT$datatable(
        display_df,
        filter = list(
          position = "top",
          settings = list(select = list(maxOptions = 2000))
        ),
        options = list(
          pageLength = 25,
          lengthMenu = list(
            c(10, 25, 50, 100, -1),
            c("10", "25", "50", "100", "All")
          ),
          scrollX = TRUE,
          dom = "ltip",
          drawCallback = DT$JS("
            function(settings) {
              var api = this.api();
              var tableId = settings.sTableId;
              if (window['_dtSelAll_' + tableId]) return;
              setTimeout(function() {
                var wrapper = $('#' + tableId + '_wrapper');
                var filterRow = wrapper.find(
                  '.dataTable thead tr.odd, ' +
                  '.dataTable thead tr:nth-child(2)'
                );
                if (filterRow.length === 0) {
                  filterRow = $(api.table().header())
                    .closest('thead').find('tr').last();
                }
                if (filterRow.length === 0) return;
                var injected = false;
                filterRow.find('td, th').each(function() {
                  var cell = $(this);
                  if (cell.find('.dt-sel-all').length) return;
                  var selEl = cell.find('select')[0];
                  if (!selEl || !selEl.selectize) return;
                  var sz = selEl.selectize;
                  var link = $(
                    '<a href=\"#\" class=\"dt-sel-all\" ' +
                    'style=\"font-size:11px;display:block;' +
                    'margin-bottom:2px;\">All</a>'
                  );
                  link.on('click', function(e) {
                    e.preventDefault();
                    var opts = Object.keys(sz.options);
                    sz.setValue(opts, true);
                    api.draw();
                  });
                  cell.prepend(link);
                  injected = true;
                });
                if (injected) {
                  window['_dtSelAll_' + tableId] = true;
                }
              }, 500);
            }
          ")
        ),
        rownames = FALSE
      )
    })

    # --- DT-filtered data reactive (used for download + return) ---
    dt_filtered_data <- shiny$reactive({
      data <- median_results()
      if (is.null(data)) return(NULL)
      filtered_rows <- input$median_table_rows_all
      if (is.null(filtered_rows)) return(data)
      result <- data[filtered_rows, , drop = FALSE]
      # Drop unused factor levels after filtering to prevent
      # issues in downstream modules (LDA, PCA, etc.)
      droplevels(result)
    })

    # --- Download handler: filtered data as Excel ---
    output$downloadData <- shiny$downloadHandler(
      filename = function() {
        paste0("median_filtered_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        data <- dt_filtered_data()
        if (is.null(data) || nrow(data) == 0) {
          wb <- openxlsx$createWorkbook()
          openxlsx$addWorksheet(wb, "No Data")
          openxlsx$writeData(
            wb, "No Data", "No filtered data available."
          )
          openxlsx$saveWorkbook(wb, file, overwrite = TRUE)
          return()
        }
        openxlsx$write.xlsx(data, file, rowNames = FALSE)
        rhino$log$info(
          "Download: median filtered data ({nrow(data)} rows)"
        )
      }
    )

    # Return DT-filtered median results for downstream modules
    dt_filtered_data
  })
}

# --- Helper: processing summary alert ---
render_summary_ui <- function(filter_msg, grouping_cols,
                              removed) {
  grouping_info <- if (is.null(grouping_cols) ||
      length(grouping_cols) == 0) {
    shiny$tags$p(
      class = "mb-1",
      shiny$tags$em(
        "No grouping selected -",
        " showing filtered data without median."
      )
    )
  } else {
    shiny$tags$p(
      class = "mb-1",
      shiny$tags$strong("Grouping by: "),
      paste(grouping_cols, collapse = ", ")
    )
  }

  removed_info <- if (!is.null(removed) &&
      length(removed) > 0) {
    shiny$tags$p(
      class = "mb-1 text-warning",
      shiny$tags$strong(
        "Columns removed (vary within groups): "
      ),
      paste(removed, collapse = ", ")
    )
  } else {
    NULL
  }

  msg_lines <- if (!is.null(filter_msg)) {
    strsplit(filter_msg, "\n")[[1]]
  } else {
    "No quality filtering applied."
  }

  shiny$tags$div(
    class = "alert alert-info",
    shiny$tags$strong("Processing Summary"),
    shiny$tags$hr(class = "my-2"),
    grouping_info,
    removed_info,
    shiny$tags$hr(class = "my-2"),
    shiny$tags$strong("Quality Filter: "),
    lapply(msg_lines, function(line) {
      shiny$tags$span(line, shiny$tags$br())
    })
  )
}
