box::use(
  bsicons,
  bslib,
  DT,
  rhino,
  shiny,
)

box::use(
  app/logic/error_handling,
  app/logic/median/column_utils,
  app/logic/median/compute,
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
        tooltip_text = "Grouping",
        value = "grouping_tab",
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
        shiny$uiOutput(ns("grouping_info"))
      ),
      sidebar_tabs$create_tab(
        icon = "funnel",
        tooltip_text = "Filter",
        value = "filter_tab",
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
    action_button = shiny$actionButton(
      inputId = ns("calculate"),
      label = shiny$tagList(
        bsicons$bs_icon("calculator"), " Calculate"
      ),
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

    # --- Reset on new data ---
    shiny$observeEvent(data_version(), {
      filtered_data(NULL)
      filter_message(NULL)
      median_results(NULL)
      removed_cols(NULL)
      last_error(NULL)
      quality_col_info(list(type = "none"))

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
      info <- column_utils$analyze_quality_column(
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

    # --- Build quality settings from current inputs ---
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

    # --- Calculate button ---
    shiny$observeEvent(input$calculate, {
      data <- input_data()
      if (is.null(data)) {
        last_error(error_handling$simple_error(
          message = "No data loaded. Upload a file first.",
          operation_name = "Median Calculation"
        ))
        return()
      }

      last_error(NULL)
      grouping_cols <- input$grouping_columns
      q_settings <- build_quality_settings()

      # Step 1: Apply quality filter
      filter_result <- quality_filter$apply_quality_filter(
        data, q_settings, grouping_cols
      )
      filtered_data(filter_result$data)
      filter_message(filter_result$message)

      # Step 2: Compute medians
      quality_col_name <- if (
        q_settings$enabled && !is.null(q_settings$column)
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
    })

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
              "Configure grouping and quality filter,",
              " then click Calculate."
            )
          )
        ))
      }

      shiny$tagList(
        render_summary_ui(
          filter_message(),
          input$grouping_columns,
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
      DT$datatable(
        median_results(),
        filter = "top",
        options = list(
          pageLength = 25,
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

    # Return median results for downstream modules
    shiny$reactive({ median_results() })
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
