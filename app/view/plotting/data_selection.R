box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
)

box::use(
  app/logic/shared/column_utils,
  app/logic/plotting/plot_factory,
  app/view/components/sidebar_tabs,
)

#' Build the data selection sidebar tab UI
#' @param ns Namespace function from the parent module
#' @return A sidebar tab created via sidebar_tabs$create_tab()
#' @export
tab_ui <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "table",
    tooltip_text = "Data Selection",
    value = "data_tab",
    shiny$h6(class = "text-muted mb-3", "Data Selection"),
    # Step 1: Descriptive columns (always visible)
    shiny$selectizeInput(
      inputId = ns("metaData"),
      label = shiny$tags$span(
        "Descriptive columns ",
        bslib$tooltip(
          bsicons$bs_icon("info-circle", class = "text-muted"),
          paste(
            "Select columns that describe the data",
            "(sample ID, treatment, etc.)",
            "for filtering and grouping."
          )
        )
      ),
      choices = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Select descriptive columns..."
      )
    ),
    shiny$checkboxInput(
      inputId = ns("disablePlots"),
      label = shiny$tags$span(
        "Disable plots (parameter screening mode) ",
        bslib$tooltip(
          bsicons$bs_icon("info-circle", class = "text-muted"),
          paste(
            "Skip plot and diagnostics generation.",
            "Select only X-Axis and measurement",
            "columns; selections pass straight to",
            "the Statistics tab. Recommended when",
            "screening 40+ measurement parameters.",
            "Auto-normalization is turned on."
          )
        )
      ),
      value = FALSE
    ),
    shiny$conditionalPanel(
      condition = paste0("input['", ns("disablePlots"), "'] == true"),
      shiny$tags$div(
        class = "alert alert-info py-1 px-2 small mb-2",
        bsicons$bs_icon("info-circle", class = "me-1"),
        "Plots are disabled. Auto-normalization is on and the ",
        "Statistics tab will default to parametric ANOVA. Go to ",
        "Statistics and click Compute."
      )
    ),
    # Step 2: X-Axis and measurement columns (shown after metaData)
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("metaData"), "'] && ",
        "input['", ns("metaData"), "'].length > 0"
      ),
      shiny$selectizeInput(
        inputId = ns("xAxis"),
        label = shiny$tags$span(
          "X-Axis ",
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            ),
            paste(
              "Select up to 3 columns for the",
              "X-Axis. Also used in statistics."
            )
          )
        ),
        choices = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Select...",
          maxItems = 3
        )
      ),
      shiny$selectizeInput(
        inputId = ns("measureVar"),
        label = shiny$tags$div(
          class = "d-flex justify-content-between align-items-center",
          shiny$tags$span(
            "Measurement columns (Y-Axis) ",
            bslib$tooltip(
              bsicons$bs_icon("info-circle", class = "text-muted"),
              paste(
                "Select columns containing measurements",
                "to plot. One plot per column."
              )
            )
          ),
          shiny$actionLink(
            inputId = ns("select_all_measure"),
            label = "   Select all",
            class = "small ms-2"
          )
        ),
        choices = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Select measurement columns...",
          closeAfterSelect = FALSE
        )
      ),
      # Step 3: Tooltip and plot type (hidden in screening mode)
      shiny$conditionalPanel(
        condition = paste0(
          "input['", ns("measureVar"), "'] && ",
          "input['", ns("measureVar"), "'].length > 0 && ",
          "input['", ns("disablePlots"), "'] != true"
        ),
        shiny$tags$hr(),
        shiny$selectizeInput(
          inputId = ns("tooltip"),
          label = shiny$tags$span(
            "Tooltip ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle", class = "text-muted"
              ),
              paste(
                "Select columns to display when",
                "hovering over plot points."
              )
            )
          ),
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Select...")
        ),
        # Plot type selector (shown after X-Axis)
        shiny$conditionalPanel(
          condition = paste0(
            "input['", ns("xAxis"), "'] && ",
            "input['", ns("xAxis"), "'].length > 0"
          ),
          shiny$selectInput(
            inputId = ns("plotType"),
            label = shiny$tags$span(
              "Plot Type ",
              bslib$tooltip(
                bsicons$bs_icon(
                  "info-circle", class = "text-muted"
                ),
                paste(
                  "Choose the visualization type.",
                  "Scatter shows individual points,",
                  "Boxplot/Violin show distributions."
                )
              )
            ),
            choices = plot_factory$get_plot_type_choices(),
            selected = "scatter"
          )
        )
      )
    )
  )
}

#' Server logic for the data selection sidebar tab
#'
#' Manages cascading input updates:
#' - metaData + measureVar choices come from column_utils
#' - xAxis + tooltip choices are derived from selected metaData
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param input_data Reactive returning the current data frame
#' @param data_version Reactive returning the data version counter
#' @export
tab_server <- function(input, output, session, input_data,
                       data_version) {
  # Reset screening mode on new data (mirrors processing.R resets)
  shiny$observeEvent(data_version(), {
    shiny$updateCheckboxInput(session, "disablePlots", value = FALSE)
  }, ignoreInit = TRUE)

  # Smart retention on new data: keep selections that still exist
  shiny$observeEvent(data_version(), {
    data <- input_data()
    if (is.null(data)) {
      rhino$log$info("Plotting data_selection: reset (no data)")
      shiny$updateSelectizeInput(
        session, "metaData",
        choices = character(0), selected = character(0)
      )
      shiny$updateSelectizeInput(
        session, "measureVar",
        choices = character(0), selected = character(0)
      )
      shiny$updateSelectizeInput(
        session, "xAxis",
        choices = character(0), selected = character(0)
      )
      shiny$updateSelectizeInput(
        session, "tooltip",
        choices = character(0), selected = character(0)
      )
      return()
    }

    desc_cols <- column_utils$get_descriptive_cols(data)
    meas_cols <- column_utils$get_measurement_cols(data)

    cur_meta <- shiny$isolate(input$metaData)
    cur_meas <- shiny$isolate(input$measureVar)
    cur_x    <- shiny$isolate(input$xAxis)
    cur_tip  <- shiny$isolate(input$tooltip)

    ret_meta <- intersect(cur_meta, desc_cols)
    ret_meas <- intersect(cur_meas, meas_cols)
    ret_x    <- intersect(cur_x, ret_meta)
    ret_tip  <- intersect(cur_tip, ret_meta)

    rhino$log$info(
      "Plotting data_selection: {length(desc_cols)} descriptive, ",
      "{length(meas_cols)} measurement cols available"
    )

    shiny$updateSelectizeInput(
      session, "metaData",
      choices = desc_cols, selected = ret_meta
    )
    shiny$updateSelectizeInput(
      session, "measureVar",
      choices = meas_cols, selected = ret_meas
    )
    shiny$updateSelectizeInput(
      session, "xAxis",
      choices = ret_meta, selected = ret_x
    )
    shiny$updateSelectizeInput(
      session, "tooltip",
      choices = ret_meta, selected = ret_tip
    )
  }, ignoreInit = TRUE)

  # Select all measurement columns on link click
  shiny$observeEvent(input$select_all_measure, {
    data <- input_data()
    if (is.null(data)) return()
    cols <- column_utils$get_measurement_cols(data)
    shiny$updateSelectizeInput(
      session, "measureVar",
      choices = cols, selected = cols
    )
  })

  # Update xAxis + tooltip choices from selected metaData.
  # Debounced so rapid metaData picks don't repeatedly rebuild
  # the xAxis/tooltip widgets (which would close their dropdowns).
  debounced_meta <- shiny$reactive({
    m <- input$metaData
    if (is.null(m)) character(0) else m
  }) |> shiny$debounce(500)

  shiny$observe({
    selected_meta <- debounced_meta()

    cur_x <- shiny$isolate(input$xAxis)
    cur_tip <- shiny$isolate(input$tooltip)

    shiny$updateSelectizeInput(
      session, "xAxis",
      choices = selected_meta,
      selected = cur_x[cur_x %in% selected_meta]
    )
    shiny$updateSelectizeInput(
      session, "tooltip",
      choices = selected_meta,
      selected = cur_tip[cur_tip %in% selected_meta]
    )
  })
}
