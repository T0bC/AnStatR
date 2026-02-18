box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
)

box::use(
  app/logic/column_utils,
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
    # Step 2: Measurement columns (shown after metaData)
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("metaData"), "'] && ",
        "input['", ns("metaData"), "'].length > 0"
      ),
      shiny$selectizeInput(
        inputId = ns("measureVar"),
        label = shiny$tags$span(
          "Measurement columns (Y-Axis) ",
          bslib$tooltip(
            bsicons$bs_icon("info-circle", class = "text-muted"),
            paste(
              "Select columns containing measurements",
              "to plot. One plot per column."
            )
          )
        ),
        choices = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Select measurement columns..."
        )
      ),
      # Step 3: X-Axis and Tooltip (shown after measureVar)
      shiny$conditionalPanel(
        condition = paste0(
          "input['", ns("measureVar"), "'] && ",
          "input['", ns("measureVar"), "'].length > 0"
        ),
        shiny$tags$hr(),
        shiny$fluidRow(
          shiny$column(
            6,
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
            )
          ),
          shiny$column(
            6,
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
            )
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

  # Update metaData choices when data changes (non-version)
  shiny$observe({
    data <- input_data()
    if (is.null(data)) return()
    cols <- column_utils$get_descriptive_cols(data)
    shiny$updateSelectizeInput(
      session, "metaData",
      choices = cols,
      selected = input$metaData[input$metaData %in% cols]
    )
  })

  # Update measureVar choices when data changes (non-version)
  shiny$observe({
    data <- input_data()
    if (is.null(data)) return()
    cols <- column_utils$get_measurement_cols(data)
    shiny$updateSelectizeInput(
      session, "measureVar",
      choices = cols,
      selected = input$measureVar[input$measureVar %in% cols]
    )
  })

  # Update xAxis + tooltip choices from selected metaData
  shiny$observe({
    selected_meta <- input$metaData
    if (is.null(selected_meta)) selected_meta <- character(0)

    shiny$updateSelectizeInput(
      session, "xAxis",
      choices = selected_meta,
      selected = input$xAxis[input$xAxis %in% selected_meta]
    )
    shiny$updateSelectizeInput(
      session, "tooltip",
      choices = selected_meta,
      selected = input$tooltip[
        input$tooltip %in% selected_meta
      ]
    )
  })
}
