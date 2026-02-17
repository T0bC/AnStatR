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

#' @export
tab_ui <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "sliders",
    tooltip_text = "Data Selection",
    value = "data_tab",
    shiny$h6(class = "text-muted mb-3", "Data Selection"),
    shiny$helpText(
      paste(
        "Select the correct columns for clustering.",
        "Avoid columns with many empty cells.",
        "Rows with empty cells are deleted!"
      )
    ),
    # Metadata columns selection
    shiny$selectizeInput(
      inputId = ns("metaData"),
      label = shiny$tags$span(
        "Descriptive (metadata) columns ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Select columns that describe the",
            "data, such as the sample ID,",
            "treatment, etc., that are important",
            "for your analysis."
          )
        )
      ),
      choices = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Select descriptive columns...",
        closeAfterSelect = FALSE
      )
    ),
    # Measurement columns selection
    shiny$selectizeInput(
      inputId = ns("measureVar"),
      label = shiny$tags$div(
        class = "d-flex justify-content-between align-items-center",
        shiny$tags$span(
          "Measurement columns ",
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            ),
            paste(
              "Select columns that contain the",
              "actual measurements, such as texture",
              "or other parameters, that you want",
              "to include in the clustering analysis.",
              "Only select columns that contain",
              "numerical data!"
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
    shiny$tags$hr(),
    # Data scaling options
    shiny$tags$label(
      class = "control-label",
      "Data Scaling ",
      bslib$tooltip(
        bsicons$bs_icon(
          "info-circle", class = "text-muted"
        ),
        paste(
          "Choose how to preprocess the data",
          "before clustering. Scaling ensures variables",
          "with different units contribute equally."
        )
      )
    ),
    shiny$radioButtons(
      inputId = ns("scale_method"),
      label = NULL,
      choices = list(
        "Scale & Center (recommended)" = "scale_center",
        "Center only" = "center_only",
        "No scaling" = "none"
      ),
      selected = "scale_center"
    ),
    shiny$tags$small(
      class = "text-muted",
      shiny$tags$dl(
        class = "mb-0",
        shiny$tags$dt("Scale & Center"),
        shiny$tags$dd(
          class = "ms-2 mb-1",
          "Z-score standardization (mean=0, SD=1).",
          " Best when variables have different",
          " units or magnitudes."
        ),
        shiny$tags$dt("Center only"),
        shiny$tags$dd(
          class = "ms-2 mb-1",
          "Subtract mean, keep original variance.",
          " Use when all variables share the same",
          " unit and variance differences matter."
        ),
        shiny$tags$dt("No scaling"),
        shiny$tags$dd(
          class = "ms-2 mb-0",
          "Use raw data. Only if data is already",
          " preprocessed or on the same scale."
        )
      )
    )
  )
}

#' Server logic for the Cluster data selection sidebar tab
#'
#' Populates metaData with descriptive columns and
#' measureVar with measurement columns using column_utils
#' naming conventions. GroupBiplot choices come from
#' selected metaData.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param input_data Reactive returning the current data frame
#' @param data_version Reactive returning the data version counter
#' @export
tab_server <- function(input, output, session,
                       input_data, data_version) {
  # Smart retention on new data: keep selections that
  # still exist in the new dataset
  shiny$observeEvent(data_version(), {
    data <- input_data()
    if (is.null(data)) {
      rhino$log$info(
        "Cluster data_selection: reset (no data)"
      )
      shiny$updateSelectizeInput(
        session, "metaData",
        choices = character(0),
        selected = character(0)
      )
      shiny$updateSelectizeInput(
        session, "measureVar",
        choices = character(0),
        selected = character(0)
      )
      shiny$updateSelectizeInput(
        session, "groupBiplot",
        choices = character(0),
        selected = character(0)
      )
      return()
    }

    desc_cols <- column_utils$get_descriptive_cols(data)
    meas_cols <- column_utils$get_measurement_cols(data)

    cur_meta <- shiny$isolate(input$metaData)
    cur_meas <- shiny$isolate(input$measureVar)
    cur_grp  <- shiny$isolate(input$groupBiplot)

    ret_meta <- intersect(cur_meta, desc_cols)
    ret_meas <- intersect(cur_meas, meas_cols)
    ret_grp  <- intersect(cur_grp, ret_meta)

    rhino$log$info(
      "Cluster data_selection: ",
      "{length(desc_cols)} descriptive, ",
      "{length(meas_cols)} measurement cols"
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
      session, "groupBiplot",
      choices = ret_meta, selected = ret_grp
    )
  }, ignoreInit = TRUE)

  # Update metaData choices when data changes
  shiny$observe({
    data <- input_data()
    if (is.null(data)) return()
    cols <- column_utils$get_descriptive_cols(data)
    shiny$updateSelectizeInput(
      session, "metaData",
      choices = cols,
      selected = input$metaData[
        input$metaData %in% cols
      ]
    )
  })

  # Update measureVar choices when data changes
  shiny$observe({
    data <- input_data()
    if (is.null(data)) return()
    cols <- column_utils$get_measurement_cols(data)
    shiny$updateSelectizeInput(
      session, "measureVar",
      choices = cols,
      selected = input$measureVar[
        input$measureVar %in% cols
      ]
    )
  })

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

  # Update GroupBiplot choices from selected metaData
  shiny$observe({
    selected_meta <- input$metaData
    if (is.null(selected_meta)) {
      selected_meta <- character(0)
    }
    shiny$updateSelectizeInput(
      session, "groupBiplot",
      choices = selected_meta,
      selected = input$groupBiplot[
        input$groupBiplot %in% selected_meta
      ]
    )
  })
}
