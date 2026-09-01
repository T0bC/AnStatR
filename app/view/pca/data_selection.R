box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
)

box::use(
  app/logic/shared/column_utils,
  app/view/components/sidebar_tabs,
  app/view/shared/recommendation_banner,
)

#' @export
tab_ui <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "table",
    tooltip_text = "Data Selection",
    value = "data_tab",
    shiny$h6(class = "text-muted mb-3", "Data Selection"),
    shiny$helpText(
      paste(
        "Select the correct columns for the PCA.",
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
    shiny$uiOutput(ns("recommended_hint")),
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
              "to include in the PCA analysis.",
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
          "before PCA. Scaling ensures variables",
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
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("analysis_type"), "'] == 'ipca'"
      ),
      shiny$tags$div(
        class = "alert alert-secondary py-2 small mb-2",
        bsicons$bs_icon(
          "info-circle-fill", class = "me-1"
        ),
        paste(
          "IPCA always centers data internally",
          "(mixOmics has no option to disable this);",
          "\"Center only\" and \"No scaling\" behave",
          "identically for IPCA. Only the",
          "\"Scale & Center\" vs. non-scaled choice",
          "has an effect."
        )
      )
    ),
    bslib$accordion(
      id = ns("scaling_help_accordion"),
      open = FALSE,
      bslib$accordion_panel(
        title = shiny$tags$small(
          class = "text-muted",
          "Scaling method details"
        ),
        value = "scaling_details",
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
    ),
    shiny$tags$hr(),
    shiny$tags$label(
      class = "control-label",
      "Residualize by (optional) ",
      bslib$tooltip(
        bsicons$bs_icon(
          "info-circle", class = "text-muted"
        ),
        paste(
          "Remove a known confound before PCA by subtracting",
          "each group's mean from every measurement column.",
          "Use this when a metadata variable (e.g.",
          "site/location) is expected to dominate the",
          "measurements and mask the signal you actually",
          "care about. Check the Eigencorrelation plot first",
          "to see which metadata variable correlates most",
          "strongly with the top components — that is",
          "usually the one to residualize by. Applied before",
          "scaling, on the original units."
        )
      )
    ),
    shiny$selectizeInput(
      inputId = ns("residualizeCol"),
      label = NULL,
      choices = NULL,
      multiple = TRUE,
      options = list(
        maxItems = 1,
        placeholder = "None (use raw measurements)"
      )
    ),
    shiny$tags$hr(),
    shiny$checkboxInput(
      inputId = ns("correct_skewness"),
      label = shiny$tags$span(
        "Normalize skewed variables ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Transform highly skewed variables",
            "(|skewness| > 2) using bestNormalize.",
            "This reduces the influence of extreme",
            "outliers but changes the data distribution.",
            "Only enable if outliers are measurement",
            "errors, not real signal."
          )
        )
      ),
      value = FALSE
    )
  )
}

#' Server logic for the PCA data selection sidebar tab
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
#' @param recommended_parameters Reactive returning a character vector of
#'   parameter names recommended by the Statistics screening ranking, or
#'   NULL
#' @export
tab_server <- function(input, output, session,
                       input_data, data_version,
                       recommended_parameters = NULL) {
  # --- Recommended-parameters hint + apply button ---
  output$recommended_hint <- shiny$renderUI({
    if (is.null(recommended_parameters)) return(NULL)
    rec <- recommended_parameters()
    if (length(rec) == 0) return(NULL)
    recommendation_banner$render_recommendation_banner(
      rec, session$ns, "apply_recommended"
    )
  })

  shiny$observeEvent(input$apply_recommended, {
    data <- input_data()
    if (is.null(data) || is.null(recommended_parameters)) return()
    rec <- recommended_parameters()
    cols <- column_utils$get_measurement_cols(data)
    sel <- intersect(rec, cols)
    shiny$updateSelectizeInput(
      session, "measureVar",
      choices = cols, selected = sel
    )
    if (length(sel) < length(rec)) {
      dropped <- setdiff(rec, cols)
      shiny$showNotification(
        paste0(
          length(dropped), " recommended parameter(s) not found in ",
          "the current data and were skipped: ",
          paste(dropped, collapse = ", ")
        ),
        type = "warning"
      )
    }
    rhino$log$info(
      "PCA data_selection: applied {length(sel)}/{length(rec)} ",
      "recommended parameter(s)"
    )
  })

  # Smart retention on new data: keep selections that
  # still exist in the new dataset
  shiny$observeEvent(data_version(), {
    data <- input_data()
    if (is.null(data)) {
      rhino$log$info(
        "PCA data_selection: reset (no data)"
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
        session, "GroupBiplot",
        choices = character(0),
        selected = character(0)
      )
      shiny$updateSelectizeInput(
        session, "residualizeCol",
        choices = character(0),
        selected = character(0)
      )
      return()
    }

    desc_cols <- column_utils$get_descriptive_cols(data)
    meas_cols <- column_utils$get_measurement_cols(data)

    cur_meta <- shiny$isolate(input$metaData)
    cur_meas <- shiny$isolate(input$measureVar)
    cur_grp  <- shiny$isolate(input$GroupBiplot)
    cur_resid <- shiny$isolate(input$residualizeCol)

    ret_meta <- intersect(cur_meta, desc_cols)
    ret_meas <- intersect(cur_meas, meas_cols)
    ret_grp  <- intersect(cur_grp, ret_meta)
    ret_resid <- intersect(cur_resid, ret_meta)

    rhino$log$info(
      "PCA data_selection: ",
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
      session, "GroupBiplot",
      choices = ret_meta, selected = ret_grp
    )
    shiny$updateSelectizeInput(
      session, "residualizeCol",
      choices = ret_meta, selected = ret_resid
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

  # Update GroupBiplot choices from selected metaData (debounced)
  # When metadata columns are selected and GroupBiplot has no
  # selection yet, auto-select them so the biplot is colored and
  # ellipses are drawn by default.
  debounced_meta <- shiny$reactive({
    m <- input$metaData
    if (is.null(m)) character(0) else m
  }) |> shiny$debounce(500)

  shiny$observe({
    selected_meta <- debounced_meta()
    cur_grp <- shiny$isolate(input$GroupBiplot)
    retained_grp <- cur_grp[cur_grp %in% selected_meta]
    new_grp <- if (
      length(retained_grp) == 0 && length(selected_meta) > 0
    ) {
      selected_meta
    } else {
      retained_grp
    }
    shiny$updateSelectizeInput(
      session, "GroupBiplot",
      choices = selected_meta,
      selected = new_grp
    )
    cur_resid <- shiny$isolate(input$residualizeCol)
    shiny$updateSelectizeInput(
      session, "residualizeCol",
      choices = selected_meta,
      selected = cur_resid[cur_resid %in% selected_meta]
    )
  })
}
