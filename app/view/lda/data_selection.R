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
    icon = "table",
    tooltip_text = "Data Selection",
    value = "data_tab",
    shiny$h6(class = "text-muted mb-3", "Data Selection"),
    # Data source toggle
    shiny$radioButtons(
      inputId = ns("data_source"),
      label = shiny$tags$span(
        "Data Source ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Choose whether to run LDA/QDA on",
            "raw measurement data or on PCA",
            "scores (reduced dimensionality).",
            "PCA scores are recommended for",
            "high-dimensional data (40+ variables)."
          )
        )
      ),
      choices = list(
        "Raw Data" = "raw",
        "PCA Scores" = "pca_scores"
      ),
      selected = "raw"
    ),
    shiny$uiOutput(ns("pca_scores_hint")),
    shiny$tags$hr(),
    shiny$helpText(
      paste(
        "Select the correct columns for LDA/QDA.",
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
            "species, site, etc., that are",
            "important for your analysis."
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
    # Grouping column selection (required for LDA/QDA)
    shiny$selectizeInput(
      inputId = ns("groupingCol"),
      label = shiny$tags$span(
        shiny$tags$strong("Grouping column "),
        shiny$tags$span(
          class = "text-danger", "(required)"
        ),
        " ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Select the column that defines the",
            "groups (e.g., species). LDA/QDA",
            "maximizes separation between these",
            "groups. Must have at least 2 levels."
          )
        )
      ),
      choices = NULL,
      multiple = FALSE,
      options = list(
        placeholder = "Select grouping column..."
      )
    ),
    # Measurement columns selection
    shiny$selectizeInput(
      inputId = ns("measureVar"),
      label = shiny$tags$div(
        class = paste(
          "d-flex justify-content-between",
          "align-items-center"
        ),
        shiny$tags$span(
          "Measurement columns ",
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            ),
            paste(
              "Select columns that contain the",
              "actual measurements (numeric data)",
              "to include in the LDA/QDA analysis."
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
    # Data scaling options (only for raw data)
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("data_source"), "'] == 'raw'"
      ),
      shiny$tags$label(
        class = "control-label",
        "Data Scaling ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Choose how to preprocess the data",
            "before LDA/QDA. Scaling ensures",
            "variables with different units",
            "contribute equally. Not needed when",
            "using PCA scores."
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
                "Z-score standardization",
                " (mean=0, SD=1).",
                " Best when variables have different",
                " units or magnitudes."
              ),
              shiny$tags$dt("Center only"),
              shiny$tags$dd(
                class = "ms-2 mb-1",
                "Subtract mean, keep original",
                " variance.",
                " Use when all variables share the",
                " same unit and variance differences",
                " matter."
              ),
              shiny$tags$dt("No scaling"),
              shiny$tags$dd(
                class = "ms-2 mb-0",
                "Use raw data. Only if data is",
                " already preprocessed or on the",
                " same scale."
              )
            )
          )
        )
      ),
      shiny$tags$hr(),
      shiny$checkboxInput(
        inputId = ns("correct_skewness"),
        label = shiny$tags$span(
          "Correct skewed variables ",
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            ),
            paste(
              "Automatically detect and transform",
              "highly skewed variables (|skewness| > 1)",
              "using log or Box-Cox transformation.",
              "This reduces the influence of outliers",
              "on LDA/QDA results. Verify in Load Data",
              "\u2192 Data Preview."
            )
          )
        ),
        value = TRUE
      )
    )
  )
}

#' Server logic for the LDA data selection sidebar tab
#'
#' Populates metaData with descriptive columns and
#' measureVar with measurement columns using column_utils
#' naming conventions. groupingCol choices come from
#' selected metaData.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param input_data Reactive returning the current data frame
#' @param data_version Reactive returning the data version counter
#' @param pca_scores_data Reactive returning PCA scores data
#'   frame (metadata + Dim.1, Dim.2, …) or NULL
#' @param pca_result Reactive returning the full PCA result
#'   (with $eig table for variance recommendation) or NULL
#' @export
tab_server <- function(input, output, session,
                       input_data, data_version,
                       pca_scores_data = NULL,
                       pca_result = NULL) {
  # Helper: get the active data frame for column detection
  active_data <- shiny$reactive({
    if (
      !is.null(input$data_source) &&
      input$data_source == "pca_scores" &&
      !is.null(pca_scores_data)
    ) {
      pca_scores_data()
    } else {
      input_data()
    }
  })

  # Show hint when PCA Scores is selected but unavailable
  output$pca_scores_hint <- shiny$renderUI({
    if (
      is.null(input$data_source) ||
      input$data_source != "pca_scores"
    ) {
      return(NULL)
    }
    pca_data <- if (!is.null(pca_scores_data)) {
      pca_scores_data()
    }
    if (is.null(pca_data)) {
      shiny$tags$div(
        class = "alert alert-info py-1 px-2 small mb-2",
        bsicons$bs_icon(
          "info-circle", class = "me-1"
        ),
        "Run PCA first in the PCA tab, then",
        " return here to use PCA scores."
      )
    } else {
      n_dims <- ncol(pca_data) - length(
        column_utils$get_descriptive_cols(pca_data)
      )
      # Build variance recommendation
      rec <- pca_dims_recommendation(
        pca_result
      )
      shiny$tagList(
        shiny$tags$div(
          class = paste(
            "alert alert-success py-1",
            "px-2 small mb-2"
          ),
          bsicons$bs_icon(
            "check-circle", class = "me-1"
          ),
          paste0(
            "PCA scores loaded: ",
            n_dims, " dimensions, ",
            nrow(pca_data),
            " observations."
          )
        ),
        if (!is.null(rec)) {
          shiny$tags$div(
            class = paste(
              "alert alert-info py-1",
              "px-2 small mb-2"
            ),
            bsicons$bs_icon(
              "lightbulb", class = "me-1"
            ),
            shiny$tags$strong(
              "Recommendation: "
            ),
            paste0(
              "Select the first ",
              rec$n90, " dimensions",
              " for \u226590% variance (",
              rec$cum90, "%), or ",
              rec$n95,
              " for \u226595% (",
              rec$cum95, "%)."
            )
          )
        }
      )
    }
  })

  # When data_source changes, repopulate column selectors
  shiny$observeEvent(input$data_source, {
    data <- active_data()
    if (is.null(data)) return()

    desc_cols <- column_utils$get_descriptive_cols(data)
    meas_cols <- column_utils$get_measurement_cols(data)

    rhino$log$info(
      "LDA data_selection: source='{input$data_source}',",
      " {length(desc_cols)} descriptive,",
      " {length(meas_cols)} measurement cols"
    )

    shiny$updateSelectizeInput(
      session, "metaData",
      choices = desc_cols,
      selected = desc_cols
    )
    # For PCA scores, pre-select all dims
    sel_meas <- if (input$data_source == "pca_scores") {
      meas_cols
    } else {
      character(0)
    }
    shiny$updateSelectizeInput(
      session, "measureVar",
      choices = meas_cols,
      selected = sel_meas
    )
    shiny$updateSelectizeInput(
      session, "groupingCol",
      choices = desc_cols,
      selected = character(0)
    )
  }, ignoreInit = TRUE)

  # Smart retention on new data: keep selections that
  # still exist in the new dataset
  shiny$observeEvent(data_version(), {
    data <- input_data()
    if (is.null(data)) {
      rhino$log$info(
        "LDA data_selection: reset (no data)"
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
        session, "groupingCol",
        choices = character(0),
        selected = character(0)
      )
      return()
    }

    desc_cols <- column_utils$get_descriptive_cols(data)
    meas_cols <- column_utils$get_measurement_cols(data)

    cur_meta <- shiny$isolate(input$metaData)
    cur_meas <- shiny$isolate(input$measureVar)
    cur_grp  <- shiny$isolate(input$groupingCol)

    ret_meta <- intersect(cur_meta, desc_cols)
    ret_meas <- intersect(cur_meas, meas_cols)
    ret_grp  <- if (
      !is.null(cur_grp) && cur_grp %in% ret_meta
    ) cur_grp else character(0)

    rhino$log$info(
      "LDA data_selection: ",
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
      session, "groupingCol",
      choices = ret_meta, selected = ret_grp
    )
  }, ignoreInit = TRUE)

  # Update metaData choices when active data changes
  shiny$observe({
    data <- active_data()
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

  # Update measureVar choices when active data changes
  shiny$observe({
    data <- active_data()
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
    data <- active_data()
    if (is.null(data)) return()
    cols <- column_utils$get_measurement_cols(data)
    shiny$updateSelectizeInput(
      session, "measureVar",
      choices = cols, selected = cols
    )
  })

  # Update groupingCol choices from selected metaData
  shiny$observe({
    selected_meta <- input$metaData
    if (is.null(selected_meta)) {
      selected_meta <- character(0)
    }
    current_grp <- input$groupingCol
    sel <- if (
      !is.null(current_grp) &&
      current_grp %in% selected_meta
    ) current_grp else character(0)
    shiny$updateSelectizeInput(
      session, "groupingCol",
      choices = selected_meta,
      selected = sel
    )
  })
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Compute PCA dimension recommendation based on
#' cumulative variance thresholds (90% and 95%).
#'
#' @param pca_result Reactive returning the full PCA
#'   result or NULL
#' @return List with n90, cum90, n95, cum95 or NULL
pca_dims_recommendation <- function(pca_result) {
  if (is.null(pca_result)) return(NULL)
  pca_res <- tryCatch(
    pca_result(),
    error = function(e) NULL
  )
  if (
    is.null(pca_res) ||
    !isTRUE(pca_res$success) ||
    is.null(pca_res$result$eig)
  ) {
    return(NULL)
  }
  eig <- pca_res$result$eig
  cum_var <- eig[["cumulative.variance.percent"]]
  if (is.null(cum_var) || length(cum_var) == 0) {
    return(NULL)
  }
  n90 <- which(cum_var >= 90)[1]
  n95 <- which(cum_var >= 95)[1]
  if (is.na(n90)) n90 <- length(cum_var)
  if (is.na(n95)) n95 <- length(cum_var)
  list(
    n90 = n90,
    cum90 = round(cum_var[n90], 1),
    n95 = n95,
    cum95 = round(cum_var[n95], 1)
  )
}
