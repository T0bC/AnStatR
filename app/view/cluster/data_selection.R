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
            "Choose whether to cluster on raw",
            "measurement data, PCA scores",
            "(reduced dimensionality), or LDA",
            "scores (discriminant axes).",
            "PCA/LDA scores are recommended",
            "after dimension reduction."
          )
        )
      ),
      choices = list(
        "Raw Data" = "raw",
        "PCA Scores" = "pca_scores",
        "LDA Scores" = "lda_scores"
      ),
      selected = "raw"
    ),
    shiny$uiOutput(ns("data_source_hint")),
    shiny$tags$hr(),
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
            "before clustering. Scaling ensures",
            "variables with different units",
            "contribute equally. Not needed when",
            "using PCA or LDA scores."
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
              "on clustering results. Verify in Load Data",
              "\u2192 Data Preview."
            )
          )
        ),
        value = TRUE
      )
    )
  )
}

#' Server logic for the Cluster data selection sidebar tab
#'
#' Populates metaData with descriptive columns and
#' measureVar with measurement columns using column_utils
#' naming conventions. GroupBiplot choices come from
#' selected metaData. Supports switching between raw data,
#' PCA scores, and LDA scores as data source.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param input_data Reactive returning the current data frame
#' @param data_version Reactive returning the data version counter
#' @param pca_scores_data Reactive returning PCA scores data
#'   frame (metadata + Dim.1, Dim.2, …) or NULL
#' @param lda_scores_data Reactive returning LDA scores data
#'   frame (metadata + LD1, LD2, …) or NULL
#' @param pca_result Reactive returning the full PCA result
#'   (with $eig table for variance recommendation) or NULL
#' @export
tab_server <- function(input, output, session,
                       input_data, data_version,
                       pca_scores_data = NULL,
                       lda_scores_data = NULL,
                       pca_result = NULL) {
  # Helper: get the active data frame for column detection
  active_data <- shiny$reactive({
    src <- input$data_source
    if (
      !is.null(src) &&
      src == "pca_scores" &&
      !is.null(pca_scores_data)
    ) {
      pca_scores_data()
    } else if (
      !is.null(src) &&
      src == "lda_scores" &&
      !is.null(lda_scores_data)
    ) {
      lda_scores_data()
    } else {
      input_data()
    }
  })

  # Show hint when PCA/LDA Scores is selected
  output$data_source_hint <- shiny$renderUI({
    src <- input$data_source
    if (is.null(src) || src == "raw") return(NULL)

    if (src == "pca_scores") {
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
          column_utils$get_descriptive_cols(
            pca_data
          )
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
    } else if (src == "lda_scores") {
      lda_data <- if (!is.null(lda_scores_data)) {
        lda_scores_data()
      }
      if (is.null(lda_data)) {
        # Distinguish: no result at all vs result
        # without scores (QDA / LOO-CV)
        lda_scores_unavailable_hint()
      } else {
        shiny$tags$div(
          class = paste(
            "alert alert-success py-1",
            "px-2 small mb-2"
          ),
          bsicons$bs_icon(
            "check-circle", class = "me-1"
          ),
          paste0(
            "LDA scores loaded: ",
            ncol(lda_data) - length(
              column_utils$get_descriptive_cols(
                lda_data
              )
            ),
            " discriminant axes, ",
            nrow(lda_data), " observations."
          )
        )
      }
    }
  })

  # When data_source changes, repopulate column selectors
  shiny$observeEvent(input$data_source, {
    data <- active_data()
    if (is.null(data)) return()

    desc_cols <- column_utils$get_descriptive_cols(data)
    meas_cols <- column_utils$get_measurement_cols(data)

    rhino$log$info(
      "Cluster data_selection:",
      " source='{input$data_source}',",
      " {length(desc_cols)} descriptive,",
      " {length(meas_cols)} measurement cols"
    )

    shiny$updateSelectizeInput(
      session, "metaData",
      choices = desc_cols,
      selected = desc_cols
    )
    # For PCA/LDA scores, pre-select all dims
    sel_meas <- if (
      input$data_source %in% c("pca_scores", "lda_scores")
    ) {
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
      session, "groupBiplot",
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

  # Update GroupBiplot choices from selected metaData
  shiny$observe({
    selected_meta <- input$metaData
    if (is.null(selected_meta)) {
      selected_meta <- character(0)
    }
    cluster_option <- "CLUSTER"
    all_choices <- unique(c(selected_meta, cluster_option))
    shiny$updateSelectizeInput(
      session, "groupBiplot",
      choices = all_choices,
      selected = input$groupBiplot[
        input$groupBiplot %in% all_choices
      ]
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

#' Build the hint UI for when LDA scores are unavailable
#'
#' Returns an alert explaining why LDA scores cannot be
#' used: either LDA has not been run, or the analysis type
#' (QDA) or validation mode (LOO-CV) does not produce
#' linear discriminant scores.
#'
#' @return shiny tag
lda_scores_unavailable_hint <- function() {
  shiny$tags$div(
    class = "alert alert-warning py-1 px-2 small mb-2",
    bsicons$bs_icon(
      "exclamation-triangle", class = "me-1"
    ),
    shiny$tags$strong("LDA scores not available."),
    shiny$tags$br(),
    "Possible reasons:",
    shiny$tags$ul(
      class = "mb-1 ps-3",
      shiny$tags$li(
        "LDA has not been run yet."
      ),
      shiny$tags$li(
        paste(
          "QDA was used instead of LDA.",
          "QDA does not produce linear",
          "discriminant scores."
        )
      ),
      shiny$tags$li(
        paste(
          "LOO cross-validation mode was used.",
          "Only model-fitting mode produces",
          "projection scores."
        )
      )
    ),
    "Run LDA (not QDA) in model-fitting mode",
    " (not LOO-CV) in the LDA tab to generate",
    " LD scores for clustering."
  )
}
