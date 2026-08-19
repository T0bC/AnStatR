box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
)

box::use(
  app/view/components/sidebar_tabs,
  app/logic/pca/pca[run_pca_tune_keepx],
)

#' @export
tab_ui <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "gear",
    tooltip_text = "Analysis Settings",
    value = "settings_tab",
    shiny$h6(
      class = "text-muted mb-3",
      "Analysis Settings"
    ),
    # Analysis type: PCA vs sPCA vs IPCA
    shiny$radioButtons(
      inputId = ns("analysis_type"),
      label = shiny$tags$span(
        "Analysis Type ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "PCA maximises variance explained per",
            "component using all variables.",
            "sPCA (sparse PCA) restricts each",
            "component to a chosen number of",
            "variables, making it easier to identify",
            "which variables drive each component.",
            "IPCA (independent PCA) finds",
            "statistically independent components",
            "via ICA rather than variance-ranked",
            "ones — a different lens on the same",
            "data, useful when sources of variation",
            "are expected to be independent rather",
            "than merely uncorrelated."
          )
        )
      ),
      choices = list(
        "PCA (Standard)" = "pca",
        "sPCA (Sparse)" = "spca",
        "IPCA (Independent)" = "ipca"
      ),
      selected = "pca"
    ),
    shiny$tags$hr(),
    # sPCA: per-component keepX + auto-tune
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("analysis_type"), "'] == 'spca'"
      ),
      shiny$numericInput(
        inputId = ns("spca_ncomp"),
        label = shiny$tags$span(
          "Number of components ",
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            ),
            paste(
              "Number of sparse components to",
              "extract. Each gets its own",
              "keepX below."
            )
          )
        ),
        value = 2,
        min = 1,
        max = 20,
        step = 1
      ),
      shiny$tags$label(
        class = "control-label",
        "Variables to keep per component ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "How many measurement variables each",
            "component is allowed to keep. This is a",
            "count of variables, not a threshold.",
            "Too small and real contributors are cut;",
            "too large and sPCA drifts back toward",
            "plain PCA with no useful shortlist.",
            "Do not guess: click Optimise variable",
            "selection to let cross-validation choose,",
            "then adjust only if you need a shorter",
            "list for practical reasons."
          )
        )
      ),
      shiny$uiOutput(ns("spca_keepx_inputs")),
      shiny$actionButton(
        inputId = ns("tune_spca_keepx_button"),
        label = shiny$tags$span(
          bsicons$bs_icon("magic", class = "me-1"),
          "Optimise variable selection (recommended)"
        ),
        class = "btn-outline-secondary btn-sm w-100 mt-1"
      ),
      shiny$tags$small(
        class = "text-muted d-block mt-1",
        paste(
          "Strongly recommended before reporting a",
          "variable list: cross-validation decides how",
          "many variables each component should keep,",
          "instead of you guessing. Takes seconds to a",
          "few minutes depending on data size. The",
          "suggested values fill the boxes above; you",
          "can still edit them, then press Compute PCA",
          "to apply them. Until this has run, results",
          "are marked \"keepX not tuned\"."
        )
      )
    ),
    # IPCA: number of components + ICA mode
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("analysis_type"), "'] == 'ipca'"
      ),
      shiny$numericInput(
        inputId = ns("ipca_ncomp"),
        label = shiny$tags$span(
          "Number of components ",
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            ),
            paste(
              "Number of independent components to",
              "extract via ICA."
            )
          )
        ),
        value = 2,
        min = 1,
        max = 20,
        step = 1
      ),
      shiny$selectInput(
        inputId = ns("ipca_mode"),
        label = shiny$tags$span(
          "ICA Algorithm ",
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            ),
            paste(
              "Deflation: extracts components one at a",
              "time (FastICA default, more stable for",
              "small samples). Parallel: extracts all",
              "components simultaneously (can be faster",
              "on larger datasets, sometimes less stable)."
            )
          )
        ),
        choices = list(
          "Deflation (recommended)" = "deflation",
          "Parallel" = "parallel"
        ),
        selected = "deflation"
      ),
      shiny$tags$div(
        class = "alert alert-secondary py-2 small",
        bsicons$bs_icon(
          "info-circle-fill", class = "me-1"
        ),
        paste(
          "IPCA components are not ranked by variance",
          "explained, unlike PCA/sPCA — component order",
          "is arbitrary. Contribution % and cos2 are not",
          "shown for IPCA results."
        )
      )
    )
  )
}

#' Server logic for the PCA analysis settings sidebar tab
#'
#' Handles analysis type and sPCA/IPCA parameter settings.
#' Resets to defaults when new data is loaded.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param data_version Reactive returning the data version counter
#' @param input_data Reactive returning the current raw data frame
#' @export
tab_server <- function(input, output, session,
                       data_version,
                       input_data = NULL) {
  # keepX values actually used to fit the last sPCA model,
  # compared against tune.spca()'s suggestion so an
  # untuned (or hand-edited) selection is never presented as
  # cross-validated. NULL = tuning never ran for this session.
  keepx_tuned <- shiny$reactiveVal(NULL)

  shiny$observeEvent(data_version(), {
    rhino$log$info(
      "PCA analysis_settings: reset for new data"
    )
    shiny$updateRadioButtons(
      session, "analysis_type", selected = "pca"
    )
    shiny$updateNumericInput(
      session, "spca_ncomp", value = 2
    )
    shiny$updateNumericInput(
      session, "ipca_ncomp", value = 2
    )
    shiny$updateSelectInput(
      session, "ipca_mode", selected = "deflation"
    )
    keepx_tuned(NULL)
  }, ignoreInit = TRUE)

  shiny$observeEvent(input$analysis_type, {
    keepx_tuned(NULL)
  }, ignoreInit = TRUE)

  # Dynamic per-component keepX numeric inputs (sPCA)
  output$spca_keepx_inputs <- shiny$renderUI({
    ncomp <- input_num(input$spca_ncomp, 2)
    if (ncomp < 1) return(NULL)
    n_vars <- length(input$measureVar)
    default_keep <- if (n_vars > 0) min(10, n_vars) else 10

    shiny$tagList(
      lapply(seq_len(ncomp), function(i) {
        current <- input[[paste0("spca_keepx_", i)]]
        shiny$numericInput(
          inputId = session$ns(paste0("spca_keepx_", i)),
          label = paste0("Dim", i),
          value = if (is.null(current) || is.na(current)) {
            default_keep
          } else {
            current
          },
          min = 1,
          max = max(n_vars, 1),
          step = 1
        )
      })
    )
  })

  # Tune keepX via cross-validation, fill the boxes above
  shiny$observeEvent(input$tune_spca_keepx_button, {
    if (input$analysis_type != "spca") return()

    data <- input_data()
    measure_cols <- input$measureVar
    if (is.null(data) || is.null(measure_cols) ||
        length(measure_cols) == 0) {
      return()
    }

    ncomp <- input_num(input$spca_ncomp, 2)
    scale_method <- input$scale_method
    do_center <- !is.null(scale_method) &&
      scale_method %in% c("scale_center", "center_only")
    do_scale <- !is.null(scale_method) &&
      scale_method == "scale_center"

    tune_res <- run_pca_tune_keepx(
      data, measure_cols, ncomp = ncomp,
      center = do_center, scale. = do_scale
    )

    if (!tune_res$success) {
      rhino$log$warn(
        "sPCA: keepX tuning failed — ",
        "{tune_res$error$message}"
      )
      return()
    }

    keep_x <- tune_res$result
    keepx_tuned(as.numeric(keep_x))
    for (i in seq_len(ncomp)) {
      shiny$updateNumericInput(
        session, paste0("spca_keepx_", i),
        value = as.numeric(keep_x[i])
      )
    }
  })

  list(keepx_tuned = keepx_tuned)
}


# =============================================================================
# Local helpers (not exported)
# =============================================================================

#' Coalesce a numeric Shiny input to a default
#'
#' @param value The input value (may be NULL or NA)
#' @param default Fallback numeric value
#' @return Numeric, never NULL or NA
input_num <- function(value, default) {
  if (is.null(value) || is.na(value)) default else value
}
