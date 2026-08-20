box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
  stats,
)

box::use(
  app/view/components/sidebar_tabs,
  app/view/shared/tuning_controls[
    check_cv_settings, estimate_cv_runtime, parse_keepx_grid,
    render_cv_advice, render_runtime_estimate
  ],
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
    # Analysis type: LDA vs QDA vs MDA vs PLS-DA
    shiny$radioButtons(
      inputId = ns("analysis_type"),
      label = shiny$tags$span(
        "Analysis Type ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "LDA assumes equal covariance matrices",
            "across groups. QDA allows each group",
            "to have its own covariance matrix.",
            "MDA models each group as a mixture",
            "of Gaussians for flexible boundaries.",
            "QDA/MDA require more observations.",
            "PLS-DA/sPLS-DA work even when there are",
            "more measurement variables than specimens",
            "and handle collinear variables natively;",
            "sPLS-DA additionally performs sparse",
            "variable selection."
          )
        )
      ),
      choices = list(
        "LDA (Linear)" = "lda",
        "QDA (Quadratic)" = "qda",
        "MDA (Mixture)" = "mda",
        "PLS-DA" = "plsda",
        "sPLS-DA (sparse)" = "splsda"
      ),
      selected = "lda"
    ),
    shiny$tags$hr(),
    # Estimation method (LDA only)
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("analysis_type"),
        "'] == 'lda'"
      ),
      shiny$selectInput(
        inputId = ns("method"),
        label = shiny$tags$span(
          "Estimation Method ",
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            ),
            paste(
              "'moment': standard estimators of",
              "mean and variance.",
              "'mle': maximum likelihood estimators.",
              "'mve': minimum volume ellipsoid",
              "(robust).",
              "'t': robust estimates based on a",
              "t-distribution."
            )
          )
        ),
        choices = list(
          "Moment (standard)" = "moment",
          "MLE" = "mle",
          "MVE (robust)" = "mve",
          "t-distribution (robust)" = "t"
        ),
        selected = "moment"
      )
    ),
    # QDA method (QDA only — subset of LDA methods)
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("analysis_type"),
        "'] == 'qda'"
      ),
      shiny$selectInput(
        inputId = ns("qda_method"),
        label = shiny$tags$span(
          "Estimation Method ",
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            ),
            paste(
              "'moment': standard estimators of",
              "mean and variance.",
              "'mle': maximum likelihood estimators.",
              "'mve': minimum volume ellipsoid",
              "(robust).",
              "'t': robust estimates based on a",
              "t-distribution."
            )
          )
        ),
        choices = list(
          "Moment (standard)" = "moment",
          "MLE" = "mle",
          "MVE (robust)" = "mve",
          "t-distribution (robust)" = "t"
        ),
        selected = "moment"
      )
    ),
    # MDA settings (MDA only)
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("analysis_type"),
        "'] == 'mda'"
      ),
      shiny$numericInput(
        inputId = ns("mda_subclasses"),
        label = shiny$tags$span(
          "Subclasses per group ",
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            ),
            paste(
              "MDA models each group as a mixture",
              "of Gaussian sub-populations.",
              "1 = equivalent to standard LDA.",
              "Higher values capture multi-modal",
              "or non-elliptical group shapes but",
              "need enough observations per group.",
              "The app enforces a hard minimum of",
              "max(subclasses, p + 1) observations per",
              "group; around 10 per subclass is",
              "recommended for stable estimates.",
              "Changing this also changes the",
              "discriminant coordinate system."
            )
          )
        ),
        value = 3,
        min = 1,
        max = 20,
        step = 1
      ),
      shiny$numericInput(
        inputId = ns("mda_iter"),
        label = shiny$tags$span(
          "Max EM iterations ",
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            ),
            paste(
              "Maximum iterations of the",
              "Expectation-Maximisation algorithm",
              "that estimates subclass memberships.",
              "Low values (3-5) give quick fits;",
              "increase to 20-50 if results seem",
              "unstable or deviance is still",
              "decreasing between runs."
            )
          )
        ),
        value = 5,
        min = 1,
        max = 100,
        step = 1
      )
    ),
    # PLS-DA / sPLS-DA settings
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("analysis_type"),
        "'] == 'plsda' || input['",
        ns("analysis_type"), "'] == 'splsda'"
      ),
      shiny$numericInput(
        inputId = ns("plsda_ncomp"),
        label = shiny$tags$span(
          "Number of components ",
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            ),
            paste(
              "Number of latent components to",
              "extract. Defaults to (number of",
              "groups - 1), matching LDA's LD axis",
              "count. Use the Component Diagnostics",
              "panel (perf) to check whether fewer",
              "or more components minimise",
              "cross-validated error."
            )
          )
        ),
        value = 2,
        min = 1,
        max = 20,
        step = 1
      ),
      # sPLS-DA: per-component keepX + auto-tune
      shiny$conditionalPanel(
        condition = paste0(
          "input['", ns("analysis_type"), "'] == 'splsda'"
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
              "Think of it as: how many parameters do I",
              "want to report for this component?",
              "Too small and real contributors are cut;",
              "too large and sPLS-DA drifts back toward",
              "plain PLS-DA with no useful shortlist.",
              "Do not guess: click Optimise variable",
              "selection to let cross-validation choose,",
              "then adjust only if you need a shorter",
              "list for practical reasons."
            )
          )
        ),
        shiny$uiOutput(ns("plsda_keepx_inputs")),
        shiny$actionButton(
          inputId = ns("tune_keepx_button"),
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
            "can still edit them, then press Compute to",
            "apply them. Until this has run, results are",
            "marked \"keepX not tuned\"."
          )
        )
      )
    ),
    # Prior probabilities (not applicable to PLS-DA/sPLS-DA)
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("analysis_type"),
        "'] != 'plsda' && input['",
        ns("analysis_type"), "'] != 'splsda'"
      ),
      shiny$radioButtons(
        inputId = ns("prior"),
        label = shiny$tags$span(
          "Prior Probabilities ",
          bslib$tooltip(
            bsicons$bs_icon(
              "info-circle", class = "text-muted"
            ),
            paste(
              "Proportional: uses class proportions",
              "from the training set.",
              "Equal: assigns equal probability to",
              "each group."
            )
          )
        ),
        choices = list(
          "Proportional (default)" = "proportional",
          "Equal" = "equal"
        ),
        selected = "proportional"
      )
    ),
    # Validation method
    shiny$radioButtons(
      inputId = ns("validation_method"),
      label = shiny$tags$span(
        "Validation ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Decides whether the reported accuracy",
            "is a real performance estimate or just",
            "the model grading itself.",
            "None: fits on all data and scores the",
            "same specimens - fast for exploring,",
            "but always optimistic; do not report",
            "it. LOO-CV: predicts each specimen",
            "with a model trained without it -",
            "uses all your data, best choice for",
            "LDA/QDA/MDA and for small groups.",
            "Train/Test Split: fits on part of the",
            "data and scores the held-out rest -",
            "the strictest check, and the option",
            "to use for PLS-DA/sPLS-DA, where",
            "LOO-CV would be too slow.",
            "Whichever you pick, the Summary panel",
            "shows both figures so you can see the",
            "overfitting gap."
          )
        )
      ),
      choices = list(
        "None (fit only)" = "none",
        "Leave-one-out CV" = "loo_cv",
        "Train / Test Split" = "split"
      ),
      selected = "none"
    ),
    # Train/test split settings
    shiny$conditionalPanel(
      condition = paste0(
        "input['", ns("validation_method"),
        "'] == 'split'"
      ),
      shiny$tags$div(
        class = "ms-2 ps-2 border-start",
        shiny$sliderInput(
          inputId = ns("train_fraction"),
          label = shiny$tags$span(
            "Training set size ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              ),
              paste(
                "Fraction of data used for training.",
                "The rest is held out for testing.",
                "Split is stratified by the grouping",
                "variable to preserve class proportions."
              )
            )
          ),
          min = 0.5,
          max = 0.9,
          value = 0.7,
          step = 0.05,
          post = ""
        ),
        shiny$numericInput(
          inputId = ns("split_seed"),
          label = shiny$tags$span(
            "Random seed ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              ),
              paste(
                "Set a seed for reproducible splits.",
                "Use the same seed to get the same",
                "train/test partition each time."
              )
            )
          ),
          value = 42,
          min = 1,
          step = 1
        )
      )
    ),
    shiny$tags$hr(),
    # Advanced settings (collapsed)
    bslib$accordion(
      id = ns("advanced_accordion"),
      open = FALSE,
      bslib$accordion_panel(
        title = shiny$tags$small(
          class = "text-muted",
          "Advanced settings"
        ),
        value = "advanced_settings",
        # Tolerance
        shiny$numericInput(
          inputId = ns("tol"),
          label = shiny$tags$span(
            "Tolerance ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle",
                class = "text-muted"
              ),
              paste(
                "Tolerance for singularity detection.",
                "Variables whose variance is less",
                "than tol^2 will be rejected."
              )
            )
          ),
          value = 1.0e-4,
          min = 0,
          max = 1,
          step = 1.0e-5
        ),
        # Nu (degrees of freedom, only for method = "t")
        shiny$conditionalPanel(
          condition = paste0(
            "(input['", ns("analysis_type"),
            "'] == 'lda' && input['",
            ns("method"), "'] == 't') || ",
            "(input['", ns("analysis_type"),
            "'] == 'qda' && input['",
            ns("qda_method"), "'] == 't')"
          ),
          shiny$numericInput(
            inputId = ns("nu"),
            label = shiny$tags$span(
              "Nu (degrees of freedom) ",
              bslib$tooltip(
                bsicons$bs_icon(
                  "info-circle",
                  class = "text-muted"
                ),
                paste(
                  "Degrees of freedom for the",
                  "t-distribution method.",
                  "Lower values give more robust",
                  "estimates. Typical range: 3-10."
                )
              )
            ),
            value = 5,
            min = 1,
            max = 100,
            step = 1
          )
        ),
        # PLS-DA component diagnostics (perf) settings
        shiny$conditionalPanel(
          condition = paste0(
            "input['", ns("analysis_type"),
            "'] == 'plsda' || input['",
            ns("analysis_type"), "'] == 'splsda'"
          ),
          shiny$tags$hr(),
          shiny$fluidRow(
            shiny$column(
              6,
              shiny$numericInput(
                inputId = ns("perf_folds"),
                label = "CV folds",
                value = 5, min = 2, max = 20, step = 1
              )
            ),
            shiny$column(
              6,
              shiny$numericInput(
                inputId = ns("perf_repeats"),
                label = "CV repeats",
                value = 10, min = 1, max = 50, step = 1
              )
            )
          ),
          # sPLS-DA only: the candidate grid the keepX tuning
          # button searches over.
          shiny$conditionalPanel(
            condition = paste0(
              "input['", ns("analysis_type"), "'] == 'splsda'"
            ),
            shiny$textInput(
              inputId = ns("tune_keepx_grid"),
              label = shiny$tags$span(
                "keepX values to test ",
                bslib$tooltip(
                  bsicons$bs_icon(
                    "info-circle", class = "text-muted"
                  ),
                  paste(
                    "The candidate variable counts",
                    "cross-validation will choose between,",
                    "comma-separated. Only these values can be",
                    "selected, so include the range you",
                    "consider plausible. Values above the",
                    "number of available variables are capped.",
                    "Leave blank for the default grid."
                  )
                )
              ),
              value = "5, 10, 15, 20, 30",
              placeholder = "5, 10, 15, 20, 30"
            ),
            shiny$uiOutput(ns("tune_keepx_runtime"))
          ),
          shiny$actionButton(
            inputId = ns("run_perf_button"),
            label = shiny$tags$span(
              bsicons$bs_icon(
                "clipboard-data", class = "me-1"
              ),
              "Check component count (recommended)"
            ),
            class = "btn-outline-secondary btn-sm w-100"
          ),
          shiny$uiOutput(ns("perf_runtime")),
          shiny$tags$small(
            class = "text-muted d-block mt-1",
            paste(
              "Answers: am I using the right number of",
              "components? Estimates classification error",
              "per component by repeated cross-validation,",
              "so you can see where adding components",
              "stops helping. Also reports the error under",
              "all three prediction distances, so you can",
              "see whether that choice matters. Does not",
              "change the fitted model — if it suggests a",
              "different count, set Number of components",
              "above and press Compute again. Requires a",
              "fitted model."
            )
          )
        )
      )
    )
  )
}

#' Server logic for the LDA analysis settings sidebar tab
#'
#' Handles analysis type and parameter settings.
#' Resets to defaults when new data is loaded.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param data_version Reactive returning the data version counter
#' @param input_data Reactive returning the current raw data frame
#'   (used to count groups for the PLS-DA ncomp default)
#' @param pca_scores_data Reactive returning the PCA scores data
#'   frame, or NULL — used instead of input_data when
#'   data_source is "pca_scores"
#' @export
tab_server <- function(input, output, session,
                       data_version,
                       input_data = NULL,
                       pca_scores_data = NULL) {
  # Active data source (raw or PCA scores), mirroring
  # data_selection.R's active_data reactive
  active_data <- shiny$reactive({
    if (
      !is.null(input$data_source) &&
      input$data_source == "pca_scores" &&
      !is.null(pca_scores_data)
    ) {
      pca_scores_data()
    } else if (!is.null(input_data)) {
      input_data()
    } else {
      NULL
    }
  })
  # Up-front runtime estimates, so the cost of these opt-in
  # cross-validation runs is visible before committing to one.
  output$tune_keepx_runtime <- shiny$renderUI({
    data <- active_data()
    measure_cols <- input$measureVar
    if (is.null(data) || length(measure_cols) == 0) return(NULL)

    grid <- parse_keepx_grid(
      input$tune_keepx_grid, length(measure_cols)
    )$values %||% unique(pmin(
      length(measure_cols), c(5, 10, 15, 20, 30)
    ))

    render_runtime_estimate(estimate_cv_runtime(
      n_samples = nrow(data),
      n_vars = length(measure_cols),
      folds = input$perf_folds %||% 5,
      repeats = input$perf_repeats %||% 10,
      n_grid = length(grid),
      ncomp = input$plsda_ncomp %||% 2,
      method = "splsda"
    ))
  })

  output$perf_runtime <- shiny$renderUI({
    data <- active_data()
    measure_cols <- input$measureVar
    if (is.null(data) || length(measure_cols) == 0) return(NULL)

    folds <- input$perf_folds %||% 5
    repeats <- input$perf_repeats %||% 10

    # perf() refits the existing model rather than searching a
    # grid, so n_grid stays 1.
    shiny$tagList(
      render_runtime_estimate(estimate_cv_runtime(
        n_samples = nrow(data),
        n_vars = length(measure_cols),
        folds = folds,
        repeats = repeats,
        n_grid = 1,
        ncomp = input$plsda_ncomp %||% 2,
        method = "perf"
      )),
      render_cv_advice(check_cv_settings(
        n_samples = nrow(data),
        folds = folds,
        repeats = repeats
      ))
    )
  })

  shiny$observeEvent(data_version(), {
    rhino$log$info(
      "LDA analysis_settings: reset for new data"
    )
    shiny$updateRadioButtons(
      session, "analysis_type", selected = "lda"
    )
    shiny$updateSelectInput(
      session, "method", selected = "moment"
    )
    shiny$updateSelectInput(
      session, "qda_method", selected = "moment"
    )
    shiny$updateRadioButtons(
      session, "prior", selected = "proportional"
    )
    shiny$updateRadioButtons(
      session, "validation_method", selected = "none"
    )
    shiny$updateSliderInput(
      session, "train_fraction", value = 0.7
    )
    shiny$updateNumericInput(
      session, "split_seed", value = 42
    )
    shiny$updateNumericInput(
      session, "tol", value = 1.0e-4
    )
    shiny$updateNumericInput(
      session, "nu", value = 5
    )
    shiny$updateNumericInput(
      session, "mda_subclasses", value = 3
    )
    shiny$updateNumericInput(
      session, "mda_iter", value = 5
    )
    shiny$updateNumericInput(
      session, "plsda_ncomp", value = 2
    )
    shiny$updateNumericInput(
      session, "perf_folds", value = 5
    )
    shiny$updateNumericInput(
      session, "perf_repeats", value = 10
    )
  }, ignoreInit = TRUE)

  # Default ncomp to (n_groups - 1) when the grouping
  # column changes, mirroring LDA's LD axis count.
  # Only applied automatically the first time a grouping
  # column is picked for the current dataset — subsequent
  # manual edits to plsda_ncomp are never overwritten here.
  ncomp_auto_set <- shiny$reactiveVal(FALSE)

  shiny$observeEvent(data_version(), {
    ncomp_auto_set(FALSE)
  }, ignoreInit = TRUE)

  shiny$observeEvent(input$groupingCol, {
    grp <- input$groupingCol
    if (is.null(grp) || grp == "") return()
    if (isTRUE(ncomp_auto_set())) return()

    data <- active_data()
    if (is.null(data) || !grp %in% names(data)) return()

    n_groups <- length(unique(stats$na.omit(data[[grp]])))
    default_ncomp <- max(1, n_groups - 1)

    shiny$updateNumericInput(
      session, "plsda_ncomp", value = default_ncomp
    )
    ncomp_auto_set(TRUE)
  }, ignoreInit = TRUE)

  # PLS-DA/sPLS-DA have no native LOO-CV fitting mode.
  # Component-count/error diagnostics are instead available
  # via the dedicated perf() panel, so hide the LOO-CV choice
  # and fall back to "None" if it was previously selected.
  shiny$observeEvent(input$analysis_type, {
    is_plsda <- input$analysis_type %in% c("plsda", "splsda")
    choices <- if (is_plsda) {
      list(
        "None (fit only)" = "none",
        "Train / Test Split" = "split"
      )
    } else {
      list(
        "None (fit only)" = "none",
        "Leave-one-out CV" = "loo_cv",
        "Train / Test Split" = "split"
      )
    }
    current <- input$validation_method
    selected <- if (is_plsda && identical(current, "loo_cv")) {
      "none"
    } else {
      current %||% "none"
    }
    shiny$updateRadioButtons(
      session, "validation_method",
      choices = choices, selected = selected
    )
  }, ignoreInit = TRUE)

  # Dynamic per-component keepX numeric inputs (sPLS-DA)
  output$plsda_keepx_inputs <- shiny$renderUI({
    ncomp <- input_num(input$plsda_ncomp, 2)
    if (ncomp < 1) return(NULL)
    n_vars <- length(input$measureVar)
    default_keep <- if (n_vars > 0) min(10, n_vars) else 10

    shiny$tagList(
      lapply(seq_len(ncomp), function(i) {
        current <- input[[paste0("keepx_", i)]]
        shiny$numericInput(
          inputId = session$ns(paste0("keepx_", i)),
          label = paste0("Comp", i),
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
}


# =============================================================================
# Local helpers (not exported)
# =============================================================================

#' Coalesce a numeric Shiny input to a default
#'
#' Unlike `%||%`, also falls back when the input is NA — which
#' numericInput can transiently send while its DOM element is
#' being rebuilt by a renderUI() (e.g. when ncomp changes).
#'
#' @param value The input value (may be NULL or NA)
#' @param default Fallback numeric value
#' @return Numeric, never NULL or NA
input_num <- function(value, default) {
  if (is.null(value) || is.na(value)) default else value
}
