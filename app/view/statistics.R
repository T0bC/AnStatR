box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
)

box::use(
  app/logic/error_handling,
  app/logic/statistics,
  app/view/components/sidebar_tabs,
  app/view/error_display,
)

# --- Sidebar Tab 1: Options ---
options_tab <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "gear",
    tooltip_text = "Options",
    value = "options_tab",
    shiny$h6(class = "text-muted mb-3", "Options"),
    # Trim value display (read-only, synced from Plotting)
    shiny$uiOutput(ns("trim_value_display")),
    shiny$tags$hr(),
    # Output options
    shiny$fluidRow(
      shiny$column(
        6,
        shiny$checkboxInput(
          inputId = ns("show_additional_output"),
          label = shiny$tags$span(
            "Additional Output ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle", class = "text-muted"
              ),
              paste(
                "Show additional output like",
                "Linear Contrasts and",
                "Cliff's Delta tables."
              )
            )
          ),
          value = FALSE
        )
      ),
      shiny$column(
        6,
        shiny$checkboxInput(
          inputId = ns("use_scientific_notation"),
          label = shiny$tags$span(
            "Scientific Notation ",
            bslib$tooltip(
              bsicons$bs_icon(
                "info-circle", class = "text-muted"
              ),
              "Display results in scientific notation."
            )
          ),
          value = FALSE
        )
      )
    ),
    shiny$tags$hr(),
    # Statistical approach selection
    shiny$radioButtons(
      inputId = ns("test_approach"),
      label = "Statistical Approach:",
      choices = list(
        "Robust Tests" = "robust",
        "Parametric Tests" = "parametric"
      ),
      selected = "robust"
    ),
    # Expandable info about approaches
    bslib$accordion(
      id = ns("test_approach_accordion"),
      open = FALSE,
      bslib$accordion_panel(
        title = "When to use which approach?",
        value = "test_approach_info",
        shiny$uiOutput(ns("approach_details"))
      )
    ),
    shiny$tags$hr(),
    # Filter significant p-values
    shiny$checkboxInput(
      inputId = ns("filter_p_values"),
      label = shiny$tags$span(
        "Show only significant p-values ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Filter results for p-values < 0.07.",
            "Always check effect sizes when",
            "interpreting p-values!"
          )
        )
      ),
      value = FALSE
    ),
    # Valid comparisons (conditionally shown for multi-way)
    shiny$uiOutput(ns("valid_comparisons_ui"))
  )
}

# --- Sidebar Tab 2: Bootstrap ---
bootstrap_tab <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "arrow-repeat",
    tooltip_text = "Bootstrap",
    value = "bootstrap_tab",
    shiny$h6(class = "text-muted mb-3", "Bootstrap Options"),
    # Bootstrap toggle
    shiny$checkboxInput(
      inputId = ns("use_bootstrap"),
      label = shiny$tags$span(
        "Use bootstrap version ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Recommended when sample sizes are",
            "very small or unequal. Gives better",
            "approximation but takes longer."
          )
        )
      ),
      value = FALSE
    ),
    # Bootstrap options (shown when enabled)
    shiny$conditionalPanel(
      condition = "input.use_bootstrap == true",
      ns = ns,
      shiny$tags$div(
        class = "mt-3",
        shiny$fluidRow(
          shiny$column(
            6,
            shiny$numericInput(
              inputId = ns("boot_samples"),
              label = shiny$tags$span(
                "Bootstrap samples ",
                bslib$tooltip(
                  bsicons$bs_icon(
                    "info-circle",
                    class = "text-muted"
                  ),
                  paste(
                    "Number of bootstrap iterations.",
                    "Values over 599 rarely change",
                    "results significantly."
                  )
                )
              ),
              value = 599,
              min = 100,
              max = 10000,
              step = 100
            )
          ),
          shiny$column(
            6,
            shiny$numericInput(
              inputId = ns("boot_sample_size"),
              label = shiny$tags$span(
                "Samples per bootstrap ",
                bslib$tooltip(
                  bsicons$bs_icon(
                    "info-circle",
                    class = "text-muted"
                  ),
                  paste(
                    "Leave blank to default to the",
                    "smallest group size. Larger",
                    "values are capped automatically."
                  )
                )
              ),
              value = NA,
              min = 1
            )
          )
        )
      )
    ),
    # Info text
    shiny$tags$div(
      class = "small text-muted mt-3",
      shiny$tags$p(
        "Bootstrap methods are recommended when:",
        shiny$tags$ul(
          shiny$tags$li("Sample sizes are very small"),
          shiny$tags$li("Group sizes are unequal"),
          shiny$tags$li("Data is heavily skewed")
        )
      )
    )
  )
}

# --- Sidebar Tab 3: P-Value Adjustment ---
adjustments_tab <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "sliders2",
    tooltip_text = "P-Value Adjustment",
    value = "adjustments_tab",
    shiny$h6(class = "text-muted mb-3", "P-Value Adjustment"),
    # P-value correction method
    shiny$radioButtons(
      inputId = ns("p_val_cor_method"),
      label = shiny$tags$span(
        "Adjustment method ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          "Method to correct for multiple comparisons."
        )
      ),
      choices = c(
        "Holm" = "holm",
        "Hochberg" = "hochberg",
        "Hommel" = "hommel",
        "Bonferroni" = "bonferroni",
        "Benjamini-Hochberg (BH)" = "BH",
        "Benjamini-Yekutieli (BY)" = "BY",
        "FDR" = "fdr",
        "None" = "none"
      ),
      selected = "bonferroni"
    ),
    shiny$tags$hr(),
    # Help text
    shiny$tags$div(
      class = "small text-muted",
      shiny$tags$p(
        shiny$tags$strong("About p-value adjustment:")
      ),
      shiny$tags$p(
        "Corrects for multiple comparisons."
      ),
      shiny$tags$ul(
        shiny$tags$li(
          shiny$tags$strong("Bonferroni:"),
          " Most conservative method"
        ),
        shiny$tags$li(
          shiny$tags$strong("BH/BY:"),
          " Less conservative, controls FDR"
        ),
        shiny$tags$li(
          shiny$tags$strong("Holm:"),
          " Step-down, more powerful than Bonferroni"
        ),
        shiny$tags$li(
          shiny$tags$strong("None:"),
          " No adjustment (use with caution)"
        )
      ),
      shiny$tags$p(
        class = "text-warning",
        bsicons$bs_icon("exclamation-triangle"),
        " Don't be evil and do p-hacking!"
      )
    )
  )
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  sidebar_tabs$tab_layout(
    ns = ns,
    sidebar_id = "sidebar_tabs",
    tabs = list(
      options_tab(ns),
      bootstrap_tab(ns),
      adjustments_tab(ns)
    ),
    main_content = shiny$uiOutput(ns("main_content")),
    action_button = shiny$tagList(
      shiny$actionButton(
        inputId = ns("compute_button"),
        label = "Compute Statistics",
        class = "btn-primary btn-sm w-100",
        icon = bsicons$bs_icon("calculator")
      ),
      shiny$tags$div(
        class = "small text-muted mt-2",
        "Computation may take some time."
      )
    )
  )
}

#' @export
server <- function(id, input_data, data_version,
                   plotting_x_axis = NULL,
                   plotting_measures = NULL,
                   plotting_trim_percent = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    last_error <- shiny$reactiveVal(NULL)
    computation_results <- shiny$reactiveVal(NULL)
    computation_status <- shiny$reactiveVal("idle")

    # --- Reset state on new data ---
    shiny$observeEvent(data_version(), {
      computation_results(NULL)
      computation_status("idle")
      last_error(NULL)
      rhino$log$info("Statistics: state reset for new data")
    }, ignoreInit = TRUE)

    # --- Trim value display (read-only from Plotting) ---
    output$trim_value_display <- shiny$renderUI({
      tr <- if (!is.null(plotting_trim_percent)) {
        plotting_trim_percent()
      } else {
        0
      }
      tr_val <- tr %||% 0

      shiny$tags$div(
        class = "d-flex align-items-center gap-2 mb-2",
        bsicons$bs_icon("scissors", class = "text-muted"),
        shiny$tags$span(
          class = "small",
          paste0(
            "Trim: ", tr_val, "%",
            " (from Plotting tab)"
          )
        )
      )
    })

    # --- Approach details accordion content ---
    output$approach_details <- shiny$renderUI({
      approach <- input$test_approach %||% "robust"
      if (approach == "robust") {
        shiny$tags$div(
          class = "small",
          shiny$tags$p(
            shiny$tags$strong("Robust Tests"),
            " use trimmed means and are less",
            " sensitive to outliers and",
            " non-normal distributions."
          ),
          shiny$tags$p(
            "Recommended for most real-world data."
          )
        )
      } else {
        shiny$tags$div(
          class = "small",
          shiny$tags$p(
            shiny$tags$strong("Parametric Tests"),
            " assume normal distribution and",
            " equal variances."
          ),
          shiny$tags$p(
            "Use only when assumptions are met."
          )
        )
      }
    })

    # --- Valid comparisons UI (multi-way designs) ---
    output$valid_comparisons_ui <- shiny$renderUI({
      x_axis <- if (!is.null(plotting_x_axis)) {
        plotting_x_axis()
      } else {
        NULL
      }
      if (is.null(x_axis) || length(x_axis) < 2) {
        return(NULL)
      }

      shiny$tags$div(
        class = "mt-2",
        shiny$tags$hr(),
        shiny$tags$p(
          class = "small text-muted",
          bsicons$bs_icon(
            "info-circle", class = "me-1"
          ),
          paste0(
            length(x_axis),
            "-way design detected. Pairwise",
            " comparisons will be computed for",
            " all factor combinations."
          )
        )
      )
    })

    # --- Collect statistics parameters ---
    stats_params <- shiny$reactive({
      list(
        test_approach = input$test_approach %||% "robust",
        use_bootstrap = input$use_bootstrap %||% FALSE,
        boot_samples = input$boot_samples %||% 599,
        boot_sample_size = input$boot_sample_size,
        p_val_cor_method = input$p_val_cor_method %||%
          "bonferroni",
        show_additional_output =
          input$show_additional_output %||% FALSE,
        use_scientific_notation =
          input$use_scientific_notation %||% FALSE,
        filter_p_values =
          input$filter_p_values %||% FALSE
      )
    })

    # --- Main content: placeholder, error, or results ---
    output$main_content <- shiny$renderUI({
      err <- last_error()
      if (error_handling$is_app_error(err)) {
        return(
          error_display$error_alert_structured(
            err, type = "danger"
          )
        )
      }

      status <- computation_status()
      results <- computation_results()

      # Idle / no results yet
      if (status == "idle" || is.null(results)) {
        return(
          bslib$card(
            bslib$card_header(
              "Statistical Test Results"
            ),
            bslib$card_body(
              class = paste(
                "d-flex align-items-center",
                "justify-content-center"
              ),
              style = "min-height: 300px;",
              shiny$tags$div(
                class = "text-center text-muted",
                shiny$tags$p(
                  bsicons$bs_icon(
                    "calculator",
                    size = "3em",
                    class = "mb-3"
                  )
                ),
                shiny$tags$p(
                  "Configure options in the sidebar",
                  " and click ",
                  shiny$tags$strong(
                    "Compute Statistics"
                  ),
                  " to run the analysis."
                ),
                shiny$tags$p(
                  class = "small",
                  paste(
                    "Data selection, filtering, and",
                    "trimming are inherited from the",
                    "Plotting tab."
                  )
                )
              )
            )
          )
        )
      }

      # Error state from computation
      if (status == "error" && !is.null(results$error)) {
        return(
          bslib$card(
            bslib$card_header(
              class = "bg-danger text-white",
              "Error"
            ),
            bslib$card_body(
              shiny$tags$div(
                class = "alert alert-danger",
                bsicons$bs_icon("exclamation-triangle"),
                " ",
                results$error
              )
            )
          )
        )
      }

      # Results state — placeholder for future rendering
      if (status == "done") {
        return(
          shiny$tagList(
            shiny$tags$div(
              class = paste(
                "d-flex justify-content-between",
                "align-items-center mb-3"
              ),
              shiny$tags$h5(
                class = "mb-0",
                "Statistical Test Results"
              ),
              shiny$tags$small(
                class = "text-muted",
                paste(
                  "Computed:",
                  format(
                    results$timestamp, "%H:%M:%S"
                  )
                )
              )
            ),
            shiny$tags$div(
              class = "alert alert-info py-2",
              shiny$tags$small(
                shiny$tags$strong("Configuration: "),
                paste0(
                  length(results$measures),
                  " measurement(s), ",
                  length(results$x_axis),
                  "-way design, ",
                  "Bootstrap: ",
                  ifelse(
                    results$params$use_bootstrap,
                    "Yes", "No"
                  ),
                  ", P-adjustment: ",
                  results$params$p_val_cor_method
                )
              )
            ),
            shiny$tags$div(
              class = "text-center text-muted py-4",
              shiny$tags$p(
                "Result rendering will be implemented",
                " in the next step."
              )
            )
          )
        )
      }
    })

    # --- Handle compute button click ---
    shiny$observeEvent(input$compute_button, {
      data <- input_data()
      measures <- if (!is.null(plotting_measures)) {
        plotting_measures()
      } else {
        NULL
      }
      x_cols <- if (!is.null(plotting_x_axis)) {
        plotting_x_axis()
      } else {
        NULL
      }
      params <- stats_params()

      if (is.null(data) || nrow(data) == 0) {
        computation_status("error")
        computation_results(list(
          error = paste(
            "No data available. Please load data",
            "and configure the Plotting tab first."
          )
        ))
        return()
      }

      if (length(measures) == 0) {
        computation_status("error")
        computation_results(list(
          error = paste(
            "No measurement columns selected.",
            "Please select measurements in the",
            "Plotting tab."
          )
        ))
        return()
      }

      if (length(x_cols) == 0) {
        computation_status("error")
        computation_results(list(
          error = paste(
            "No X-axis columns selected.",
            "Please select X-axis in the",
            "Plotting tab."
          )
        ))
        return()
      }

      computation_status("computing")
      rhino$log$info(
        "Statistics: computing for ",
        "{length(measures)} measure(s), ",
        "{length(x_cols)}-way design"
      )

      # Store parameters and metadata for results display
      tr_val <- if (!is.null(plotting_trim_percent)) {
        (plotting_trim_percent() %||% 0) / 100
      } else {
        0
      }

      computation_results(list(
        measures = measures,
        x_axis = x_cols,
        params = params,
        trim_value = tr_val,
        timestamp = Sys.time()
      ))
      computation_status("done")

      rhino$log$info("Statistics: computation complete")
    })

    # Return for downstream modules
    invisible(NULL)
  })
}
