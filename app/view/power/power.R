box::use(
  bsicons,
  bslib,
  ggiraph,
  ggplot2,
  rhino,
  shiny,
  stats[qnorm],
)

box::use(
  app/logic/plotting/scatter,
  app/logic/power/dummy_data,
  app/logic/power/power_calc,
  app/logic/shared/error_handling,
  app/view/components/sidebar_tabs,
  app/view/shared/error_display,
  app/view/power/design,
  app/view/power/effect_input,
  app/view/power/options,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  sidebar_tabs$tab_layout(
    ns = ns,
    sidebar_id = "sidebar_tabs",
    tabs = list(
      design$tab_ui(ns),
      effect_input$tab_ui(ns),
      options$tab_ui(ns)
    ),
    main_content = shiny$tags$div(
      class = "scrollable-content",
      shiny$uiOutput(ns("main_content"))
    ),
    enable_responsive_plots = TRUE,
    results_id = "main_content",
    action_button = shiny$tagList(
      shiny$actionButton(
        inputId = ns("compute_button"),
        label = "Compute Power Analysis",
        class = "btn-primary btn-sm w-100",
        icon = bsicons$bs_icon("lightning-charge")
      ),
      shiny$tags$div(
        class = "small text-muted mt-2",
        "Configure design and effect size first."
      )
    )
  )
}

#' @export
server <- function(id, input_data = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    computation_results <- shiny$reactiveVal(NULL)
    computation_status <- shiny$reactiveVal("idle")
    last_error <- shiny$reactiveVal(NULL)

    # --- Window size for responsive ggiraph sizing ---
    window_size <- shiny$reactiveVal(list(width = 800, height = 300))
    shiny$observe({
      ws <- input$windowSize
      if (!is.null(ws) && !is.null(ws$width)) {
        window_size(ws)
      }
    })

    # --- Delegate to sub-module servers ---
    design_result <- design$tab_server(
      input, output, session,
      input_data = input_data
    )

    effect_params <- effect_input$tab_server(
      input, output, session,
      design_reactive = design_result$design,
      mode_reactive = design_result$mode,
      input_data = input_data
    )

    options_params <- options$tab_server(
      input, output, session,
      mode_reactive = design_result$mode,
      effect_params_reactive = effect_params
    )

    # --- Handle compute button click ---
    shiny$observeEvent(input$compute_button, {
      design_params <- design_result$design()
      effect <- effect_params()
      opts <- options_params()

      rhino$log$info(
        "Power: Computing for {design_params$n_ways}-way design, ",
        "{design_params$n_groups} groups"
      )

      computation_status("computing")

      # Build params for power_calc
      # In import mode, use computed_f if available; otherwise use effect_size
      effect_size_value <- if (!is.null(effect$computed_f)) {
        effect$computed_f
      } else {
        effect$effect_size
      }

      params <- list(
        solve_for = opts$solve_for,
        alpha = opts$alpha,
        power_target = opts$power_target,
        n_per_group = opts$n_per_group,
        effect_type = effect$effect_type,
        effect_size = effect_size_value,
        input_mode = effect$input_mode %||% "mean_sd",
        group_means = effect$group_means,
        group_sd = effect$group_sd,
        group_medians = effect$group_medians,
        group_iqr = effect$group_iqr,
        n_groups = design_params$n_groups,
        n_ways = design_params$n_ways,
        approach = opts$approach,
        n_sim = opts$n_sim,
        distribution = effect$distribution
      )

      distribution <- params$distribution %||% "normal"
      is_simulation <- opts$approach != "parametric" ||
        (distribution != "normal" && opts$approach == "parametric")

      if (is_simulation) {
        shiny$showNotification(
          paste0(
            "Starting Monte Carlo simulation (",
            params$n_sim,
            " iterations). This may take a moment."
          ),
          type = "message",
          duration = 5
        )
      }

      # Run power analysis
      power_exec <- error_handling$safe_execute(
        expr = if (is_simulation) {
          shiny$withProgress(
            message = "Running simulation...",
            detail = "Please wait while power is estimated.",
            value = 0,
            {
              last_progress <- 0
              progress_cb <- function(value, detail = NULL) {
                value <- max(0, min(1, value))
                delta <- value - last_progress
                if (delta > 0) {
                  shiny$incProgress(delta, detail = detail)
                  last_progress <<- value
                }
              }

              result <- power_calc$perform_power_analysis(
                params,
                progress_cb = progress_cb
              )

              if (last_progress < 1) {
                shiny$incProgress(1 - last_progress, detail = "Finalizing results...")
              }
              result
            }
          )
        } else {
          power_calc$perform_power_analysis(params)
        },
        operation_name = "Power Analysis",
        context = list(
          solve_for = params$solve_for,
          approach = params$approach,
          n_groups = params$n_groups,
          n_sim = params$n_sim,
          distribution = params$distribution
        ),
        error_parser = error_handling$default_error_parser
      )

      if (!isTRUE(power_exec$success)) {
        computation_status("error")
        last_error(power_exec$error)
        computation_results(NULL)
        return()
      }

      result <- power_exec$result

      if (error_handling$is_app_error(result)) {
        computation_status("error")
        last_error(result)
        computation_results(NULL)
        return()
      }

      # Show any messages from power calculation (e.g., auto-switch to simulation)
      if (!is.null(result$messages) && length(result$messages) > 0) {
        for (msg in result$messages) {
          shiny$showNotification(msg, type = "message", duration = 6)
        }
      }

      # Generate dummy data for visualization
      n_for_dummy <- if (opts$solve_for == "sample_size") {
        result$result$value
      } else {
        opts$n_per_group
      }

      if (effect$effect_type == "raw") {
        # For raw mode, we need to get means/sd for dummy data
        # If median_iqr mode, use the converted values from the result
        if (effect$input_mode == "median_iqr") {
          # Use group_means from normalized params in result
          # For now, approximate by using medians as means (close for symmetric)
          dummy_means <- effect$group_medians
          dummy_sd <- effect$group_iqr / (2 * qnorm(0.75))  # IQR to SD for normal
        } else {
          dummy_means <- effect$group_means
          dummy_sd <- effect$group_sd
        }

        dummy_df <- dummy_data$simulate_group_data(
          group_means = dummy_means,
          group_sd = dummy_sd,
          n_per_group = n_for_dummy,
          distribution = effect$distribution,
          factor_structure = design_params$factors,
          measure_name = design_params$measure_name,
          seed = 42
        )
      } else {
        # Create dummy means from effect size
        k <- design_params$n_groups
        pooled_sd <- 1
        f <- effect$effect_size
        dummy_means <- seq(0, f * sqrt(k) * pooled_sd, length.out = k)
        dummy_means <- dummy_means - mean(dummy_means) + 5

        # Generate group names from factor structure
        group_names <- generate_group_names(design_params$factors)
        names(dummy_means) <- group_names

        dummy_df <- dummy_data$simulate_group_data(
          group_means = dummy_means,
          group_sd = pooled_sd,
          n_per_group = n_for_dummy,
          distribution = effect$distribution,
          factor_structure = design_params$factors,
          measure_name = design_params$measure_name,
          seed = 42
        )
      }

      result$dummy_data <- dummy_df
      result$design_params <- design_params

      computation_results(result)
      computation_status("done")
      last_error(NULL)

      rhino$log$info("Power: Computation complete")
    })

    # --- Main content rendering ---
    output$main_content <- shiny$renderUI({
      err <- last_error()
      if (error_handling$is_app_error(err)) {
        return(error_display$error_alert_structured(err, type = "danger"))
      }

      status <- computation_status()
      results <- computation_results()

      # Idle state
      if (status == "idle" || is.null(results)) {
        return(
          bslib$card(
            bslib$card_header("Power Analysis Results"),
            bslib$card_body(
              class = "d-flex align-items-center justify-content-center",
              style = "min-height: 300px;",
              shiny$tags$div(
                class = "text-center text-muted",
                shiny$tags$p(
                  bsicons$bs_icon("lightning-charge", size = "3em", class = "mb-3")
                ),
                shiny$tags$p(
                  "Configure your study design and effect size in the sidebar,",
                  " then click ",
                  shiny$tags$strong("Compute Power Analysis"),
                  " to calculate."
                ),
                shiny$tags$p(
                  class = "small",
                  "Supports 1-way, 2-way, and 3-way factorial designs."
                )
              )
            )
          )
        )
      }

      # Computing state
      if (status == "computing") {
        return(
          bslib$card(
            bslib$card_header("Computing..."),
            bslib$card_body(
              class = "d-flex align-items-center justify-content-center",
              style = "min-height: 300px;",
              shiny$tags$div(
                class = "spinner-border text-primary",
                role = "status"
              )
            )
          )
        )
      }

      # Results state
      if (status == "done" && !is.null(results)) {
        result_info <- results$result
        approach_label <- switch(
          results$params$approach,
          "parametric" = "Parametric (ANOVA)",
          "robust" = "Robust (Simulation)",
          "nonparametric" = "Non-Parametric (Simulation)"
        )

        shiny$tagList(
          # Results card
          bslib$card(
            bslib$card_header(
              class = "bg-primary text-white",
              bsicons$bs_icon("check-circle", class = "me-2"),
              "Power Analysis Result"
            ),
            bslib$card_body(
              shiny$tags$div(
                class = "row",
                shiny$tags$div(
                  class = "col-md-6",
                  shiny$tags$h4(result_info$description),
                  shiny$tags$p(
                    class = "text-muted",
                    paste0(
                      "Approach: ", approach_label, " | ",
                      "Effect size (f): ", round(results$effect_f, 3), " | ",
                      "\u03b1 = ", results$params$alpha
                    )
                  )
                ),
                shiny$tags$div(
                  class = "col-md-6",
                  shiny$tags$h6("Design Table"),
                  shiny$tags$div(
                    class = "table-responsive",
                    render_design_table(results$design_table_df)
                  )
                )
              )
            )
          ),
          # Power curve card
          bslib$card(
            class = "mt-3",
            bslib$card_header(
              bsicons$bs_icon("graph-up", class = "me-2"),
              "Power Curve"
            ),
            bslib$card_body(
              shiny$plotOutput(ns("power_curve_plot"), height = "250px")
            )
          ),
          # Dummy data preview card
          bslib$card(
            class = "mt-3",
            bslib$card_header(
              bsicons$bs_icon("bar-chart-line", class = "me-2"),
              "Simulated Data Preview"
            ),
            bslib$card_body(
              ggiraph$girafeOutput(ns("dummy_scatter"), height = "350px")
            )
          )
        )
      }
    })

    # --- Power curve plot ---
    output$power_curve_plot <- shiny$renderPlot({
      results <- computation_results()
      shiny$req(results)

      df <- results$power_curve_df
      has_required_cols <- is.data.frame(df) && all(c("n", "power") %in% names(df))
      if (!has_required_cols) {
        col_names <- if (is.null(names(df))) character(0) else names(df)
        rhino$log$warn(
          "Power curve anomaly: invalid payload shape. class={paste(class(df), collapse = ',')}, columns={paste(col_names, collapse = ',')}"
        )
      }
      shiny$validate(shiny$need(
        has_required_cols,
        "Power curve data is unavailable. Please recompute analysis."
      ))

      df <- df[, c("n", "power"), drop = FALSE]
      n_total <- nrow(df)
      n_na <- sum(is.na(df$n))
      power_na <- sum(is.na(df$power))
      finite_mask <- is.finite(df$n) & is.finite(df$power)
      n_finite <- sum(finite_mask)
      df <- df[finite_mask, , drop = FALSE]
      if (nrow(df) == 0) {
        rhino$log$warn(
          "Power curve anomaly: no plottable rows after finite filtering. rows_total={n_total}, rows_finite={n_finite}, n_na={n_na}, power_na={power_na}"
        )
      }
      shiny$validate(shiny$need(
        nrow(df) > 0,
        "Power curve data contains no plottable values."
      ))

      target_power <- results$params$power_target
      result_n <- if (results$result$type == "sample_size") {
        results$result$value
      } else {
        results$params$n_per_group
      }

      ggplot2$ggplot(df, ggplot2$aes(x = n, y = power)) +
        ggplot2$geom_line(color = "#0d6efd", linewidth = 1.2) +
        ggplot2$geom_hline(
          yintercept = target_power,
          linetype = "dashed",
          color = "#6c757d"
        ) +
        ggplot2$geom_vline(
          xintercept = result_n,
          linetype = "dashed",
          color = "#198754"
        ) +
        ggplot2$geom_point(
          data = data.frame(n = result_n, power = target_power),
          ggplot2$aes(x = n, y = power),
          color = "#dc3545",
          size = 4
        ) +
        ggplot2$scale_y_continuous(
          limits = c(0, 1),
          labels = scales::percent
        ) +
        ggplot2$labs(
          x = "Sample Size per Group (n)",
          y = "Power",
          title = NULL
        ) +
        ggplot2$theme_minimal() +
        ggplot2$theme(
          panel.grid.minor = ggplot2$element_blank(),
          text = ggplot2$element_text(size = 12)
        )
    })

    # --- Dummy scatter plot using existing scatter logic ---
    output$dummy_scatter <- ggiraph$renderGirafe({
      results <- computation_results()
      shiny$req(results, results$dummy_data)

      df <- results$dummy_data
      design_params <- results$design_params

      # Determine x_cols from factor structure
      x_cols <- if (!is.null(design_params$factors)) {
        sapply(design_params$factors, function(f) f$name)
      } else {
        "group"
      }

      # Filter to existing columns
      x_cols <- x_cols[x_cols %in% names(df)]
      if (length(x_cols) == 0) x_cols <- names(df)[1]

      measure_col <- design_params$measure_name
      if (!measure_col %in% names(df)) {
        measure_col <- names(df)[ncol(df)]
      }

      # Create scatter plot using existing logic
      p <- scatter$create_scatter_plot(
        data = df,
        x_cols = x_cols,
        y_col = measure_col,
        point_style = list(size = 3, alpha = 0.7),
        grid_legend = list(show_median = TRUE, show_sd = TRUE)
      )

      ws <- window_size()
      w_svg <- max(5, ws$width / 120)
      h_svg <- max(3.5, 350 / 96)

      ggiraph$girafe(
        ggobj = p,
        width_svg = w_svg,
        height_svg = h_svg,
        options = list(
          ggiraph$opts_sizing(rescale = FALSE),
          ggiraph$opts_hover(css = "fill-opacity:1; stroke-width:2;"),
          ggiraph$opts_tooltip(
            css = paste(
              "background-color:white;",
              "padding:8px;",
              "border-radius:4px;",
              "border:1px solid #ccc;",
              "font-size:12px;"
            ),
            use_fill = FALSE
          ),
          ggiraph$opts_selection(type = "none")
        )
      )
    })
  })
}

# Internal separator for multi-way group names (must match dummy_data.R)
GROUP_SEP <- ":::"

# --- Helper: generate group names from factor structure ---
generate_group_names <- function(factors) {
  if (is.null(factors) || length(factors) == 0) {
    return(character(0))
  }

  if (length(factors) == 1) {
    return(factors[[1]]$levels)
  }

  # Multi-way: generate all combinations using internal separator
  level_lists <- lapply(factors, function(f) f$levels)
  grid <- expand.grid(level_lists, stringsAsFactors = FALSE)
  apply(grid, 1, paste, collapse = GROUP_SEP)
}

# --- Helper: render design table as HTML ---
render_design_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(shiny$tags$p(class = "text-muted", "No design table available."))
  }

  shiny$tags$table(
    class = "table table-sm table-striped mb-0",
    shiny$tags$thead(
      shiny$tags$tr(
        lapply(names(df), function(col) {
          shiny$tags$th(class = "small", col)
        })
      )
    ),
    shiny$tags$tbody(
      lapply(seq_len(nrow(df)), function(i) {
        shiny$tags$tr(
          lapply(names(df), function(col) {
            shiny$tags$td(class = "small", as.character(df[i, col]))
          })
        )
      })
    )
  )
}
