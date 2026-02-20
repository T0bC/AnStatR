box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
)

box::use(
  app/logic/error_handling,
  app/logic/lda/lda[validate_inputs, run_lda, run_qda],
  app/logic/pca/na_handling[clean_na_rows],
  app/logic/pca/scaling[scale_data],
  app/view/components/sidebar_tabs,
  app/view/error_display,
  app/view/lda/analysis_settings,
  app/view/lda/data_selection,
  app/view/pca/na_summary,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  sidebar_tabs$tab_layout(
    ns = ns,
    sidebar_id = "sidebar_tabs",
    tabs = list(
      data_selection$tab_ui(ns),
      analysis_settings$tab_ui(ns)
    ),
    main_content = shiny$uiOutput(ns("main_content")),
    action_button = shiny$tagList(
      shiny$actionButton(
        inputId = ns("compute_lda_button"),
        label = "Compute LDA / QDA",
        class = "btn-primary btn-sm w-100",
        icon = bsicons$bs_icon("play-fill")
      )
    )
  )
}

#' @export
server <- function(id, input_data, data_version) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    last_error <- shiny$reactiveVal(NULL)
    result <- shiny$reactiveVal(NULL)
    na_info <- shiny$reactiveVal(NULL)
    validation_warnings <- shiny$reactiveVal(character(0))

    # Reset state when new data is loaded
    shiny$observeEvent(data_version(), {
      result(NULL)
      last_error(NULL)
      na_info(NULL)
      validation_warnings(character(0))
      rhino$log$info("LDA: state reset for new data")
    }, ignoreInit = TRUE)

    # Delegate to sub-module servers
    data_selection$tab_server(
      input, output, session,
      input_data = input_data,
      data_version = data_version
    )
    analysis_settings$tab_server(
      input, output, session,
      data_version = data_version
    )

    # Handle Compute LDA/QDA button
    shiny$observeEvent(input$compute_lda_button, {
      last_error(NULL)
      result(NULL)
      na_info(NULL)
      validation_warnings(character(0))

      data <- input_data()
      measure_cols <- input$measureVar
      grouping_col <- input$groupingCol
      analysis_type <- input$analysis_type

      # Validate inputs
      validation <- validate_inputs(
        measure_cols, data, grouping_col
      )
      if (!validation$valid) {
        last_error(validation$error)
        return()
      }
      if (length(validation$warnings) > 0) {
        validation_warnings(validation$warnings)
      }

      # Clean NAs in measurement columns
      meta_cols <- input$metaData
      if (is.null(meta_cols)) meta_cols <- character(0)
      na_result <- clean_na_rows(
        data, measure_cols, meta_cols
      )
      na_info(na_result)
      cleaned_data <- na_result$data

      if (nrow(cleaned_data) < 2) {
        last_error(error_handling$simple_error(
          message = paste(
            "After removing rows with missing values,",
            "fewer than 2 rows remain.",
            "Consider deselecting columns with",
            "many NAs."
          ),
          operation_name = "LDA Data Preparation",
          context = list(
            rows_before = na_result$rows_before,
            rows_removed = na_result$rows_removed,
            rows_after = na_result$rows_after
          )
        ))
        return()
      }

      # Scale data based on user selection (raw data only)
      analysis_data <- cleaned_data
      data_source <- input$data_source
      scale_method <- input$scale_method
      if (
        data_source == "raw" &&
        !is.null(scale_method) &&
        scale_method != "none"
      ) {
        do_center <- scale_method %in%
          c("scale_center", "center_only")
        do_scale <- scale_method == "scale_center"
        scale_res <- scale_data(
          cleaned_data, measure_cols,
          center = do_center, scale = do_scale
        )
        if (!scale_res$success) {
          last_error(scale_res$error)
          return()
        }
        analysis_data <- scale_res$result
      }

      # Determine method based on analysis type
      method <- if (analysis_type == "lda") {
        input$method
      } else {
        input$qda_method
      }

      # Build prior
      prior_choice <- input$prior
      tol <- input$tol %||% 1.0e-4
      cv <- input$cv %||% FALSE
      nu_val <- if (method == "t") input$nu else NULL

      rhino$log$info(
        "LDA: computing {toupper(analysis_type)}",
        " ({length(measure_cols)} columns,",
        " {nrow(analysis_data)} rows,",
        " grouping='{grouping_col}',",
        " method='{method}')"
      )

      # Run LDA or QDA
      lda_res <- if (analysis_type == "lda") {
        run_lda(
          data = analysis_data,
          columns = measure_cols,
          grouping_col = grouping_col,
          prior = prior_choice,
          tol = tol,
          method = method,
          cv = cv,
          nu = nu_val,
          meta_cols = meta_cols
        )
      } else {
        run_qda(
          data = analysis_data,
          columns = measure_cols,
          grouping_col = grouping_col,
          prior = prior_choice,
          tol = tol,
          method = method,
          cv = cv,
          nu = nu_val,
          meta_cols = meta_cols
        )
      }

      if (!lda_res$success) {
        last_error(lda_res$error)
        return()
      }

      result(lda_res$result)
    })

    # Main content: placeholder, error, or results
    output$main_content <- shiny$renderUI({
      err <- last_error()
      if (error_handling$is_app_error(err)) {
        return(
          error_display$error_alert_structured(
            err, type = "danger"
          )
        )
      }

      if (is.null(result())) {
        # Show validation warnings if present
        warns <- validation_warnings()
        warn_banner <- if (length(warns) > 0) {
          shiny$tags$div(
            class = "alert alert-warning",
            role = "alert",
            shiny$tags$strong("Warnings:"),
            shiny$tags$ul(
              lapply(warns, function(w) {
                shiny$tags$li(w)
              })
            )
          )
        }

        return(shiny$tagList(
          warn_banner,
          bslib$card(
            bslib$card_header("LDA / QDA Results"),
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
                    "arrows-expand-vertical",
                    size = "3em",
                    class = "mb-3"
                  )
                ),
                shiny$tags$p(
                  "Configure options in the sidebar",
                  " and click ",
                  shiny$tags$strong(
                    "Compute LDA / QDA"
                  ),
                  " to run the analysis."
                ),
                shiny$tags$p(
                  class = "small text-muted mt-2",
                  paste(
                    "LDA finds linear combinations",
                    "of variables that maximize",
                    "separation between groups.",
                    "QDA allows each group to have",
                    "its own covariance structure."
                  )
                )
              )
            )
          )
        ))
      }

      # NA summary banner
      na_res <- na_info()
      na_banner <- if (!is.null(na_res)) {
        na_summary$render_na_summary(na_res)
      }

      # Validation warnings banner
      warns <- validation_warnings()
      warn_banner <- if (length(warns) > 0) {
        shiny$tags$div(
          class = "alert alert-warning",
          role = "alert",
          shiny$tags$strong("Warnings:"),
          shiny$tags$ul(
            lapply(warns, function(w) {
              shiny$tags$li(w)
            })
          )
        )
      }

      shiny$tagList(
        na_banner,
        warn_banner,
        bslib$accordion(
          id = ns("results_accordion"),
          open = "results_panel",
          multiple = TRUE,
          bslib$accordion_panel(
            title = shiny$tags$span(
              bsicons$bs_icon(
                "arrows-expand-vertical",
                class = "me-1"
              ),
              "LDA / QDA Results"
            ),
            value = "results_panel",
            shiny$tags$p(
              class = "text-muted",
              paste(
                "Result panels will be added here",
                "once the LDA/QDA computation is",
                "implemented."
              )
            )
          )
        )
      )
    })

    # Return for downstream modules
    invisible(NULL)
  })
}
