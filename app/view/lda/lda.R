box::use(
  bsicons,
  bslib,
  ggiraph,
  ggplot2,
  rhino,
  shiny,
)

box::use(
  app/logic/error_handling,
  app/logic/lda/data_splitting[create_stratified_split],
  app/logic/lda/lda[
    run_lda, run_mda, run_predict, run_qda,
    validate_inputs
  ],
  app/logic/lda/lda_export[create_lda_excel, create_lda_bundle],
  app/logic/preprocessing/na_handling[clean_na_rows],
  app/logic/pca/scaling[scale_data],
  app/logic/preprocessing/skewness_transform[
    detect_skewness, transform_skewed
  ],
  app/view/components/sidebar_tabs,
  app/view/error_display,
  app/view/lda/analysis_settings,
  app/view/lda/data_selection,
  app/view/lda/plotting_controls,
  app/view/lda/results_display,
  app/view/lda/var_contrib_jitter,
  app/view/shared/preprocessing_summary,
)

box::use(
  app/logic/lda/ld_plot[create_ld_plot, create_qda_plot],
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  sidebar_tabs$tab_layout(
    ns = ns,
    sidebar_id = "sidebar_tabs",
    tabs = list(
      data_selection$tab_ui(ns),
      analysis_settings$tab_ui(ns),
      plotting_controls$tab_ui(ns)
    ),
    main_content = shiny$uiOutput(ns("main_content")),
    action_button = shiny$tagList(
      shiny$actionButton(
        inputId = ns("compute_lda_button"),
        label = "Compute LDA / QDA / MDA",
        class = "btn-primary btn-sm w-100",
        icon = bsicons$bs_icon("play-fill")
      )
    )
  )
}

#' @export
server <- function(id, input_data, data_version,
                   pca_result = NULL) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    last_error <- shiny$reactiveVal(NULL)
    result <- shiny$reactiveVal(NULL)
    test_result <- shiny$reactiveVal(NULL)
    na_info <- shiny$reactiveVal(NULL)
    transform_info <- shiny$reactiveVal(NULL)
    skewness_info <- shiny$reactiveVal(NULL)
    validation_warnings <- shiny$reactiveVal(character(0))
    bundle_data <- shiny$reactiveVal(NULL)

    # Reset state when new data is loaded
    shiny$observeEvent(data_version(), {
      result(NULL)
      test_result(NULL)
      last_error(NULL)
      na_info(NULL)
      transform_info(NULL)
      skewness_info(NULL)
      validation_warnings(character(0))
      bundle_data(NULL)
      rhino$log$info("LDA: state reset for new data")
    }, ignoreInit = TRUE)

    # Reactive: PCA scores as a flat data frame
    # (metadata cols + Dim.1, Dim.2, … columns)
    pca_scores_data <- shiny$reactive({
      if (is.null(pca_result)) return(NULL)
      pca_res <- pca_result()
      if (is.null(pca_res) || !isTRUE(pca_res$success)) {
        return(NULL)
      }
      res <- pca_res$result
      coord <- as.data.frame(res$ind$coord)
      meta <- res$ind$meta
      if (
        !is.null(meta) &&
        nrow(meta) == nrow(coord) &&
        !("Row" %in% names(meta) && ncol(meta) == 1)
      ) {
        cbind(meta, coord)
      } else {
        coord
      }
    })

    # Delegate to sub-module servers
    data_selection$tab_server(
      input, output, session,
      input_data = input_data,
      data_version = data_version,
      pca_scores_data = pca_scores_data,
      pca_result = pca_result
    )
    analysis_settings$tab_server(
      input, output, session,
      data_version = data_version
    )
    plotting_controls$tab_server(
      input, output, session,
      lda_result = result
    )

    # Delegate variable contribution jitter plot rendering
    var_contrib_jitter_state <- var_contrib_jitter$render_output(
      input, output, session,
      lda_result = result
    )

    # Reactive: last plot for download
    last_ld_plot <- shiny$reactiveVal(NULL)

    # Handle Compute LDA/QDA button
    shiny$observeEvent(input$compute_lda_button, {
      last_error(NULL)
      result(NULL)
      test_result(NULL)
      na_info(NULL)
      transform_info(NULL)
      validation_warnings(character(0))
      bundle_data(NULL)

      data_source <- input$data_source
      measure_cols <- input$measureVar
      grouping_col <- input$groupingCol
      analysis_type <- input$analysis_type
      validation_method <- input$validation_method

      # Select source data
      data <- if (data_source == "pca_scores") {
        pca_scores_data()
      } else {
        input_data()
      }

      if (is.null(data)) {
        last_error(error_handling$simple_error(
          message = if (data_source == "pca_scores") {
            paste(
              "No PCA results available.",
              "Run PCA first in the PCA tab,",
              "then return here."
            )
          } else {
            "No data available."
          },
          operation_name = "LDA Data Preparation"
        ))
        return()
      }

      # Validate inputs (pass subclasses for MDA validation)
      mda_subclasses <- input$mda_subclasses %||% 3
      validation <- validate_inputs(
        measure_cols, data, grouping_col,
        analysis_type = analysis_type,
        subclasses = mda_subclasses
      )
      if (!validation$valid) {
        last_error(validation$error)
        return()
      }
      if (length(validation$warnings) > 0) {
        validation_warnings(validation$warnings)
      }

      # Clean NAs in measurement columns and grouping column
      meta_cols <- input$metaData
      if (is.null(meta_cols)) meta_cols <- character(0)
      na_result <- clean_na_rows(
        data, measure_cols, meta_cols,
        grouping_col = grouping_col
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

      # Always detect skewness for raw data (for info banner)
      if (data_source == "raw") {
        skew_result <- detect_skewness(
          cleaned_data, measure_cols
        )
        skewness_info(skew_result)

        # Apply normalization only if enabled
        if (isTRUE(input$correct_skewness)) {
          if (any(skew_result$is_skewed)) {
            transform_res <- transform_skewed(
              cleaned_data, measure_cols, skew_result
            )
            if (transform_res$success) {
              cleaned_data <- transform_res$result$data
              transform_info(transform_res$result)
            } else {
              rhino$log$warn(
                "LDA: skewness correction failed,",
                " proceeding with untransformed data"
              )
            }
          }
        }
      }

      # Scale data (raw data only, skip for PCA scores)
      analysis_data <- cleaned_data
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
      } else if (analysis_type == "qda") {
        input$qda_method
      } else {
        "moment"  # MDA does not use MASS method
      }

      # Build prior and params
      prior_choice <- input$prior
      tol <- input$tol %||% 1.0e-4
      cv <- validation_method == "loo_cv"
      nu_val <- if (method == "t") input$nu else NULL

      # Handle train/test split if requested
      train_data <- analysis_data
      held_out_data <- NULL
      split_info <- NULL

      if (validation_method == "split") {
        train_frac <- input$train_fraction %||% 0.7
        seed <- input$split_seed %||% 42
        split_res <- create_stratified_split(
          analysis_data, grouping_col,
          train_fraction = train_frac,
          seed = seed
        )
        if (!split_res$success) {
          last_error(split_res$error)
          return()
        }
        train_data <- split_res$result$train_data
        held_out_data <- split_res$result$test_data
        split_info <- split_res$result$split_summary
      }

      rhino$log$info(
        "LDA: computing {toupper(analysis_type)}",
        " ({length(measure_cols)} columns,",
        " {nrow(train_data)} rows,",
        " grouping='{grouping_col}',",
        " method='{method}',",
        " validation='{validation_method}')"
      )

      # Run LDA, QDA, or MDA
      if (analysis_type == "mda") {
        mda_iter <- input$mda_iter %||% 5
        lda_res <- run_mda(
          data = train_data,
          columns = measure_cols,
          grouping_col = grouping_col,
          prior = prior_choice,
          cv = cv,
          meta_cols = meta_cols,
          subclasses = mda_subclasses,
          iter = mda_iter
        )
      } else {
        run_fn <- if (analysis_type == "lda") {
          run_lda
        } else {
          run_qda
        }
        lda_res <- run_fn(
          data = train_data,
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

      # Store bundle data for RDS export
      tf_info <- transform_info()
      t_params <- if (
        !is.null(tf_info) &&
        !is.null(tf_info$transform_params)
      ) {
        tf_info$transform_params
      } else {
        list()
      }
      # Capture scale params from the pre-scaled data
      s_params <- if (
        data_source == "raw" &&
        !is.null(scale_method) &&
        scale_method != "none"
      ) {
        numeric_pre <- cleaned_data[
          , measure_cols, drop = FALSE
        ]
        sc_center <- if (do_center) {
          colMeans(numeric_pre, na.rm = TRUE)
        } else {
          NULL
        }
        sc_scale <- if (do_scale) {
          vapply(
            numeric_pre,
            function(col) stats::sd(col, na.rm = TRUE),
            numeric(1)
          )
        } else {
          NULL
        }
        list(center = sc_center, scale = sc_scale)
      } else {
        NULL
      }
      bundle_data(list(
        raw_data = na_result$data,
        used_data = analysis_data,
        numeric_cols = measure_cols,
        meta_cols = meta_cols,
        transform_params = t_params,
        scale_params = s_params,
        data_source = data_source,
        settings = list(
          skewness_correction = (
            data_source == "raw" &&
            isTRUE(input$correct_skewness)
          ),
          scale_method = if (
            data_source == "raw"
          ) {
            scale_method %||% "none"
          } else {
            "none"
          },
          prior = prior_choice,
          analysis_type = analysis_type,
          method = method,
          validation_method = validation_method
        )
      ))

      # Predict on test set if split mode
      if (
        validation_method == "split" &&
        !is.null(held_out_data)
      ) {
        pred_res <- run_predict(
          lda_res$result, held_out_data,
          measure_cols,
          grouping_col = grouping_col,
          meta_cols = meta_cols
        )
        if (pred_res$success) {
          pred_res$result$split_summary <- split_info
          test_result(pred_res$result)
        } else {
          rhino$log$warn(
            "LDA: test prediction failed: ",
            pred_res$error$message
          )
          validation_warnings(c(
            validation_warnings(),
            paste(
              "Test set prediction failed:",
              pred_res$error$message
            )
          ))
        }
      }
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
                    "Compute LDA / QDA / MDA"
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
                    "its own covariance structure.",
                    "MDA models each group as a",
                    "mixture of Gaussians."
                  )
                )
              )
            )
          )
        ))
      }

      # Preprocessing summary banner (NA + skewness)
      na_res <- na_info()
      tf_res <- transform_info()
      preprocess_banner <- preprocessing_summary$render_na_summary(
        na_res,
        transform_result = tf_res,
        n_measure_cols = length(input$measureVar)
      )

      # Skewness warning (when normalization disabled but skewed cols exist)
      skew_warning <- if (
        !isTRUE(input$correct_skewness) &&
        !is.null(skewness_info())
      ) {
        preprocessing_summary$render_skewness_warning(
          skewness_info(),
          n_measure_cols = length(input$measureVar)
        )
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

      # LDA results panel content (nested accordion)
      lda_content <- results_display$render_lda_results(
        result(), ns,
        test_result = test_result()
      )

      lda_panel <- bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon(
            "bar-chart-line", class = "me-1"
          ),
          "LDA Results"
        ),
        value = "lda_panel",
        lda_content
      )

      # Scores plot panel (LDA or QDA with companion LDA)
      res <- result()
      ld_plot_panel <- NULL
      has_lda_plot <- !is.null(res) &&
        res$analysis_type %in% c("lda", "mda") &&
        !is.null(res$scores) &&
        ncol(res$scores) > 0
      has_qda_plot <- !is.null(res) &&
        res$analysis_type == "qda" &&
        !is.null(res$model)
      if (has_lda_plot || has_qda_plot) {
        plot_title <- if (has_lda_plot) {
          "LD Scores Plot"
        } else {
          "QDA Classification Plot"
        }
        ld_plot_panel <- bslib$accordion_panel(
          title = shiny$tags$span(
            bsicons$bs_icon(
              "graph-up", class = "me-1"
            ),
            plot_title
          ),
          value = "ld_plot_panel",
          ggiraph$girafeOutput(
            ns("ld_plot"), height = "500px"
          ),
          download_buttons(ns, "ld_plot")
        )
      }

      # Variable contribution jitter plot panel
      var_contrib_panel <- NULL
      has_scaling <- !is.null(res) && (
        (!is.null(res$scaling)) ||
        (res$analysis_type == "qda" &&
          !is.null(res$lda_scaling))
      )
      if (has_scaling) {
        var_contrib_panel <- bslib$accordion_panel(
          title = shiny$tags$span(
            bsicons$bs_icon(
              "diagram-3", class = "me-1"
            ),
            "Variable Contributions"
          ),
          value = "var_contrib_jitter_panel",
          shiny$tagList(
            ggiraph$girafeOutput(
              ns("var_contrib_jitter"),
              height = "auto"
            ),
            shiny$uiOutput(
              ns("var_contrib_jitter_caption")
            )
          ),
          download_buttons(ns, "var_contrib")
        )
      }

      shiny$tagList(
        preprocess_banner,
        skew_warning,
        warn_banner,
        bslib$accordion(
          id = ns("results_accordion"),
          open = "ld_plot_panel",
          multiple = TRUE,
          lda_panel,
          ld_plot_panel,
          var_contrib_panel
        )
      )
    })

    # Download handler: Excel export
    output$download_lda_excel <- shiny$downloadHandler(
      filename = function() {
        paste0(
          "lda_results_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".xlsx"
        )
      },
      content = function(file) {
        res <- result()
        shiny$req(res)
        create_lda_excel(
          res, file,
          test_result = test_result()
        )
      }
    )

    # Download handler: RDS export
    output$download_lda_rds <- shiny$downloadHandler(
      filename = function() {
        res <- result()
        prefix <- if (!is.null(res)) {
          paste0(res$analysis_type, "_bundle_")
        } else {
          "lda_bundle_"
        }
        paste0(
          prefix,
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".rds"
        )
      },
      content = function(file) {
        res <- result()
        shiny$req(res)
        bd <- bundle_data()
        shiny$req(bd)
        bundle <- create_lda_bundle(
          lda_result = res,
          raw_data = bd$raw_data,
          used_data = bd$used_data,
          numeric_cols = bd$numeric_cols,
          meta_cols = bd$meta_cols,
          transform_params = bd$transform_params,
          scale_params = bd$scale_params,
          settings = bd$settings,
          data_source = bd$data_source
        )
        saveRDS(bundle, file)
      }
    )

    # Scores plot renderer (LDA or QDA)
    output$ld_plot <- ggiraph$renderGirafe({
      res <- result()
      if (is.null(res)) return(NULL)

      dim_x <- input$ldDimX %||% "LD1"
      dim_y <- input$ldDimY %||% "LD2"
      show_bound <- isTRUE(input$show_boundaries)

      plot_res <- if (
        res$analysis_type %in% c("lda", "mda")
      ) {
        if (is.null(res$scores)) return(NULL)
        show_diag <- isTRUE(input$show_diagnostics)
        create_ld_plot(
          lda_result = res,
          dim_x = dim_x,
          dim_y = dim_y,
          show_diagnostics = show_diag,
          show_boundaries = show_bound
        )
      } else if (res$analysis_type == "qda") {
        if (is.null(res$model)) return(NULL)
        create_qda_plot(
          qda_result = res,
          dim_x = dim_x,
          dim_y = dim_y,
          show_boundaries = show_bound
        )
      } else {
        return(NULL)
      }

      if (!plot_res$success) return(NULL)

      last_ld_plot(plot_res$result)

      ggiraph$girafe(
        ggobj = plot_res$result,
        width_svg = 10,
        height_svg = 7,
        options = list(
          ggiraph$opts_sizing(
            rescale = TRUE, width = 1
          ),
          ggiraph$opts_hover(
            css = paste0(
              "fill-opacity:0.8;",
              "stroke:black;stroke-width:2px;"
            )
          ),
          ggiraph$opts_tooltip(
            css = paste0(
              "background-color:white;",
              "padding:8px;",
              "border-radius:4px;",
              "border:1px solid #ccc;",
              "font-family:sans-serif;"
            ),
            use_fill = FALSE
          ),
          ggiraph$opts_selection(type = "none")
        )
      )
    })

    # Register LD plot download handlers
    register_plot_downloads(
      output, input, "ld_plot",
      last_ld_plot, "LD_Scores_Plot"
    )
    register_plot_downloads(
      output, input, "var_contrib",
      var_contrib_jitter_state$plot,
      "Variable_Contributions"
    )

    # Return LDA result for downstream modules (e.g. Cluster)
    result
  })
}


# =============================================================================
# Local helpers (not exported)
# =============================================================================

#' Build SVG + PNG download buttons for a plot
#'
#' @param ns Namespace function
#' @param id_prefix Character, e.g. "ld_plot"
#' @return tagList with two download buttons
download_buttons <- function(ns, id_prefix) {
  shiny$tags$div(
    class = "d-flex gap-2 mt-2",
    shiny$downloadButton(
      ns(paste0(id_prefix, "_dl_svg")),
      label = shiny$tags$span(
        bsicons$bs_icon(
          "filetype-svg", class = "me-1"
        ),
        "SVG"
      ),
      class = "btn btn-outline-secondary btn-sm"
    ),
    shiny$downloadButton(
      ns(paste0(id_prefix, "_dl_png")),
      label = shiny$tags$span(
        bsicons$bs_icon(
          "filetype-png", class = "me-1"
        ),
        "PNG"
      ),
      class = "btn btn-outline-secondary btn-sm"
    )
  )
}

#' Register SVG + PNG download handlers for a plot
#'
#' @param output Shiny output object
#' @param input Shiny input object
#' @param id_prefix Character, e.g. "ld_plot"
#' @param plot_reactive reactiveVal returning a ggplot
#' @param filename_base Character, base name for the file
register_plot_downloads <- function(output, input,
                                    id_prefix,
                                    plot_reactive,
                                    filename_base) {
  output[[paste0(id_prefix, "_dl_svg")]] <-
    shiny$downloadHandler(
      filename = function() {
        paste0(filename_base, "_", Sys.Date(), ".svg")
      },
      content = function(file) {
        p <- plot_reactive()
        shiny$req(p)
        w <- input$width %||% 16
        h <- input$height %||% 10
        ggplot2$ggsave(
          file, plot = p, device = "svg",
          width = w, height = h, units = "cm"
        )
        rhino$log$info(
          "Download: SVG '{filename_base}'"
        )
      }
    )

  output[[paste0(id_prefix, "_dl_png")]] <-
    shiny$downloadHandler(
      filename = function() {
        paste0(filename_base, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- plot_reactive()
        shiny$req(p)
        w <- input$width %||% 16
        h <- input$height %||% 10
        ggplot2$ggsave(
          file, plot = p, device = "png",
          width = w, height = h,
          units = "cm", dpi = 600
        )
        rhino$log$info(
          "Download: PNG '{filename_base}'"
        )
      }
    )
}
