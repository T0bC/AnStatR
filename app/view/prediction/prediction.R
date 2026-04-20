box::use(
  bsicons,
  bslib,
  DT,
  ggiraph,
  ggplot2,
  openxlsx,
  rhino,
  shiny,
  tools[file_ext],
)

box::use(
  app/logic/shared/error_handling,
  app/logic/load_data/load_data[
    read_data_file, validate_data,
    validate_file_extension
  ],
  app/logic/prediction/bundle_io[load_bundle],
  app/logic/prediction/predict[
    preprocess_unknown, predict_unknown
  ],
  app/logic/prediction/prediction_plots[
    create_prediction_overlay_plot
  ],
  app/logic/prediction/validation[
    validate_unknown_data
  ],
  app/view/components/sidebar_tabs,
  app/view/shared/error_display,
  app/view/prediction/results_display[
    render_prediction_results, build_results_table
  ],
  app/view/prediction/plotting_controls,
  app/view/prediction/upload,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  sidebar_tabs$tab_layout(
    ns = ns,
    sidebar_id = "sidebar_tabs",
    tabs = list(
      upload$tab_ui(ns),
      plotting_controls$tab_ui(ns)
    ),
    main_content = shiny$uiOutput(ns("main_content")),
    action_button = shiny$tagList(
      shiny$actionButton(
        inputId = ns("predict_button"),
        label = "Run Prediction",
        class = "btn-primary btn-sm w-100",
        icon = bsicons$bs_icon("crosshair2")
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    last_error <- shiny$reactiveVal(NULL)
    bundle <- shiny$reactiveVal(NULL)
    unknown_data <- shiny$reactiveVal(NULL)
    validation_result <- shiny$reactiveVal(NULL)
    prediction_result <- shiny$reactiveVal(NULL)
    last_plot <- shiny$reactiveVal(NULL)

    # Delegate upload sidebar
    upload$tab_server(
      input, output, session,
      bundle_reactive = bundle,
      unknown_data_reactive = unknown_data,
      validation_reactive = validation_result
    )

    # Delegate plotting controls sidebar
    plotting_controls$tab_server(
      input, output, session,
      bundle_reactive = bundle,
      unknown_data_reactive = unknown_data
    )

    # Handle bundle file upload
    shiny$observeEvent(input$bundle_file, {
      last_error(NULL)
      bundle(NULL)
      validation_result(NULL)
      prediction_result(NULL)
      last_plot(NULL)

      file_info <- input$bundle_file
      shiny$req(file_info)

      rhino$log$info(
        "Prediction: loading bundle '",
        "{file_info$name}'"
      )

      bundle_res <- load_bundle(file_info$datapath)
      if (!bundle_res$success) {
        last_error(bundle_res$error)
        return()
      }

      bundle(bundle_res$result)

      # Re-validate unknown data if already loaded
      unknown <- unknown_data()
      if (!is.null(unknown)) {
        val <- validate_unknown_data(
          unknown, bundle_res$result
        )
        validation_result(val)
      }

      rhino$log$info(
        "Prediction: bundle loaded successfully"
      )
    })

    # Handle unknown data file upload
    shiny$observeEvent(input$unknown_file, {
      last_error(NULL)
      unknown_data(NULL)
      validation_result(NULL)
      prediction_result(NULL)
      last_plot(NULL)

      file_info <- input$unknown_file
      shiny$req(file_info)

      rhino$log$info(
        "Prediction: loading unknown data '",
        "{file_info$name}'"
      )

      # Validate extension
      ext_check <- validate_file_extension(
        file_info$name
      )
      if (!ext_check$valid) {
        last_error(error_handling$simple_error(
          message = paste(
            "Unsupported file format.",
            "Please upload a .csv or .xlsx file."
          ),
          operation_name = "Load Unknown Data"
        ))
        return()
      }

      # Read the file
      read_res <- read_data_file(
        file_info$datapath, ext_check$ext
      )
      if (!read_res$success) {
        last_error(read_res$error)
        return()
      }

      # Validate it's a usable data frame
      val_data <- validate_data(read_res$data)
      if (!val_data$valid) {
        last_error(val_data$error)
        return()
      }

      unknown_data(read_res$data)

      # Validate against bundle if loaded
      bdl <- bundle()
      if (!is.null(bdl)) {
        val <- validate_unknown_data(
          read_res$data, bdl
        )
        validation_result(val)
      }

      rhino$log$info(
        "Prediction: unknown data loaded",
        " ({nrow(read_res$data)} rows)"
      )
    })

    # Handle Predict button
    shiny$observeEvent(input$predict_button, {
      last_error(NULL)
      prediction_result(NULL)
      last_plot(NULL)

      bdl <- bundle()
      unknown <- unknown_data()

      if (is.null(bdl)) {
        last_error(error_handling$simple_error(
          message = paste(
            "No model bundle loaded.",
            "Please upload a .rds bundle first."
          ),
          operation_name = "Prediction"
        ))
        return()
      }

      if (is.null(unknown)) {
        last_error(error_handling$simple_error(
          message = paste(
            "No unknown data loaded.",
            "Please upload a CSV or Excel file."
          ),
          operation_name = "Prediction"
        ))
        return()
      }

      # Validate
      val <- validation_result()
      if (is.null(val)) {
        val <- validate_unknown_data(unknown, bdl)
        validation_result(val)
      }
      if (!val$valid) {
        last_error(error_handling$simple_error(
          message = paste(
            "Validation failed:",
            paste(val$errors, collapse = "; ")
          ),
          operation_name = "Prediction"
        ))
        return()
      }

      # Preprocess
      rhino$log$info("Prediction: preprocessing")
      preprocessed <- preprocess_unknown(unknown, bdl)

      # Predict
      pred_res <- predict_unknown(bdl, preprocessed)
      if (!pred_res$success) {
        last_error(pred_res$error)
        return()
      }

      prediction_result(pred_res$result)
      rhino$log$info(
        "Prediction: complete — ",
        "{pred_res$result$n_unknowns} predicted"
      )
    })

    # Main content rendering
    output$main_content <- shiny$renderUI({
      err <- last_error()
      if (error_handling$is_app_error(err)) {
        return(
          error_display$error_alert_structured(
            err, type = "danger"
          )
        )
      }

      pred <- prediction_result()
      if (is.null(pred)) {
        return(render_placeholder(ns))
      }

      bdl <- bundle()
      unknown <- unknown_data()
      val <- validation_result()

      # Warnings banner
      warn_banner <- NULL
      if (
        !is.null(val) &&
        length(val$warnings) > 0
      ) {
        warn_banner <- shiny$tags$div(
          class = "alert alert-warning",
          role = "alert",
          shiny$tags$strong("Warnings:"),
          shiny$tags$ul(
            lapply(val$warnings, function(w) {
              shiny$tags$li(w)
            })
          )
        )
      }

      # Results panel
      results_panel <- bslib$accordion_panel(
        title = shiny$tags$span(
          bsicons$bs_icon(
            "table", class = "me-1"
          ),
          "Prediction Results"
        ),
        value = "results_panel",
        render_prediction_results(
          pred, bdl, unknown, ns
        )
      )

      # Plot panel
      plot_panel <- NULL
      has_scores <- !is.null(pred$scores) &&
        ncol(pred$scores) >= 2
      if (has_scores) {
        plot_panel <- bslib$accordion_panel(
          title = shiny$tags$span(
            bsicons$bs_icon(
              "graph-up", class = "me-1"
            ),
            "Overlay Plot"
          ),
          value = "plot_panel",
          ggiraph$girafeOutput(
            ns("overlay_plot"), height = "500px"
          ),
          shiny$tags$div(
            class = "d-flex gap-2 mt-2",
            shiny$downloadButton(
              ns("plot_dl_svg"),
              label = shiny$tags$span(
                bsicons$bs_icon(
                  "filetype-svg", class = "me-1"
                ),
                "SVG"
              ),
              class = paste(
                "btn btn-outline-secondary btn-sm"
              )
            ),
            shiny$downloadButton(
              ns("plot_dl_png"),
              label = shiny$tags$span(
                bsicons$bs_icon(
                  "filetype-png", class = "me-1"
                ),
                "PNG"
              ),
              class = paste(
                "btn btn-outline-secondary btn-sm"
              )
            )
          )
        )
      }

      shiny$tagList(
        warn_banner,
        bslib$accordion(
          id = ns("results_accordion"),
          open = if (!is.null(plot_panel)) {
            "plot_panel"
          } else {
            "results_panel"
          },
          multiple = TRUE,
          results_panel,
          plot_panel
        )
      )
    })

    # DT table rendering
    output$prediction_table <- DT$renderDT({
      pred <- prediction_result()
      shiny$req(pred)
      unknown <- unknown_data()
      shiny$req(unknown)

      meta_col <- input$label_col
      if (
        is.null(meta_col) || meta_col == ""
      ) {
        meta_col <- NULL
      }

      df <- build_results_table(
        pred, unknown, meta_col
      )

      DT$datatable(
        df,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = "frtip"
        ),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    # Overlay plot rendering
    output$overlay_plot <- ggiraph$renderGirafe({
      pred <- prediction_result()
      shiny$req(pred)
      bdl <- bundle()
      shiny$req(bdl)
      unknown <- unknown_data()
      shiny$req(unknown)

      analysis_type <- bdl$analysis_type
      is_pca <- analysis_type == "pca"

      # Dimension inputs differ by analysis type
      if (is_pca) {
        dim_x <- input$dim_x
        dim_y <- input$dim_y
      } else {
        dim_x <- input$ldDimX
        dim_y <- input$ldDimY
      }

      shiny$req(dim_x, dim_y)

      meta_col <- input$label_col
      if (
        is.null(meta_col) || meta_col == ""
      ) {
        meta_col <- NULL
      }

      # PCA-specific plot controls
      group_cols <- input$group_col
      if (
        is.null(group_cols) ||
        length(group_cols) == 0
      ) {
        group_cols <- NULL
      }
      show_hull <- isTRUE(input$show_convex_hull)
      pt_alpha <- input$point_alpha %||% "Contribution"
      pt_size <- input$point_size %||% "Contribution"
      biplot_layer <- input$biplot_layer %||%
        "individuals"

      # LDA/MDA/QDA-specific plot controls
      show_diag <- isTRUE(input$show_diagnostics)
      show_bound <- isTRUE(input$show_boundaries)

      plot_res <- create_prediction_overlay_plot(
        bdl, pred, unknown,
        dim_x, dim_y, meta_col,
        group_cols = group_cols,
        show_convex_hull = show_hull,
        point_alpha = pt_alpha,
        point_size = pt_size,
        layer = biplot_layer,
        show_diagnostics = show_diag,
        show_boundaries = show_bound
      )

      if (!plot_res$success) return(NULL)

      last_plot(plot_res$result)

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

    # Plot download handlers
    output$plot_dl_svg <- shiny$downloadHandler(
      filename = function() {
        paste0(
          "prediction_overlay_", Sys.Date(), ".svg"
        )
      },
      content = function(file) {
        p <- last_plot()
        shiny$req(p)
        w <- input$width %||% 16
        h <- input$height %||% 10
        ggplot2$ggsave(
          file, plot = p, device = "svg",
          width = w, height = h, units = "cm"
        )
      }
    )

    output$plot_dl_png <- shiny$downloadHandler(
      filename = function() {
        paste0(
          "prediction_overlay_", Sys.Date(), ".png"
        )
      },
      content = function(file) {
        p <- last_plot()
        shiny$req(p)
        w <- input$width %||% 16
        h <- input$height %||% 10
        ggplot2$ggsave(
          file, plot = p, device = "png",
          width = w, height = h,
          units = "cm", dpi = 600
        )
      }
    )

    # Excel results download
    output$download_results_excel <-
      shiny$downloadHandler(
        filename = function() {
          paste0(
            "prediction_results_",
            format(Sys.time(), "%Y%m%d_%H%M%S"),
            ".xlsx"
          )
        },
        content = function(file) {
          pred <- prediction_result()
          shiny$req(pred)
          unknown <- unknown_data()
          shiny$req(unknown)

          meta_col <- input$label_col
          if (
            is.null(meta_col) || meta_col == ""
          ) {
            meta_col <- NULL
          }

          df <- build_results_table(
            pred, unknown, meta_col
          )

          wb <- openxlsx$createWorkbook()
          openxlsx$addWorksheet(wb, "Predictions")
          openxlsx$writeData(wb, "Predictions", df)
          openxlsx$setColWidths(
            wb, "Predictions",
            cols = seq_len(ncol(df)),
            widths = "auto"
          )
          openxlsx$saveWorkbook(
            wb, file, overwrite = TRUE
          )

          rhino$log$info(
            "Prediction: Excel export saved"
          )
        }
      )
  })
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

render_placeholder <- function(ns) {
  bslib$card(
    bslib$card_header("Prediction"),
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
            "crosshair",
            size = "3em",
            class = "mb-3"
          )
        ),
        shiny$tags$p(
          "Upload a ",
          shiny$tags$strong(".rds model bundle"),
          " and ",
          shiny$tags$strong("unknown data"),
          " in the sidebar, then click ",
          shiny$tags$strong("Run Prediction"),
          "."
        ),
        shiny$tags$p(
          class = "small text-muted mt-2",
          paste(
            "Supports PCA projection, LDA, MDA,",
            "and QDA classification. The model",
            "bundle contains the trained model and",
            "all preprocessing parameters needed",
            "to predict on new data."
          )
        )
      )
    )
  )
}
