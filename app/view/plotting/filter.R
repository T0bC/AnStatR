box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
)

box::use(
  app/logic/data_utils,
  app/view/components/sidebar_tabs,
)

#' Build the filter sidebar tab UI
#' @param ns Namespace function from the parent module
#' @return A sidebar tab created via sidebar_tabs$create_tab()
#' @export
tab_ui <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "funnel",
    tooltip_text = "Filter Data",
    value = "filter_tab",
    shiny$h6(class = "text-muted mb-3", "Filter Data"),
    shiny$selectizeInput(
      inputId = ns("hideCols"),
      label = shiny$tags$span(
        "Hide columns ",
        bslib$tooltip(
          bsicons$bs_icon(
            "info-circle", class = "text-muted"
          ),
          paste(
            "Hide selected descriptive columns from",
            "filtering but keep them for tooltips."
          )
        )
      ),
      choices = NULL,
      multiple = TRUE,
      options = list(placeholder = "Optional...")
    ),
    shiny$tags$hr(),
    shiny$uiOutput(ns("checkboxes"))
  )
}

#' Server logic for the filter sidebar tab
#'
#' Manages:
#' - hideCols choices (derived from metaData selection)
#' - Filter columns reactive (metaData minus hideCols)
#' - Dynamic checkbox rendering with two-column layout
#' - Filtered data reactive with NA handling
#' - Filter state persistence across data recalculations
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param input_data Reactive returning the current data frame
#' @param data_version Reactive returning the data version counter
#' @return List with filtered_data reactive
#' @export
tab_server <- function(input, output, session, input_data,
                       data_version) {
  ns <- session$ns
  saved_filter_state <- shiny$reactiveVal(list())

  # Smart retention on new data: keep hideCols that still exist
  shiny$observeEvent(data_version(), {
    cur_hide <- shiny$isolate(input$hideCols)
    cur_meta <- shiny$isolate(input$metaData)
    # hideCols choices come from metaData; retain valid ones
    if (!is.null(cur_hide) && !is.null(cur_meta)) {
      retained <- intersect(cur_hide, cur_meta)
    } else {
      retained <- character(0)
    }
    shiny$updateSelectizeInput(
      session, "hideCols", selected = retained
    )
    # Clear saved filter state — checkbox values may differ
    saved_filter_state(list())
    rhino$log$info("Plotting filter: reset for new data")
  }, ignoreInit = TRUE)

  # Update hideCols choices from selected metaData
  shiny$observe({
    selected_meta <- input$metaData
    if (is.null(selected_meta)) selected_meta <- character(0)
    shiny$updateSelectizeInput(
      session, "hideCols",
      choices = selected_meta,
      selected = input$hideCols[
        input$hideCols %in% selected_meta
      ]
    )
  })

  # Filter columns = metaData minus hideCols
  filter_cols <- shiny$reactive({
    selected <- input$metaData
    hidden <- input$hideCols
    if (is.null(selected)) return(character(0))
    selected[!selected %in% hidden]
  })

  # Save filter state before data changes (for persistence)
  shiny$observeEvent(input_data(), {
    cols <- shiny$isolate(filter_cols())
    if (length(cols) > 0) {
      state <- lapply(cols, function(col) input[[col]])
      names(state) <- cols
      saved_filter_state(state)
    }
  }, priority = 100, ignoreInit = TRUE)

  # Render dynamic filter checkboxes
  output$checkboxes <- shiny$renderUI({
    data <- input_data()
    shiny$req(data)

    cols <- filter_cols()
    if (length(cols) == 0) {
      return(shiny$tags$p(
        class = "text-muted fst-italic small",
        paste(
          "Select descriptive columns or unhide",
          "some to see filtering options."
        )
      ))
    }

    saved_state <- shiny$isolate(saved_filter_state())

    get_selected <- function(col, choices) {
      if (!is.null(saved_state[[col]])) {
        valid <- intersect(saved_state[[col]], choices)
        if (length(valid) > 0) return(valid)
      }
      choices
    }

    make_checkbox <- function(col) {
      ch <- data_utils$get_filter_choices(data[[col]])
      shiny$checkboxGroupInput(
        ns(col), label = col,
        choices = ch, selected = get_selected(col, ch)
      )
    }

    if (length(cols) > 1) {
      half <- ceiling(length(cols) / 2)
      cols1 <- cols[seq_len(half)]
      cols2 <- cols[-seq_len(half)]

      shiny$fluidRow(
        shiny$column(6, lapply(cols1, make_checkbox)),
        shiny$column(6, lapply(cols2, make_checkbox))
      )
    } else {
      make_checkbox(cols)
    }
  })

  # Filtered data reactive
  filtered_data <- shiny$reactive({
    data <- input_data()
    shiny$req(data)

    cols <- filter_cols()
    if (length(cols) == 0) return(data)

    # Build filters list from checkbox inputs
    filters <- lapply(cols, function(col) input[[col]])
    names(filters) <- cols

    result <- data_utils$filter_data(data, filters)
    rhino$log$info(
      "Plotting filter: {nrow(result)}/{nrow(data)} rows retained"
    )
    result
  })

  # Return filtered data for downstream use
  list(
    filtered_data = filtered_data,
    filter_cols = filter_cols
  )
}
