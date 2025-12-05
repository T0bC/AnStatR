#' Server logic for the Plotting page
#'
#' @param id Module namespace ID
#' @param median_data Reactive containing the median-processed data from server_median
#' @param data_version Reactive integer that increments when new data is loaded
#' @return NULL (side effects only)
server_plotting <- function(id, median_data, data_version) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Source column utilities
        source("R/utils/column_utils.R", local = TRUE)
        
        # Reset all inputs when new data is loaded
        if (!is.null(data_version)) {
            shiny::observeEvent(data_version(), {
                shiny::updateSelectizeInput(session, "metaData", selected = character(0))
                shiny::updateSelectizeInput(session, "measureVar", selected = character(0))
                shiny::updateSelectizeInput(session, "hideCols", selected = character(0))
                shiny::updateSelectizeInput(session, "xAxis", selected = character(0))
                shiny::updateSelectizeInput(session, "tooltip", selected = character(0))
                shiny::updateSliderInput(session, "trim_slider", value = 0)
            }, ignoreInit = TRUE)
        }
        
        # Reactive: check if descriptive columns are selected (debounced to prevent flickering)
        has_descriptive_selection_raw <- shiny::reactive({
            !is.null(input$metaData) && length(input$metaData) > 0
        })
        has_descriptive_selection <- has_descriptive_selection_raw |> shiny::debounce(300)
        
        # Reactive: get descriptive columns (strict pattern: uppercase + underscores only)
        descriptive_cols <- shiny::reactive({
            shiny::req(median_data())
            get_descriptive_cols(median_data())
        })
        
        # Reactive: get measurement columns
        measurement_cols <- shiny::reactive({
            shiny::req(median_data())
            get_measurement_cols(median_data())
        })
        
        # Update metaData choices when data changes
        shiny::observe({
            cols <- descriptive_cols()
            shiny::updateSelectizeInput(
                session, "metaData",
                choices = cols,
                selected = input$metaData
            )
        })
        
        # Update measureVar choices when data changes
        shiny::observe({
            cols <- measurement_cols()
            shiny::updateSelectizeInput(
                session, "measureVar",
                choices = cols,
                selected = input$measureVar
            )
        })
        
        # Update hideCols, xAxis, tooltip choices based on selected metaData
        shiny::observe({
            selected_meta <- input$metaData
            if (is.null(selected_meta)) selected_meta <- character(0)
            
            # Update hideCols
            shiny::updateSelectizeInput(
                session, "hideCols",
                choices = selected_meta,
                selected = input$hideCols[input$hideCols %in% selected_meta]
            )
            
            # Update xAxis
            shiny::updateSelectizeInput(
                session, "xAxis",
                choices = selected_meta,
                selected = input$xAxis[input$xAxis %in% selected_meta]
            )
            
            # Update tooltip
            shiny::updateSelectizeInput(
                session, "tooltip",
                choices = selected_meta,
                selected = input$tooltip[input$tooltip %in% selected_meta]
            )
        })
        
        # Show/hide UI sections based on descriptive selection using CSS
        shiny::observe({
            has_selection <- has_descriptive_selection()
            
            # Remove any existing visibility style first
            shiny::removeUI(selector = paste0("#", ns("visibility_style")), immediate = TRUE)
            
            # Insert appropriate visibility style
            if (has_selection) {
                # Show step containers, hide placeholder
                shiny::insertUI(
                    selector = "head",
                    where = "beforeEnd",
                    ui = shiny::tags$style(
                        id = ns("visibility_style"),
                        paste0(
                            "#", ns("step2_container"), ",",
                            "#", ns("step3_container"), ",",
                            "#", ns("step4_container"), ",",
                            "#", ns("download_container"),
                            " { display: block !important; }",
                            "#", ns("placeholder_message"), " { display: none !important; }"
                        )
                    ),
                    immediate = TRUE
                )
            } else {
                # Hide step containers, show placeholder
                shiny::insertUI(
                    selector = "head",
                    where = "beforeEnd",
                    ui = shiny::tags$style(
                        id = ns("visibility_style"),
                        paste0(
                            "#", ns("step2_container"), ",",
                            "#", ns("step3_container"), ",",
                            "#", ns("step4_container"), ",",
                            "#", ns("download_container"),
                            " { display: none !important; }",
                            "#", ns("placeholder_message"), " { display: block !important; }"
                        )
                    ),
                    immediate = TRUE
                )
            }
        })
        
        # Reactive: columns to show for filtering (metaData minus hideCols)
        filter_cols <- shiny::reactive({
            selected <- input$metaData
            hidden <- input$hideCols
            if (is.null(selected)) return(character(0))
            selected[!selected %in% hidden]
        }) |> shiny::debounce(300)
        
        # Render checkboxes for filtering
        output$checkboxes <- shiny::renderUI({
            data <- median_data()
            shiny::req(data)
            
            cols <- filter_cols()
            
            if (length(cols) == 0) {
                return(shiny::tags$p(
                    class = "text-muted fst-italic small",
                    "Select descriptive columns or unhide some to see filtering options."
                ))
            }
            
            # Split columns into two groups for side-by-side layout
            if (length(cols) > 1) {
                half <- ceiling(length(cols) / 2)
                cols1 <- cols[seq_len(half)]
                cols2 <- cols[-seq_len(half)]
                
                shiny::fluidRow(
                    shiny::column(6, lapply(cols1, function(col) {
                        choices <- unique(data[[col]])
                        shiny::checkboxGroupInput(ns(col), label = col, choices = choices, selected = choices)
                    })),
                    shiny::column(6, lapply(cols2, function(col) {
                        choices <- unique(data[[col]])
                        shiny::checkboxGroupInput(ns(col), label = col, choices = choices, selected = choices)
                    }))
                )
            } else {
                col <- cols[1]
                choices <- unique(data[[col]])
                shiny::checkboxGroupInput(ns(col), label = col, choices = choices, selected = choices)
            }
        })
        
        # Placeholder for plots output
        output$plots <- shiny::renderUI({
            if (!has_descriptive_selection()) {
                return(bslib::card(
                    bslib::card_header("Plots"),
                    bslib::card_body(
                        shiny::tags$p(
                            class = "text-muted",
                            "Select descriptive columns in the sidebar to begin configuring your plots."
                        )
                    )
                ))
            }
            
            bslib::card(
                bslib::card_header("Plots"),
                bslib::card_body(
                    shiny::p("Plot output will appear here once server logic is implemented.")
                )
            )
        })
    })
}
