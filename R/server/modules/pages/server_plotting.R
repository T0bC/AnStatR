#' Server logic for the Plotting page
#'
#' @param id Module namespace ID
#' @param median_data Reactive containing the median-processed data from server_median
#' @param data_version Reactive integer that increments when new data is loaded
#' @return NULL (side effects only)
server_plotting <- function(id, median_data, data_version) {
    shiny::moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Source utilities and components
        source("R/utils/column_utils.R", local = TRUE)
        source("R/server/modules/pages/plotting/plot_scatter.R", local = TRUE)
        source("R/server/modules/pages/plotting/plot_renderer.R", local = TRUE)
        
        # Reset all inputs when new data is loaded
        if (!is.null(data_version)) {
            shiny::observeEvent(data_version(), {
                shiny::updateSelectizeInput(session, "metaData", selected = character(0))
                shiny::updateSelectizeInput(session, "measureVar", selected = character(0))
                shiny::updateSelectizeInput(session, "hideCols", selected = character(0))
                shiny::updateSelectizeInput(session, "xAxis", selected = character(0))
                shiny::updateSelectizeInput(session, "tooltip", selected = character(0))
                shiny::updateSliderInput(session, "trim_slider", value = 0)
                shiny::updateCheckboxInput(session, "enableOutlierDetection", value = FALSE)
                shiny::updateRadioButtons(session, "detectOutlier", selected = "IQR")
                shiny::updateSliderInput(session, "standardFactor", value = 1.5)
                shiny::updateSliderInput(session, "probabilityFactor", value = 0.05)
                shiny::updateNumericInput(session, "bootstrapSamples", value = 1000)
            }, ignoreInit = TRUE)
        }
        
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
        
        # Reactive: filtered data based on checkbox selections
        filtered_data <- shiny::reactive({
            data <- median_data()
            shiny::req(data)
            
            cols <- filter_cols()
            if (length(cols) == 0) return(data)
            
            # Apply filters from each checkbox group
            for (col in cols) {
                selected_values <- input[[col]]
                if (!is.null(selected_values) && length(selected_values) > 0) {
                    data <- data[data[[col]] %in% selected_values, , drop = FALSE]
                }
            }
            
            data
        }) |> shiny::debounce(300)
        
        # Reactive: selected measurement columns
        selected_measures <- shiny::reactive({
            input$measureVar
        })
        
        # Reactive: selected X-axis columns
        selected_x_axis <- shiny::reactive({
            input$xAxis
        })
        
        # Reactive: selected tooltip columns
        selected_tooltip_cols <- shiny::reactive({
            input$tooltip
        })
        
        # Reactive: window size from JS (for dynamic SVG sizing)
        # Access namespaced input set by plot_resize.js via initializeWindowSize()
        window_size <- shiny::reactive({
            input$windowSize
        })
        
        # Setup plot outputs using injected component
        # Following explicit dependency injection pattern
        setup_plot_outputs(
            output = output,
            ns = ns,
            filtered_data = filtered_data,
            x_cols = selected_x_axis,
            measure_cols = selected_measures,
            tooltip_cols = selected_tooltip_cols,
            create_scatter_plot = create_scatter_plot,
            window_size = window_size
        )
        
        # Render the plots UI container
        output$plots <- shiny::renderUI({
            # Check if we have the minimum required selections
            has_data <- !is.null(median_data()) && nrow(median_data()) > 0
            measures <- input$measureVar
            x_axis <- input$xAxis
            
            if (!has_data) {
                return(create_placeholder_ui(
                    "No data available. Please complete the Median Analysis first."
                ))
            }
            
            if (is.null(measures) || length(measures) == 0) {
                return(create_placeholder_ui(
                    shiny::tagList(
                        "Select measurement columns in the ",
                        shiny::tags$strong("Data"),
                        " tab to generate plots."
                    )
                ))
            }
            
            if (is.null(x_axis) || length(x_axis) == 0) {
                return(create_placeholder_ui(
                    shiny::tagList(
                        "Select X-axis column(s) in the ",
                        shiny::tags$strong("Data"),
                        " tab to generate plots."
                    )
                ))
            }
            
            # Generate plot grid using injected component
            generate_plot_grid_ui(
                ns = ns,
                measures = measures,
                plot_height = "400px"
            )
        })
    })
}
