#' Create summary dataframes reactive
#'
#' Computes summary statistics based on current sorting/filtering options.
#' Data is expected to have {col}_outlier and {col}_trimmed columns for each
#' measurement, created by the plotting module.
#'
#' @param input Shiny input object from the parent module
#' @param median_data Reactive containing processed data with outlier/trimmed flags
#' @param measurement_cols Reactive returning measurement column names
#' @param descriptive_cols Reactive returning descriptive column names
#' @param x_axis_col Reactive returning the default X-axis column name
#' @return Reactive returning list of summary dataframes
create_summary_dfs_reactive <- function(input, median_data, measurement_cols,
                                         descriptive_cols, x_axis_col) {
    
    shiny::reactive({
        shiny::req(input$sorting_options, median_data())
        
        # Validate: can't select other columns if "Measurement" is selected
        shiny::validate(
            shiny::need(
                !(length(input$sorting_options) > 1 && "Measurement" %in% input$sorting_options),
                "Cannot select other columns when 'Measurement' is selected."
            )
        )
        
        is_measurement_mode <- length(input$sorting_options) == 1 && 
            input$sorting_options == "Measurement"
        
        if (is_measurement_mode) {
            shiny::req(input$filter_options_select)
        }
        
        data <- median_data()
        measure_cols <- measurement_cols()
        desc_cols <- descriptive_cols()
        
        # Determine grouping variables
        grouping_vars <- if (is_measurement_mode) {
            input$filter_options_select
        } else {
            # Use X-axis column as default grouping when sorting by descriptive cols
            x_col <- x_axis_col()
            if (!is.null(x_col) && x_col %in% names(data)) {
                x_col
            } else if (length(desc_cols) > 0) {
                desc_cols[1]
            } else {
                character(0)
            }
        }
        
        # Summarize the data (uses {col}_outlier and {col}_trimmed columns)
        summary_df <- summarize_data(
            data = data,
            grouping_vars = grouping_vars,
            measure_vars = measure_cols,
            exclude_vars = desc_cols,
            shapiro_test = input$shapiro
        )
        
        if (is_measurement_mode) {
            # Split by measurement - exclude helper columns
            select_rows <- measure_cols[!grepl("_outlier|_trimmed", measure_cols)]
            
            lapply(select_rows, function(measurement) {
                df <- summary_df %>%
                    dplyr::filter(Measurement == measurement) %>%
                    dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 3)))
                
                list(
                    col = measurement,
                    df = df
                )
            })
        } else {
            # Filter by selected columns
            filter_by_columns(summary_df, input$sorting_options)
        }
    })
}


#' Setup summary table outputs
#'
#' Renders DT tables and creates download handlers for each summary table.
#'
#' @param output Shiny output object from the parent module
#' @param session Shiny session object from the parent module
#' @param summary_dfs Reactive returning list of summary dataframes
setup_summary_table_outputs <- function(output, session, summary_dfs) {
    ns <- session$ns
    
    shiny::observe({
        shiny::req(summary_dfs())
        
        lapply(summary_dfs(), function(summary_list) {
            col <- summary_list$col
            summary_df <- summary_list$df
            
            # Create unique IDs (sanitize column name for valid HTML id)
            safe_col <- gsub("[^a-zA-Z0-9]", "_", col)
            table_name <- paste("table", safe_col, sep = "_")
            download_name <- paste("download", safe_col, sep = "_")
            
            # Render the table
            output[[table_name]] <- DT::renderDataTable({
                DT::datatable(
                    summary_df,
                    options = list(
                        pageLength = 10,
                        scrollX = TRUE,
                        dom = 'Bfrtip'
                    ),
                    rownames = FALSE
                )
            })
            
            # Download handler for individual table (xlsx format)
            output[[download_name]] <- shiny::downloadHandler(
                filename = function() {
                    paste0("summary_stats_", safe_col, ".xlsx")
                },
                content = function(file) {
                    openxlsx::write.xlsx(summary_df, file, rowNames = FALSE)
                }
            )
        })
    })
}


#' Setup summary tables UI output
#'
#' Renders the main content area with cards for each summary table.
#'
#' @param output Shiny output object from the parent module
#' @param ns Namespace function from session
#' @param summary_dfs Reactive returning list of summary dataframes
#' @param median_data Reactive containing the median-processed data
setup_summary_tables_ui <- function(output, ns, summary_dfs, median_data) {
    
    output$summary_tables <- shiny::renderUI({
        # Check if data is available
        if (is.null(median_data())) {
            return(
                bslib::card(
                    bslib::card_header("No Data"),
                    bslib::card_body(
                        shiny::tags$div(
                            class = "text-center text-muted py-5",
                            bsicons::bs_icon("table", size = "3rem"),
                            shiny::tags$h5(class = "mt-3", "No data available"),
                            shiny::tags$p("Load and process data to view summary statistics.")
                        )
                    )
                )
            )
        }
        
        # Check if summary data is available
        summaries <- summary_dfs()
        if (is.null(summaries) || length(summaries) == 0) {
            return(
                bslib::card(
                    bslib::card_header("Summary Statistics"),
                    bslib::card_body(
                        shiny::tags$div(
                            class = "text-center text-muted py-5",
                            bsicons::bs_icon("hourglass-split", size = "3rem"),
                            shiny::tags$h5(class = "mt-3", "Calculating..."),
                            shiny::tags$p("Select sorting options to generate summary statistics.")
                        )
                    )
                )
            )
        }
        
        # Create a card for each summary table
        table_cards <- lapply(summaries, function(summary_list) {
            col <- summary_list$col
            safe_col <- gsub("[^a-zA-Z0-9]", "_", col)
            table_name <- paste("table", safe_col, sep = "_")
            download_name <- paste("download", safe_col, sep = "_")
            
            bslib::card(
                class = "mb-3",
                bslib::card_header(
                    class = "d-flex justify-content-between align-items-center",
                    shiny::tags$span(
                        bsicons::bs_icon("table"),
                        " ",
                        col
                    ),
                    shiny::downloadButton(
                        outputId = ns(download_name),
                        label = "",
                        icon = shiny::icon("download"),
                        class = "btn-sm btn-outline-primary"
                    )
                ),
                bslib::card_body(
                    class = "p-2",
                    DT::dataTableOutput(ns(table_name))
                )
            )
        })
        
        shiny::tagList(table_cards)
    })
}


#' Setup download all handler
#'
#' Creates a download handler that exports all summary tables to a single xlsx file.
#'
#' @param output Shiny output object from the parent module
#' @param summary_dfs Reactive returning list of summary dataframes
setup_download_all_handler <- function(output, summary_dfs) {
    
    output$download_all <- shiny::downloadHandler(
        filename = function() {
            paste0("summary_statistics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
        },
        content = function(file) {
            summaries <- summary_dfs()
            shiny::req(summaries)
            
            # Create workbook
            wb <- openxlsx::createWorkbook()
            
            # Add each summary as a separate sheet
            for (summary_list in summaries) {
                col <- summary_list$col
                df <- summary_list$df
                
                # Sanitize sheet name (max 31 chars, no special chars)
                sheet_name <- gsub("[^a-zA-Z0-9 ]", "_", col)
                sheet_name <- substr(sheet_name, 1, 31)
                
                # Ensure unique sheet names
                existing_sheets <- names(wb)
                if (sheet_name %in% existing_sheets) {
                    sheet_name <- paste0(substr(sheet_name, 1, 28), "_", 
                                         sum(grepl(sheet_name, existing_sheets)) + 1)
                }
                
                openxlsx::addWorksheet(wb, sheet_name)
                openxlsx::writeData(wb, sheet_name, df)
                
                # Auto-width columns
                openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(df)), widths = "auto")
            }
            
            openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        }
    )
}
