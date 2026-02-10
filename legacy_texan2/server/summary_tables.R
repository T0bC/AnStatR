# Import required modules
box::use(../summary_stats/summary_utils)
box::use(../../utils/error_handling)
box::use(../../ui/components/error_display)

# Import dplyr for pipe operator and data manipulation functions
box::use(dplyr[...])

#' Create summary dataframes reactive
#'
#' Computes summary statistics for each measurement column.
#' Data is expected to have {col}_outlier and {col}_trimmed columns for each
#' measurement, created by the plotting module.
#' Always uses "Measurement" mode - one table per measurement column.
#'
#' @param input Shiny input object from the parent module
#'   - input$filter_options_select: Grouping columns
#'   - input$shapiro: Whether to include Shapiro-Wilk test
#' @param median_data Reactive containing processed data with outlier/trimmed flags
#' @param measurement_cols Reactive returning measurement column names
#' @return Reactive returning list of summary dataframes (one per measurement)
#' @export
create_summary_dfs_reactive <- function(input, median_data, measurement_cols) {
    
    shiny::reactive({
        shiny::req(input$filter_options_select, median_data())
        
        data <- median_data()
        measure_cols <- measurement_cols()
        
        # Filter out helper columns
        measure_cols <- measure_cols[!grepl("_outlier|_trimmed", measure_cols)]
        shiny::req(length(measure_cols) > 0)
        
        # Grouping variables from filter options
        grouping_vars <- input$filter_options_select
        
        # Validate grouping variables exist in data
        available_cols <- names(data)
        valid_grouping_vars <- grouping_vars[grouping_vars %in% available_cols]
        
        # Return error if no valid grouping variables
        if (length(valid_grouping_vars) == 0) {
            return(error_handling$simple_error(
                message = "Selected grouping columns are not available in the current dataset. Please select valid columns.",
                operation_name = "Summary Statistics",
                context = list(
                    requested_cols = grouping_vars,
                    available_cols = paste(head(available_cols, 10), collapse = ", ")
                )
            ))
        }
        
        # Summarize the data (uses {col}_outlier and {col}_trimmed columns)
        summary_result <- error_handling$safe_execute(
            expr = summary_utils$summarize_data(
                data = data,
                grouping_vars = valid_grouping_vars,
                measure_vars = measure_cols,
                shapiro_test = input$shapiro
            ),
            operation_name = "Summary Statistics",
            context = list(
                grouping_vars = valid_grouping_vars,
                n_measures = length(measure_cols),
                n_rows = nrow(data)
            ),
            error_parser = error_handling$default_error_parser
        )
        
        # Return error if summarization failed
        if (!summary_result$success) {
            return(summary_result$error)
        }
        
        summary_df <- summary_result$result
        
        # Split by measurement - one table per measurement column
        lapply(measure_cols, function(measurement) {
            df <- summary_df %>%
                dplyr::filter(Measurement == measurement) %>%
                dplyr::select(-Measurement) %>%  # Remove Measurement column (redundant in per-table view)
                dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 3)))
            
            # Remove n_outliers column if all zeros
            if ("n_outliers" %in% names(df) && all(df$n_outliers == 0, na.rm = TRUE)) {
                df <- df %>% dplyr::select(-n_outliers)
            }
            
            # Remove n_trimmed column if all zeros
            if ("n_trimmed" %in% names(df) && all(df$n_trimmed == 0, na.rm = TRUE)) {
                df <- df %>% dplyr::select(-n_trimmed)
            }
            
            list(
                col = measurement,
                df = df
            )
        })
    })
}


#' Setup summary table outputs
#'
#' Renders DT tables and creates download handlers for each summary table.
#'
#' @param output Shiny output object from the parent module
#' @param session Shiny session object from the parent module
#' @param summary_dfs Reactive returning list of summary dataframes
#' @export
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
                n_rows <- nrow(summary_df)
                
                # Determine dom string: hide pagination if only one page
                # 't' = table, 'p' = pagination, 'i' = info
                dom_string <- if (n_rows <= 10) "t" else "tip"
                
                DT::datatable(
                    summary_df,
                    options = list(
                        pageLength = 10,
                        scrollX = TRUE,
                        dom = dom_string,
                        language = list(
                            paginate = list(
                                previous = "Previous",
                                `next` = "Next"
                            )
                        )
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
#' @export
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
        
        # Check for structured error from summarize_data
        if (error_handling$is_app_error(summaries)) {
            return(error_display$error_alert_structured(summaries, type = "danger"))
        }
        
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
                fill = FALSE,  # Don't fill available space, use natural height
                bslib::card_header(
                    class = "d-flex justify-content-between align-items-center",
                    shiny::tags$span(
                        bsicons::bs_icon("table"),
                        " ",
                        col
                    ),
                    shiny::tags$a(
                        id = ns(download_name),
                        class = "shiny-download-link text-primary",
                        href = "",
                        target = "_blank",
                        download = NA,
                        title = "Download table (XLSX)",
                        style = "font-size: 1.2rem;",
                        bsicons::bs_icon("box-arrow-down")
                    )
                ),
                bslib::card_body(
                    fillable = FALSE,  # Don't constrain content height
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
#' @export
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
