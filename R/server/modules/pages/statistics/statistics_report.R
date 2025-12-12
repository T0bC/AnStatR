#' Statistics Report Generation
#'
#' Handles generation of downloadable reports (HTML/PDF) for statistics results.
#' Following the explicit dependency injection pattern.
#'
#' @name statistics_report
NULL


#' Generate markdown content for a single measurement result
#'
#' Creates markdown text containing test results and configuration info.
#'
#' @param result Single measurement result from computation_results
#' @param params Statistics parameters used for computation
#' @param x_axis X-axis columns used
#' @return Character string with markdown content
generate_measurement_markdown <- function(result, params, x_axis) {
    lines <- character(0)
    
    # Header
    lines <- c(lines, paste0("# Statistical Analysis: ", result$measure))
    lines <- c(lines, "")
    lines <- c(lines, paste0("**Design Type:** ", result$design_type))
    lines <- c(lines, paste0("**Grouping Variables:** ", paste(x_axis, collapse = ", ")))
    lines <- c(lines, paste0("**Bootstrap:** ", ifelse(params$use_bootstrap, "Yes", "No")))
    lines <- c(lines, paste0("**P-value Adjustment:** ", params$p_val_cor_method))
    lines <- c(lines, "")
    
    # Errors if any
    if (length(result$errors) > 0) {
        lines <- c(lines, "## Warnings")
        lines <- c(lines, "")
        for (err in result$errors) {
            lines <- c(lines, paste0("- ", err))
        }
        lines <- c(lines, "")
    }
    
    # Main test results (T-way ANOVA)
    if (!is.null(result$result_t_way)) {
        lines <- c(lines, paste0("## ", result$header))
        lines <- c(lines, "")
        
        if (is.data.frame(result$result_t_way)) {
            if ("Error" %in% names(result$result_t_way)) {
                # Error result
                lines <- c(lines, "**Error:**")
                lines <- c(lines, paste(result$result_t_way$Error, collapse = "\n"))
            } else {
                # Valid results - convert to markdown table
                lines <- c(lines, df_to_markdown(result$result_t_way))
            }
        }
        lines <- c(lines, "")
    }
    
    paste(lines, collapse = "\n")
}


#' Convert data frame to markdown table
#'
#' @param df Data frame to convert
#' @return Character string with markdown table
df_to_markdown <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
        return("*No data available*")
    }
    
    # Header row
    header <- paste0("| ", paste(names(df), collapse = " | "), " |")
    
    # Separator row
    separator <- paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |")
    
    # Data rows
    rows <- apply(df, 1, function(row) {
        values <- sapply(row, function(x) {
            if (is.numeric(x)) {
                format(round(x, 4), nsmall = 4)
            } else {
                as.character(x)
            }
        })
        paste0("| ", paste(values, collapse = " | "), " |")
    })
    
    paste(c(header, separator, rows), collapse = "\n")
}


#' Generate full HTML report for a measurement
#'
#' Creates a standalone HTML document with plot and results.
#'
#' @param result Single measurement result
#' @param plot_object ggplot object for the measurement
#' @param params Statistics parameters
#' @param x_axis X-axis columns
#' @param timestamp Computation timestamp
#' @return Character string with HTML content
generate_html_report <- function(result, plot_object, params, x_axis, timestamp) {
    # Generate markdown content
    md_content <- generate_measurement_markdown(result, params, x_axis)
    
    # Create temporary plot file
    plot_file <- tempfile(fileext = ".png")
    on.exit(unlink(plot_file), add = TRUE)
    
    # Save plot to file
    ggplot2::ggsave(
        filename = plot_file,
        plot = plot_object,
        width = 10,
        height = 6,
        dpi = 150,
        bg = "white"
    )
    
    # Read plot as base64
    plot_base64 <- base64enc::base64encode(plot_file)
    plot_data_uri <- paste0("data:image/png;base64,", plot_base64)
    
    # Build HTML document
    html_content <- paste0(
        '<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Statistics Report: ', htmltools::htmlEscape(result$measure), '</title>
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            line-height: 1.6;
            max-width: 900px;
            margin: 0 auto;
            padding: 20px;
            color: #333;
        }
        h1 { color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; }
        h2 { color: #34495e; margin-top: 30px; }
        table {
            border-collapse: collapse;
            width: 100%;
            margin: 15px 0;
        }
        th, td {
            border: 1px solid #ddd;
            padding: 10px;
            text-align: left;
        }
        th {
            background-color: #3498db;
            color: white;
        }
        tr:nth-child(even) { background-color: #f9f9f9; }
        tr:hover { background-color: #f5f5f5; }
        .plot-container {
            margin: 20px 0;
            text-align: center;
        }
        .plot-container img {
            max-width: 100%;
            height: auto;
            border: 1px solid #ddd;
            border-radius: 4px;
        }
        .meta-info {
            background-color: #ecf0f1;
            padding: 15px;
            border-radius: 4px;
            margin-bottom: 20px;
        }
        .warning {
            background-color: #fff3cd;
            border: 1px solid #ffc107;
            padding: 10px;
            border-radius: 4px;
            margin: 10px 0;
        }
        .footer {
            margin-top: 40px;
            padding-top: 20px;
            border-top: 1px solid #ddd;
            color: #666;
            font-size: 0.9em;
        }
    </style>
</head>
<body>
    <h1>Statistical Analysis: ', htmltools::htmlEscape(result$measure), '</h1>
    
    <div class="meta-info">
        <strong>Design Type:</strong> ', htmltools::htmlEscape(result$design_type), '<br>
        <strong>Grouping Variables:</strong> ', htmltools::htmlEscape(paste(x_axis, collapse = ", ")), '<br>
        <strong>Bootstrap:</strong> ', ifelse(params$use_bootstrap, "Yes", "No"), '<br>
        <strong>P-value Adjustment:</strong> ', htmltools::htmlEscape(params$p_val_cor_method), '
    </div>
    
    <h2>Plot</h2>
    <div class="plot-container">
        <img src="', plot_data_uri, '" alt="Statistical Plot">
    </div>
    ',
        # Warnings section
        if (length(result$errors) > 0) {
            paste0(
                '<h2>Warnings</h2>\n<div class="warning">\n<ul>\n',
                paste0("<li>", htmltools::htmlEscape(unlist(result$errors)), "</li>", collapse = "\n"),
                '\n</ul>\n</div>\n'
            )
        } else "",
        
        # Results section
        if (!is.null(result$result_t_way) && is.data.frame(result$result_t_way) && 
            !("Error" %in% names(result$result_t_way))) {
            paste0(
                '<h2>', htmltools::htmlEscape(result$header), '</h2>\n',
                df_to_html_table(result$result_t_way)
            )
        } else if (!is.null(result$result_t_way) && is.data.frame(result$result_t_way) && 
                   "Error" %in% names(result$result_t_way)) {
            paste0(
                '<h2>Error</h2>\n<div class="warning">',
                htmltools::htmlEscape(paste(result$result_t_way$Error, collapse = "; ")),
                '</div>\n'
            )
        } else "",
        
        '
    <div class="footer">
        <p>Generated by TexAn 2.0 on ', format(timestamp, "%Y-%m-%d %H:%M:%S"), '</p>
    </div>
</body>
</html>'
    )
    
    html_content
}


#' Convert data frame to HTML table
#'
#' @param df Data frame to convert
#' @return Character string with HTML table
df_to_html_table <- function(df) {
    if (is.null(df) || nrow(df) == 0) {
        return("<p><em>No data available</em></p>")
    }
    
    # Header
    header <- paste0(
        "<tr>",
        paste0("<th>", htmltools::htmlEscape(names(df)), "</th>", collapse = ""),
        "</tr>"
    )
    
    # Rows
    rows <- apply(df, 1, function(row) {
        cells <- sapply(row, function(x) {
            val <- if (is.numeric(x)) {
                format(round(x, 4), nsmall = 4)
            } else {
                as.character(x)
            }
            paste0("<td>", htmltools::htmlEscape(val), "</td>")
        })
        paste0("<tr>", paste(cells, collapse = ""), "</tr>")
    })
    
    paste0(
        "<table>\n<thead>\n", header, "\n</thead>\n<tbody>\n",
        paste(rows, collapse = "\n"),
        "\n</tbody>\n</table>"
    )
}


#' Setup download handlers for statistics reports
#'
#' Registers download handlers for each measurement result.
#' Following the explicit dependency injection pattern.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param computation_results Reactive containing computation results
#' @param cached_plot_objects Reactive containing cached ggplot objects
#' @return NULL (side effects only - registers download handlers)
setup_statistics_download_handlers <- function(input, output, session, 
                                                computation_results, cached_plot_objects) {
    ns <- session$ns
    
    # Track registered download handlers
    registered_downloads <- shiny::reactiveVal(character(0))
    
    # Register download handlers when results change
    shiny::observeEvent(computation_results(), {
        results <- computation_results()
        shiny::req(results, results$results)
        
        measures <- results$measures
        already_registered <- registered_downloads()
        
        # Register handler for each measurement
        lapply(measures, function(measure) {
            safe_measure <- gsub("[^a-zA-Z0-9]", "_", measure)
            download_id <- paste0("download_report_", safe_measure)
            
            # Find the result for this measure
            result_idx <- which(sapply(results$results, function(r) r$measure == measure))
            if (length(result_idx) == 0) return()
            
            result <- results$results[[result_idx[1]]]
            
            output[[download_id]] <- shiny::downloadHandler(
                filename = function() {
                    paste0("statistics_", safe_measure, "_", 
                           format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
                },
                content = function(file) {
                    # Get current results and plots
                    current_results <- computation_results()
                    plots <- cached_plot_objects()
                    
                    # Find result for this measure
                    res_idx <- which(sapply(current_results$results, function(r) r$measure == measure))
                    if (length(res_idx) == 0) {
                        writeLines("Error: Result not found", file)
                        return()
                    }
                    
                    res <- current_results$results[[res_idx[1]]]
                    plot_obj <- plots[[measure]]
                    
                    # Generate HTML report
                    html_content <- generate_html_report(
                        result = res,
                        plot_object = plot_obj,
                        params = current_results$params,
                        x_axis = current_results$x_axis,
                        timestamp = current_results$timestamp
                    )
                    
                    writeLines(html_content, file)
                }
            )
        })
        
        registered_downloads(measures)
    })
}
