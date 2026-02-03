#' PCA Results Display Component
#'
#' Renders PCA results with eigenvalues, variable contributions, and individual coordinates
#' in collapsible accordion panels with download buttons.

#' Render PCA results in collapsible accordion panels
#'
#' @param pca_result PCA result object from FactoMineR::PCA
#' @param ns Namespace function for download button IDs
#' @return Shiny tags object with formatted PCA display
#' @export
render_pca_results <- function(pca_result, ns) {
    # Extract eigenvalues
    eig <- pca_result$eig
    
    shiny::tagList(
        bslib::accordion(
            id = "pca_results_accordion",
            open = "eigenvalues",
            
            # Eigenvalues panel
            bslib::accordion_panel(
                title = shiny::tags$span(
                    bsicons::bs_icon("bar-chart-line", class = "me-2"),
                    "Eigenvalues & Variance"
                ),
                value = "eigenvalues",
                render_eigenvalues_table(eig)
            ),
            
            # Variable Results panel
            bslib::accordion_panel(
                title = shiny::tags$span(
                    bsicons::bs_icon("diagram-3", class = "me-2"),
                    "Variable Results"
                ),
                value = "variable_results",
                render_variable_results(pca_result$var)
            ),
            
            # Individual Results panel
            bslib::accordion_panel(
                title = shiny::tags$span(
                    bsicons::bs_icon("people", class = "me-2"),
                    "Individual Results"
                ),
                value = "individual_results",
                render_individual_results(pca_result$ind)
            ),
            
            # Downloads panel
            bslib::accordion_panel(
                title = shiny::tags$span(
                    bsicons::bs_icon("download", class = "me-2"),
                    "Download Results"
                ),
                value = "downloads",
                render_download_buttons(ns)
            )
        )
    )
}


#' Render eigenvalues table
#'
#' @param eig Eigenvalues matrix from PCA result
#' @return Shiny tags object with eigenvalues table
#' @export
render_eigenvalues_table <- function(eig) {
    eig_df <- as.data.frame(eig)
    eig_df <- cbind(
        Component = paste0("Dim.", seq_len(nrow(eig_df))),
        round(eig_df, 3)
    )
    names(eig_df) <- c("Component", "Eigenvalue", "Variance (%)", "Cumulative (%)")
    
    shiny::tags$div(
        class = "table-responsive",
        shiny::tags$table(
            class = "table table-sm table-striped",
            shiny::tags$thead(
                shiny::tags$tr(
                    lapply(names(eig_df), function(col) {
                        shiny::tags$th(
                            class = if (col != "Component") "text-end" else "",
                            col
                        )
                    })
                )
            ),
            shiny::tags$tbody(
                lapply(seq_len(nrow(eig_df)), function(i) {
                    row <- eig_df[i, ]
                    shiny::tags$tr(
                        shiny::tags$td(row$Component),
                        shiny::tags$td(class = "text-end", sprintf("%.3f", row$Eigenvalue)),
                        shiny::tags$td(class = "text-end", sprintf("%.2f%%", row$`Variance (%)`)),
                        shiny::tags$td(
                            class = "text-end",
                            shiny::tags$span(
                                class = variance_badge_class(row$`Cumulative (%)`),
                                sprintf("%.2f%%", row$`Cumulative (%)`)
                            )
                        )
                    )
                })
            )
        )
    )
}


#' Get badge class based on cumulative variance
#'
#' @param cum_var Numeric cumulative variance percentage
#' @return Character CSS class for badge
#' @export
variance_badge_class <- function(cum_var) {
    if (cum_var >= 80) "badge bg-success"
    else if (cum_var >= 60) "badge bg-warning text-dark"
    else "badge bg-secondary"
}


#' Render variable results (contributions, coordinates, cos2)
#'
#' @param var Variable results from PCA ($var)
#' @return Shiny tags object with variable results
#' @export
render_variable_results <- function(var) {
    shiny::tagList(
        # Contributions (first - most important for interpretation)
        shiny::tags$h6(class = "mt-2 mb-2", "Contributions (%)"),
        render_sortable_table(var$contrib, "Variable"),
        
        # Coordinates
        shiny::tags$h6(class = "mt-3 mb-2", "Coordinates"),
        render_matrix_table(var$coord, "Variable"),
        
        # Cos2 (quality of representation)
        shiny::tags$h6(class = "mt-3 mb-2", "Cos2 (Quality)"),
        render_matrix_table(var$cos2, "Variable")
    )
}


#' Render individual results (contributions, coordinates, cos2)
#'
#' @param ind Individual results from PCA ($ind)
#' @return Shiny tags object with individual results
#' @export
render_individual_results <- function(ind) {
    n_ind <- nrow(ind$coord)
    
    # Show message if too many individuals (but still show sortable table)
    too_many_warning <- NULL
    if (n_ind > 500) {
        too_many_warning <- shiny::tags$div(
            class = "alert alert-info mb-2",
            bsicons::bs_icon("info-circle-fill", class = "me-2"),
            sprintf(
                "Individual results contain %d observations. Tables are paginated. Download the Excel file for full data.",
                n_ind
            )
        )
    }
    
    shiny::tagList(
        too_many_warning,
        
        # Contributions (first - most important for interpretation)
        shiny::tags$h6(class = "mt-2 mb-2", "Contributions (%)"),
        render_sortable_table(ind$contrib, "Individual"),
        
        # Coordinates
        shiny::tags$h6(class = "mt-3 mb-2", "Coordinates"),
        render_sortable_table(ind$coord, "Individual"),
        
        # Cos2 (quality of representation)
        shiny::tags$h6(class = "mt-3 mb-2", "Cos2 (Quality)"),
        render_sortable_table(ind$cos2, "Individual")
    )
}


#' Render a sortable table using DT::datatable
#'
#' @param mat Matrix or data frame to render
#' @param row_label Label for the row names column
#' @return DT datatable with interactive sorting
#' @export
render_sortable_table <- function(mat, row_label = "Item") {
    df <- as.data.frame(mat)
    df <- cbind(Item = rownames(df), round(df, 4))
    rownames(df) <- NULL
    names(df)[1] <- row_label
    
    n_rows <- nrow(df)
    
    # Determine dom string: hide pagination if only one page
    dom_string <- if (n_rows <= 10) "t" else "tip"
    
    DT::datatable(
        df,
        options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = dom_string,
            order = list(),  # No default ordering - let user sort
            columnDefs = list(
                list(className = "dt-right", targets = seq(1, ncol(df) - 1))
            )
        ),
        rownames = FALSE,
        class = "table table-sm table-striped table-hover compact"
    )
}


#' Render a matrix as a static HTML table (for smaller tables)
#'
#' @param mat Matrix or data frame to render
#' @param row_label Label for the row names column
#' @return Shiny tags object with table
#' @export
render_matrix_table <- function(mat, row_label = "Item") {
    df <- as.data.frame(mat)
    df <- cbind(Item = rownames(df), round(df, 4))
    rownames(df) <- NULL
    names(df)[1] <- row_label
    
    shiny::tags$div(
        class = "table-responsive",
        style = "max-height: 300px; overflow-y: auto;",
        shiny::tags$table(
            class = "table table-sm table-striped table-hover",
            shiny::tags$thead(
                class = "sticky-top bg-white",
                shiny::tags$tr(
                    lapply(names(df), function(col) {
                        shiny::tags$th(
                            class = if (col != row_label) "text-end" else "",
                            col
                        )
                    })
                )
            ),
            shiny::tags$tbody(
                lapply(seq_len(nrow(df)), function(i) {
                    row <- df[i, ]
                    shiny::tags$tr(
                        shiny::tags$td(row[[1]]),
                        lapply(seq(2, ncol(df)), function(j) {
                            shiny::tags$td(
                                class = "text-end",
                                sprintf("%.4f", row[[j]])
                            )
                        })
                    )
                })
            )
        )
    )
}


#' Render download buttons for PCA results
#'
#' @param ns Namespace function for download button IDs
#' @return Shiny tags object with download buttons
#' @export
render_download_buttons <- function(ns) {
    shiny::tags$div(
        class = "d-flex flex-column gap-2",
        
        # Excel download
        shiny::tags$a(
            id = ns("download_pca_excel"),
            class = "btn btn-outline-primary shiny-download-link",
            href = "",
            target = "_blank",
            download = NA,
            bsicons::bs_icon("file-earmark-excel", class = "me-2"),
            "Download Excel (All Results)"
        ),
        
        # RDA download
        shiny::tags$a(
            id = ns("download_pca_rda"),
            class = "btn btn-outline-secondary shiny-download-link",
            href = "",
            target = "_blank",
            download = NA,
            bsicons::bs_icon("file-earmark-code", class = "me-2"),
            "Download RDS (PCA Object)"
        ),
        
        shiny::tags$small(
            class = "text-muted mt-2",
            "The RDS file contains the full PCA object for use in R (load with readRDS())."
        )
    )
}
