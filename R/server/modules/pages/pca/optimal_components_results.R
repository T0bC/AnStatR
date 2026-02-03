#' Optimal Components Results Display
#'
#' Renders optimal component estimation results with scree plot visualization
#' and summary table showing recommendations from each method.


#' Render optimal components content for accordion panel
#'
#' Handles both success and error cases for optimal component estimation.
#'
#' @param optimal_result Result from calculate_optimal_components() or error object
#' @param ns Namespace function for output IDs
#' @return Shiny tags object with formatted display
render_optimal_components_content <- function(optimal_result, ns) {
    # Handle NULL case
    if (is.null(optimal_result)) {
        return(shiny::tags$div(
            class = "text-muted p-3",
            "Optimal component estimation not available."
        ))
    }
    
    # Handle error case
    if (is_app_error(optimal_result)) {
        return(error_alert_structured(optimal_result, type = "warning"))
    }
    
    # Render successful results
    render_optimal_components(optimal_result, ns)
}


#' Render optimal components results
#'
#' @param optimal_result Result from calculate_optimal_components()
#' @param ns Namespace function for output IDs
#' @return Shiny tags object with formatted display
render_optimal_components <- function(optimal_result, ns) {
    if (is.null(optimal_result)) {
        return(NULL)
    }
    
    shiny::tagList(
        # Summary card
        render_optimal_summary(optimal_result),
        
        # Scree plot with thresholds
        shiny::tags$div(
            class = "mt-3",
            ggiraph::girafeOutput(ns("optimal_scree_plot"), height = "400px")
        ),
        
        # Methods table
        shiny::tags$div(
            class = "mt-3",
            render_methods_table(optimal_result$methods)
        )
    )
}


#' Render summary card with recommended components
#'
#' @param optimal_result Result from calculate_optimal_components()
#' @return Shiny tags object
render_optimal_summary <- function(optimal_result) {
    summary <- optimal_result$summary
    
    if (is.null(summary) || summary$methods_computed == 0) {
        return(shiny::tags$div(
            class = "alert alert-warning",
            bsicons::bs_icon("exclamation-triangle-fill", class = "me-2"),
            "Could not compute optimal component estimates."
        ))
    }
    
    # Recommendation text
    if (summary$min_ncp == summary$max_ncp) {
        rec_text <- sprintf("All methods suggest %d component(s).", summary$min_ncp)
    } else {
        rec_text <- sprintf(
            "Methods suggest between %d and %d components (median: %d).",
            summary$min_ncp, summary$max_ncp, summary$median_ncp
        )
    }
    
    shiny::tags$div(
        class = "alert alert-info d-flex align-items-center",
        bsicons::bs_icon("lightbulb-fill", class = "me-2 flex-shrink-0"),
        shiny::tags$div(
            shiny::tags$strong("Recommendation: "),
            rec_text,
            shiny::tags$small(
                class = "d-block text-muted mt-1",
                sprintf("Based on %d estimation methods.", summary$methods_computed)
            )
        )
    )
}


#' Render methods comparison table
#'
#' @param methods List of method results
#' @return Shiny tags object with table
render_methods_table <- function(methods) {
    # Build table rows
    rows <- lapply(names(methods), function(method_name) {
        m <- methods[[method_name]]
        
        # Determine status badge
        if (!is.null(m$error)) {
            status_badge <- shiny::tags$span(
                class = "badge bg-danger",
                "Error"
            )
            ncp_display <- shiny::tags$span(
                class = "text-muted",
                title = m$error,
                "N/A"
            )
        } else {
            status_badge <- shiny::tags$span(
                class = "badge bg-success",
                m$ncp
            )
            ncp_display <- shiny::tags$strong(m$ncp)
        }
        
        shiny::tags$tr(
            shiny::tags$td(m$name),
            shiny::tags$td(class = "text-center", ncp_display),
            shiny::tags$td(
                class = "text-muted small",
                m$description
            )
        )
    })
    
    shiny::tags$div(
        class = "table-responsive",
        shiny::tags$table(
            class = "table table-sm table-hover",
            shiny::tags$thead(
                shiny::tags$tr(
                    shiny::tags$th("Method"),
                    shiny::tags$th(class = "text-center", "Components"),
                    shiny::tags$th("Description")
                )
            ),
            shiny::tags$tbody(rows)
        )
    )
}


#' Create scree plot with threshold lines
#'
#' @param optimal_result Result from calculate_optimal_components()
#' @return ggplot object
create_optimal_scree_plot <- function(optimal_result) {
    eigenvalues <- optimal_result$eigenvalues
    n_components <- length(eigenvalues)
    methods <- optimal_result$methods
    
    # Base data frame
    df <- data.frame(
        Component = seq_len(n_components),
        Eigenvalue = eigenvalues,
        Variance = eigenvalues / sum(eigenvalues) * 100,
        Cumulative = cumsum(eigenvalues) / sum(eigenvalues) * 100
    )
    
    # Create base plot
    p <- ggplot2::ggplot(df, ggplot2::aes(x = Component, y = Eigenvalue)) +
        # Eigenvalue bars
        ggiraph::geom_col_interactive(
            ggplot2::aes(
                tooltip = sprintf(
                    "Component %d\nEigenvalue: %.3f\nVariance: %.1f%%\nCumulative: %.1f%%",
                    Component, Eigenvalue, Variance, Cumulative
                ),
                data_id = Component
            ),
            fill = "#6c757d",
            alpha = 0.7
        ) +
        # Eigenvalue line
        ggplot2::geom_line(color = "#212529", linewidth = 1) +
        ggiraph::geom_point_interactive(
            ggplot2::aes(
                tooltip = sprintf("%.3f", Eigenvalue),
                data_id = paste0("point_", Component)
            ),
            size = 3,
            color = "#212529"
        )
    
    # Add Kaiser criterion line (eigenvalue = 1)
    if (!is.null(methods$kaiser)) {
        p <- p + ggplot2::geom_hline(
            yintercept = 1,
            linetype = "dashed",
            color = "#0d6efd",
            linewidth = 0.8
        ) +
        ggplot2::annotate(
            "text",
            x = n_components * 0.95,
            y = 1.1,
            label = "Kaiser (λ=1)",
            hjust = 1,
            size = 3,
            color = "#0d6efd"
        )
    }
    
    # Add Marchenko-Pastur threshold
    if (!is.null(methods$marchenko_pastur) && !is.na(methods$marchenko_pastur$threshold)) {
        mp_thresh <- methods$marchenko_pastur$threshold
        if (mp_thresh < max(eigenvalues) * 1.5) {
            p <- p + ggplot2::geom_hline(
                yintercept = mp_thresh,
                linetype = "dotted",
                color = "#198754",
                linewidth = 0.8
            ) +
            ggplot2::annotate(
                "text",
                x = n_components * 0.95,
                y = mp_thresh + max(eigenvalues) * 0.05,
                label = sprintf("M-P (λ=%.2f)", mp_thresh),
                hjust = 1,
                size = 3,
                color = "#198754"
            )
        }
    }
    
    # Add parallel analysis line if available
    if (!is.null(methods$parallel) && !is.null(methods$parallel$random_eigenvalues)) {
        random_eigs <- methods$parallel$random_eigenvalues
        pa_df <- data.frame(
            Component = seq_along(random_eigs),
            RandomEig = random_eigs
        )
        p <- p + ggplot2::geom_line(
            data = pa_df,
            ggplot2::aes(x = Component, y = RandomEig),
            linetype = "dashed",
            color = "#dc3545",
            linewidth = 0.8
        ) +
        ggplot2::annotate(
            "text",
            x = min(3, n_components),
            y = random_eigs[min(3, length(random_eigs))] + max(eigenvalues) * 0.05,
            label = "Parallel Analysis",
            hjust = 0,
            size = 3,
            color = "#dc3545"
        )
    }
    
    # Add vertical lines for method recommendations
    method_colors <- c(
        estim_ncp = "#6610f2",
        elbow = "#fd7e14"
    )
    
    for (method_name in c("estim_ncp", "elbow")) {
        m <- methods[[method_name]]
        if (!is.null(m) && !is.null(m$ncp) && !is.na(m$ncp)) {
            p <- p + ggplot2::geom_vline(
                xintercept = m$ncp + 0.5,
                linetype = "dotdash",
                color = method_colors[method_name],
                linewidth = 0.6,
                alpha = 0.7
            )
        }
    }
    
    # Theme and labels
    p <- p +
        ggplot2::scale_x_continuous(breaks = seq_len(n_components)) +
        ggplot2::labs(
            title = "Scree Plot with Optimal Component Thresholds",
            x = "Principal Component",
            y = "Eigenvalue"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(size = 12, face = "bold"),
            panel.grid.minor = ggplot2::element_blank()
        )
    
    p
}


#' Render scree plot as interactive girafe
#'
#' @param optimal_result Result from calculate_optimal_components()
#' @return girafe object
render_optimal_scree_girafe <- function(optimal_result) {
    p <- create_optimal_scree_plot(optimal_result)
    
    ggiraph::girafe(
        ggobj = p,
        width_svg = 8,
        height_svg = 5,
        options = list(
            ggiraph::opts_hover(css = "fill:#0d6efd;stroke:#0d6efd;"),
            ggiraph::opts_tooltip(
                css = "background-color:#212529;color:white;padding:8px;border-radius:4px;font-size:12px;",
                opacity = 0.9
            ),
            ggiraph::opts_selection(type = "none")
        )
    )
}
