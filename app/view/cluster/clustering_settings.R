box::use(
  bsicons,
  bslib,
  rhino,
  shiny,
)

box::use(
  app/view/components/sidebar_tabs,
)

#' @export
tab_ui <- function(ns) {
  sidebar_tabs$create_tab(
    icon = "gear",
    tooltip_text = "Clustering Settings",
    value = "settings_tab",
    shiny$h6(class = "text-muted mb-3", "Clustering Settings"),
    # Number of clusters
    shiny$numericInput(
      inputId = ns("n_clusters"),
      label = shiny$tags$span(
        "Number of clusters ",
        bslib$tooltip(
          bsicons$bs_icon("info-circle", class = "text-muted"),
          "Specify the number of clusters to create."
        )
      ),
      value = 3,
      min = 2,
      max = 10
    ),
    # Clustering algorithm
    shiny$selectInput(
      inputId = ns("algorithm"),
      label = shiny$tags$span(
        "Clustering algorithm ",
        bslib$tooltip(
          bsicons$bs_icon("info-circle", class = "text-muted"),
          "Choose the clustering algorithm to use."
        )
      ),
      choices = list(
        "K-Means" = "kmeans",
        "Hierarchical" = "hierarchical",
        "DBSCAN" = "dbscan"
      ),
      selected = "kmeans"
    ),
    shiny$tags$hr(),
    # Clustering Metric
    shiny$selectInput(
      inputId = ns("cluster_metric"),
      label = shiny$tags$span(
        "Cluster Metric ",
        bslib$tooltip(
          bsicons$bs_icon("info-circle", class = "text-muted"),
          paste0(
            "Select the metric which is used for calculating ",
            "the dissimilarities between the observations.",
            "The currently available options are 'euclidean'",
            " and 'manhattan'. Euclidean distances are root",
            "sum-of-squares of differences, and manhattan ",
            "distances are the sum of absolute differences."
          )
        )
      ),
      choices = c("euclidean", "manhattan"),
      selected = "euclidean"
    ),
    # Clustering Method
    shiny$selectInput(
      inputId = ns("cluster_method"),
      label = shiny$tags$span(
        "Cluster Algorithm ",
        bslib$tooltip(
          bsicons$bs_icon("info-circle", class = "text-muted"),
          paste0(
            "The six methods implemented are 'average'",  
            "([unweighted pair-]group [arithMetic] average method",
            ", aka 'UPGMA'), 'single' (single linkage), 'complete'",
            "(complete linkage) and 'ward' (Ward's method)"
          )
        )
      ),
      choices = c("ward", "single", "complete", "average", "mcquitty", "median", "centroid"),
      selected = "ward"
    )
  )
}

#' Server logic for the Cluster clustering settings sidebar tab
#'
#' Handles clustering algorithm and parameter settings.
#'
#' @param input Shiny input object from parent module
#' @param output Shiny output object from parent module
#' @param session Shiny session object from parent module
#' @param input_data Reactive returning the current data frame
#' @param data_version Reactive returning the data version counter
#' @export
tab_server <- function(input, output, session,
                       input_data, data_version) {
  # Reset clustering settings when new data is loaded
  shiny$observeEvent(data_version(), {
    rhino$log$info(
      "Cluster clustering_settings: reset for new data"
    )
    # Reset to default values
    shiny$updateNumericInput(
      session, "n_clusters",
      value = 3
    )
    shiny$updateSelectInput(
      session, "algorithm",
      selected = "kmeans"
    )
    shiny$updateSelectInput(
      session, "cluster_metric",
      selected = "euclidean"
    )
    shiny$updateSelectInput(
      session, "cluster_method",
      selected = "ward"
    )
  }, ignoreInit = TRUE)

  # Validate cluster count based on data size
  shiny$observe({
    data <- input_data()
    if (!is.null(data) && nrow(data) > 0) {
      max_clusters <- min(nrow(data) - 1, 10)
      current_clusters <- input$n_clusters
      
      if (current_clusters > max_clusters) {
        shiny$updateNumericInput(
          session, "n_clusters",
          value = max_clusters,
          max = max_clusters
        )
      }
    }
  })
}
