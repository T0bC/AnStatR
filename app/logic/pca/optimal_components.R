box::use(
  rhino,
  stats,
)

box::use(
  app/logic/error_handling,
)

# =============================================================================
# Pure logic functions for optimal number of PCA components
# No Shiny dependencies allowed in this file.
# =============================================================================

#' Calculate optimal number of components using multiple methods
#'
#' Computes eigenvalues from the correlation matrix, then applies
#' Kaiser criterion, elbow detection, and parallel analysis to
#' recommend how many components to retain.
#'
#' @param data Data frame with numeric columns (already prepared/scaled)
#' @param scale Logical, whether data was scaled (affects interpretation)
#' @return List with eigenvalues, methods results, and summary,
#'   or a structured error on failure
#' @export
calculate_optimal_components <- function(data, scale = TRUE) {
  n <- nrow(data)
  p <- ncol(data)

  error_context <- list(
    n_observations = n,
    n_variables = p
  )

  # Compute eigenvalues from correlation matrix
  eig_result <- error_handling$safe_execute(
    expr = {
      cor_matrix <- stats$cor(data, use = "pairwise.complete.obs")
      eigen(cor_matrix, symmetric = TRUE, only.values = TRUE)$values
    },
    operation_name = "Eigenvalue Computation",
    context = error_context,
    error_parser = optimal_components_error_parser
  )

  if (!eig_result$success) {
    rhino$log$warn(
      "Optimal components: eigenvalue computation failed",
      " ({p} vars, {n} obs)"
    )
    return(eig_result)
  }

  eigenvalues <- eig_result$result

  # Initialize results
  results <- list(
    eigenvalues = eigenvalues,
    n = n,
    p = p,
    methods = list()
  )

  # Method 1: Kaiser criterion (eigenvalue > 1)
  kaiser_ncp <- sum(eigenvalues > 1)
  results$methods$kaiser <- list(
    name = "Kaiser Criterion",
    ncp = max(1, kaiser_ncp),
    threshold = 1,
    description = "Retain components with eigenvalue > 1"
  )

  # Method 2: Elbow detection
  elbow_result <- detect_elbow(eigenvalues)
  results$methods$elbow <- list(
    name = "Elbow Method",
    ncp = elbow_result$ncp,
    description = "Point of maximum curvature in scree plot"
  )

  # Method 3: Parallel Analysis (Horn's method)
  parallel_result <- error_handling$safe_execute(
    expr = compute_parallel_analysis(data, n_iter = 100),
    operation_name = "Parallel Analysis",
    context = error_context,
    error_parser = optimal_components_error_parser
  )

  if (parallel_result$success) {
    results$methods$parallel <- list(
      name = "Parallel Analysis (Horn)",
      ncp = parallel_result$result$ncp,
      random_eigenvalues = parallel_result$result$random_eigenvalues,
      description = paste(
        "Retain components exceeding",
        "random data eigenvalues (95th percentile)"
      )
    )
  } else {
    results$methods$parallel <- list(
      name = "Parallel Analysis (Horn)",
      ncp = NA,
      error = parallel_result$error$message,
      description = paste(
        "Retain components exceeding",
        "random data eigenvalues (95th percentile)"
      )
    )
  }

  # Summary: recommended range
  valid_ncps <- vapply(results$methods, function(m) {
    if (!is.null(m$ncp) && !is.na(m$ncp)) m$ncp else NA_real_
  }, numeric(1))
  valid_ncps <- valid_ncps[!is.na(valid_ncps)]

  if (length(valid_ncps) > 0) {
    results$summary <- list(
      min_ncp = min(valid_ncps),
      max_ncp = max(valid_ncps),
      median_ncp = round(stats$median(valid_ncps)),
      methods_computed = length(valid_ncps)
    )
  } else {
    results$summary <- list(
      min_ncp = 1,
      max_ncp = min(n - 1, p - 1),
      median_ncp = 1,
      methods_computed = 0
    )
  }

  rhino$log$info(
    "Optimal components: Kaiser={results$methods$kaiser$ncp}",
    ", Elbow={results$methods$elbow$ncp}",
    ", Parallel={results$methods$parallel$ncp}",
    " ({p} vars, {n} obs)"
  )

  list(
    success = TRUE,
    result = results,
    error = NULL
  )
}


#' Detect elbow point in eigenvalue sequence
#'
#' Uses second derivative (curvature) to find the elbow.
#'
#' @param eigenvalues Numeric vector of eigenvalues
#' @return List with ncp (elbow position)
detect_elbow <- function(eigenvalues) {
  n <- length(eigenvalues)

  if (n < 3) {
    return(list(ncp = 1))
  }

  # Compute second differences (discrete second derivative)
  first_diff <- diff(eigenvalues)
  second_diff <- diff(first_diff)

  # Elbow is where curvature is maximum (most positive second diff,
  # since eigenvalues decrease: the bend point)
  if (length(second_diff) > 0) {
    elbow_idx <- which.max(second_diff) + 1
    elbow_idx <- max(1, min(elbow_idx, n - 1))
  } else {
    elbow_idx <- 1
  }

  list(ncp = elbow_idx)
}


#' Compute parallel analysis (Horn's method)
#'
#' Generates random data and computes eigenvalues to establish
#' a baseline. Components with observed eigenvalues exceeding
#' the 95th percentile of random eigenvalues are retained.
#'
#' @param data Data frame with numeric columns
#' @param n_iter Number of random iterations
#' @return List with ncp and random eigenvalues (95th percentile)
compute_parallel_analysis <- function(data, n_iter = 100) {
  n <- nrow(data)
  p <- ncol(data)

  # Generate random eigenvalues
  random_eigs <- matrix(0, nrow = n_iter, ncol = p)

  for (i in seq_len(n_iter)) {
    random_data <- matrix(
      stats$rnorm(n * p), nrow = n, ncol = p
    )
    cor_matrix <- stats$cor(random_data)
    random_eigs[i, ] <- eigen(
      cor_matrix, symmetric = TRUE, only.values = TRUE
    )$values
  }

  # 95th percentile of random eigenvalues
  random_95 <- apply(
    random_eigs, 2, stats$quantile, probs = 0.95
  )

  # Compute actual eigenvalues from correlation matrix
  actual_cor <- stats$cor(data)
  actual_eigs <- eigen(
    actual_cor, symmetric = TRUE, only.values = TRUE
  )$values

  # Count components exceeding random threshold
  ncp <- sum(actual_eigs > random_95)
  ncp <- max(1, ncp)

  list(
    ncp = ncp,
    random_eigenvalues = random_95,
    actual_eigenvalues = actual_eigs
  )
}


#' Error parser for optimal components estimation
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
#' @export
optimal_components_error_parser <- function(
    error_msg,
    operation_name = "Optimal Components") {
  if (grepl(
    "singular|invertible",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Correlation matrix is singular.",
      " Remove highly correlated or",
      " constant variables."
    )
  } else if (grepl(
    "\\bNA\\b|missing|NaN",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Data contains missing values."
    )
  } else if (grepl(
    "dimension|ncp",
    error_msg, ignore.case = TRUE
  )) {
    paste0(
      operation_name,
      ": Insufficient dimensions for estimation."
    )
  } else {
    paste0(operation_name, " failed: ", error_msg)
  }
}
