#' Optimal Components Estimation
#'
#' Computes multiple criteria for determining the optimal number of PCA components.
#' Includes FactoMineR's estim_ncp, elbow detection, parallel analysis,
#' Marchenko-Pastur threshold, and Gavish-Donoho threshold.


#' Calculate optimal number of components using multiple methods
#'
#' @param data Data frame with numeric columns (already prepared/scaled)
#' @param eigenvalues Numeric vector of eigenvalues from PCA result
#' @param scale Logical, whether data was scaled (for estim_ncp)
#' @return List with results from each method, or structured error
calculate_optimal_components <- function(data, eigenvalues, scale = TRUE) {
    n <- nrow(data)
    p <- ncol(data)
    
    error_context <- list(
        n_observations = n,
        n_variables = p,
        n_eigenvalues = length(eigenvalues)
    )
    
    # Initialize results list
    results <- list(
        eigenvalues = eigenvalues,
        n = n,
        p = p,
        methods = list()
    )
    
    # Method 1: FactoMineR estim_ncp (GCV)
    estim_result <- safe_execute(
        expr = {
            ncp_result <- FactoMineR::estim_ncp(data, ncp.min = 0, ncp.max = min(p - 2, 10), scale = scale, method = "GCV")
            list(
                ncp = ncp_result$ncp,
                criterion = ncp_result$criterion
            )
        },
        operation_name = "estim_ncp (GCV)",
        context = error_context,
        error_parser = optimal_components_error_parser
    )
    
    if (estim_result$success) {
        results$methods$estim_ncp <- list(
            name = "Cross-Validation (GCV)",
            ncp = estim_result$result$ncp,
            criterion = estim_result$result$criterion,
            description = "Minimizes generalized cross-validation error"
        )
    } else {
        results$methods$estim_ncp <- list(
            name = "Cross-Validation (GCV)",
            ncp = NA,
            error = estim_result$error$message,
            description = "Minimizes generalized cross-validation error"
        )
    }
    
    # Method 2: Kaiser criterion (eigenvalue > 1)
    kaiser_ncp <- sum(eigenvalues > 1)
    results$methods$kaiser <- list(
        name = "Kaiser Criterion",
        ncp = max(1, kaiser_ncp),
        threshold = 1,
        description = "Retain components with eigenvalue > 1"
    )
    
    # Method 3: Elbow detection
    elbow_result <- detect_elbow(eigenvalues)
    results$methods$elbow <- list(
        name = "Elbow Method",
        ncp = elbow_result$ncp,
        description = "Point of maximum curvature in scree plot"
    )
    
    # Method 4: Parallel Analysis (Horn's method)
    parallel_result <- safe_execute(
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
            description = "Retain components exceeding random data eigenvalues"
        )
    } else {
        results$methods$parallel <- list(
            name = "Parallel Analysis (Horn)",
            ncp = NA,
            error = parallel_result$error$message,
            description = "Retain components exceeding random data eigenvalues"
        )
    }
    
    # Method 5: Marchenko-Pastur threshold
    mp_result <- compute_marchenko_pastur(eigenvalues, n, p)
    results$methods$marchenko_pastur <- list(
        name = "Marchenko-Pastur",
        ncp = mp_result$ncp,
        threshold = mp_result$threshold,
        description = "Random matrix theory upper bound"
    )
    
    # Method 6: Gavish-Donoho threshold
    gd_result <- compute_gavish_donoho(eigenvalues, n, p)
    results$methods$gavish_donoho <- list(
        name = "Gavish-Donoho",
        ncp = gd_result$ncp,
        threshold = gd_result$threshold,
        description = "Optimal hard threshold for signal recovery"
    )
    
    # Summary: recommended range
    valid_ncps <- sapply(results$methods, function(m) {
        if (!is.null(m$ncp) && !is.na(m$ncp)) m$ncp else NA
    })
    valid_ncps <- valid_ncps[!is.na(valid_ncps)]
    
    if (length(valid_ncps) > 0) {
        results$summary <- list(
            min_ncp = min(valid_ncps),
            max_ncp = max(valid_ncps),
            median_ncp = round(median(valid_ncps)),
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
    
    results
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
    # Elbow is where curvature is maximum (most negative second derivative)
    first_diff <- diff(eigenvalues)
    second_diff <- diff(first_diff)
    
    # Find the point of maximum curvature (most negative second diff)
    # This corresponds to the "elbow" where the curve bends most
    if (length(second_diff) > 0) {
        elbow_idx <- which.max(second_diff) + 1
        # Ensure at least 1 component
        elbow_idx <- max(1, min(elbow_idx, n - 1))
    } else {
        elbow_idx <- 1
    }
    
    list(ncp = elbow_idx)
}


#' Compute parallel analysis (Horn's method)
#'
#' Generates random data and computes eigenvalues to establish a baseline.
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
        # Generate random normal data with same dimensions
        random_data <- matrix(rnorm(n * p), nrow = n, ncol = p)
        # Compute correlation matrix eigenvalues
        cor_matrix <- cor(random_data)
        random_eigs[i, ] <- eigen(cor_matrix, symmetric = TRUE, only.values = TRUE)$values
    }
    
    # 95th percentile of random eigenvalues
    random_95 <- apply(random_eigs, 2, quantile, probs = 0.95)
    
    # Compute actual eigenvalues from correlation matrix
    actual_cor <- cor(data)
    actual_eigs <- eigen(actual_cor, symmetric = TRUE, only.values = TRUE)$values
    
    # Count components exceeding random threshold
    ncp <- sum(actual_eigs > random_95)
    ncp <- max(1, ncp)  # At least 1 component
    
    list(
        ncp = ncp,
        random_eigenvalues = random_95,
        actual_eigenvalues = actual_eigs
    )
}


#' Compute Marchenko-Pastur threshold
#'
#' Upper bound of the Marchenko-Pastur distribution for random matrices.
#'
#' @param eigenvalues Numeric vector of eigenvalues
#' @param n Number of observations
#' @param p Number of variables
#' @return List with ncp and threshold
compute_marchenko_pastur <- function(eigenvalues, n, p) {
    # Aspect ratio
    gamma <- p / n
    
    # Estimate noise variance as median eigenvalue (robust estimate)
    # For standardized data, noise variance should be ~1
    sigma2 <- 1
    
    # Upper bound of Marchenko-Pastur distribution
    # lambda_+ = sigma^2 * (1 + sqrt(gamma))^2
    threshold <- sigma2 * (1 + sqrt(gamma))^2
    
    # Count eigenvalues exceeding threshold
    ncp <- sum(eigenvalues > threshold)
    ncp <- max(1, ncp)
    
    list(
        ncp = ncp,
        threshold = threshold
    )
}


#' Compute Gavish-Donoho optimal threshold
#'
#' Optimal hard threshold for singular value thresholding.
#'
#' @param eigenvalues Numeric vector of eigenvalues
#' @param n Number of observations
#' @param p Number of variables
#' @return List with ncp and threshold
compute_gavish_donoho <- function(eigenvalues, n, p) {
    # Aspect ratio beta = min(n,p) / max(n,p)
    beta <- min(n, p) / max(n, p)
    
    # Optimal threshold coefficient omega(beta)
    # Approximation from Gavish & Donoho (2014)
    omega <- 0.56 * beta^3 - 0.95 * beta^2 + 1.82 * beta + 1.43
    
    # Median singular value (sqrt of eigenvalue for covariance matrix)
    # For correlation matrix of standardized data
    median_sv <- sqrt(median(eigenvalues))
    
    # Threshold for singular values
    sv_threshold <- omega * median_sv
    
    # Convert back to eigenvalue threshold
    threshold <- sv_threshold^2
    
    # Count eigenvalues exceeding threshold
    ncp <- sum(eigenvalues > threshold)
    ncp <- max(1, ncp)
    
    list(
        ncp = ncp,
        threshold = threshold
    )
}


#' Error parser for optimal components estimation
#'
#' @param error_msg Character, the original error message
#' @param operation_name Character, name of the operation
#' @return Character, user-friendly error message
optimal_components_error_parser <- function(error_msg, operation_name = "Optimal Components") {
    if (grepl("singular|invertible", error_msg, ignore.case = TRUE)) {
        paste0(operation_name, ": Data matrix is singular.")
    } else if (grepl("NA|missing|NaN", error_msg, ignore.case = TRUE)) {
        paste0(operation_name, ": Data contains missing values.")
    } else if (grepl("dimension|ncp", error_msg, ignore.case = TRUE)) {
        paste0(operation_name, ": Insufficient dimensions for estimation.")
    } else {
        paste0(operation_name, " failed: ", error_msg)
    }
}
