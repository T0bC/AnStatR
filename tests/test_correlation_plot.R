#' Test Script for Correlation Plot Function
#'
#' Independent testing outside the Shiny app for iterative development.

# Load required packages
library(ggplot2)
library(ggiraph)
library(dplyr)
library(tidyr)

# Source the correlation plot function
source("R/server/modules/pages/pca/correlation_plot.R")

# Create mock data similar to DMTA dataset structure
set.seed(42)
test_data <- data.frame(
    # Metadata columns
    SPECIES = sample(c("aupro", "auafr"), 50, replace = TRUE),
    SEX = sample(c("1", "2", "M", "F"), 50, replace = TRUE),
    
    # Measurement columns (numeric)
    madf = rnorm(50, 1.2, 0.3),
    metf = rnorm(50, 0.3, 0.05),
    medf = rnorm(50, 5000, 500),
    mea = rnorm(50, 100, 50),
    mev = rnorm(50, 80, 30),
    Sq = rnorm(50, 0.35, 0.1),
    Ssk = rnorm(50, 0.4, 0.1)
)

# Define measurement columns
measurement_cols <- c("madf", "metf", "medf", "mea", "mev", "Sq", "Ssk")

# Test 1: Basic correlation plot
cat("Test 1: Creating basic correlation plot...\n")
plot1 <- create_correlation_plot(test_data, measurement_cols)
print(plot1)

# Test 2: With scaled data
cat("\nTest 2: Creating correlation plot with scaled data...\n")
scaled_data <- test_data
scaled_data[, measurement_cols] <- scale(scaled_data[, measurement_cols])
plot2 <- create_correlation_plot(scaled_data, measurement_cols)
print(plot2)

# Test 3: Subset of columns
cat("\nTest 3: Creating correlation plot with subset of columns...\n")
subset_cols <- c("madf", "metf", "mea", "Sq")
plot3 <- create_correlation_plot(test_data, subset_cols)
print(plot3)

# Test 4: Data with some NA values (should handle gracefully)
cat("\nTest 4: Testing with NA values...\n")
test_data_na <- test_data
test_data_na$madf[c(1, 5, 10)] <- NA
plot4 <- create_correlation_plot(test_data_na, measurement_cols)
print(plot4)

# Test 5: Perfect correlation scenario
cat("\nTest 5: Testing with perfectly correlated variables...\n")
test_data_perfect <- test_data
test_data_perfect$madf_copy <- test_data_perfect$madf
plot5 <- create_correlation_plot(test_data_perfect, c("madf", "madf_copy", "metf", "mea"))
print(plot5)

cat("\nAll tests completed!\n")
