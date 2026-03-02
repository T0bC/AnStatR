# Standalone test to bypass Windsulf caching
cat("=== Standalone Test ===\n")

# Set working directory to project root
setwd("C:/Users/meissnerto/Desktop/TexAn2.0")

# Explicitly set box path to project root
options(box.path = "C:/Users/meissnerto/Desktop/TexAn2.0")

# Check file content directly
cat("Checking file content at lines 276-278:\n")
lines <- readLines('app/logic/statistics/nonparametric_tests.R')
cat(paste(lines[276:278], collapse='\n'))
cat("\n\n")

# Activate renv first
cat("Activating renv...\n")
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}

# Load required dependencies
cat("Loading dependencies...\n")
library(ARTool)
# box is used via box::use(), not library(box)

# Force clean R session and reload
cat("Loading fresh box module...\n")
cat("Box path:", getOption("box.path"), "\n")
box::use(app/logic/statistics/nonparametric_tests)

# Test with unbalanced data
cat("Creating test data...\n")
df <- expand.grid(
  GROUP = c("A", "B", "C"), 
  TREATMENT = c("X", "Y"), 
  stringsAsFactors = FALSE
)
df <- df[rep(1:nrow(df), each = c(2, 5, 5, 5, 5, 5)), ]  # Unbalanced: 2 vs 5
df$measure <- rnorm(nrow(df))

cat("Data structure:\n")
print(table(df$GROUP, df$TREATMENT))

cat("\nRunning ART test...\n")
result <- nonparametric_tests$perform_art2way(df = df, x_axis = c("GROUP", "TREATMENT"), measure_col = "measure")
print(result)
