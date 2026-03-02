#!/usr/bin/env Rscript

# =============================================================================
# Semantic Function Analysis
# Identifies functions with similar purposes based on documentation
# =============================================================================

library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(here)
library(cli)

# Read the function analysis
function_data <- read_csv(here::here("function_analysis.csv"), show_col_types = FALSE)

# Extract key semantic patterns from documentation
analyze_semantic_groups <- function(data) {
  
  # Define semantic categories with keywords
  semantic_patterns <- list(
    validation = c("validate", "check", "verify", "ensure"),
    computation = c("compute", "calculate", "run", "perform", "execute"),
    plotting = c("plot", "visualize", "create.*plot", "generate.*plot", "draw"),
    export = c("export", "save", "write", "output"),
    transformation = c("transform", "convert", "normalize", "scale", "standardize"),
    filtering = c("filter", "select", "subset", "extract"),
    error_handling = c("error", "handle.*error", "parse.*error", "catch"),
    formatting = c("format", "pretty", "display", "render"),
    statistical = c("test", "statistical", "significance", "p-value", "hypothesis"),
    data_prep = c("prepare", "clean", "preprocess", "setup")
  )
  
  # Categorize each function
  data_categorized <- data %>%
    mutate(
      # Extract first line of doc comment for quick comparison
      doc_first_line = str_extract(doc_comment, "#'[^\n]*") %>% 
        str_remove("#'") %>% 
        str_trim(),
      
      # Categorize by semantic pattern
      semantic_category = map_chr(doc_comment, function(doc) {
        if (is.na(doc)) return("undocumented")
        
        doc_lower <- tolower(doc)
        matches <- c()
        
        for (category in names(semantic_patterns)) {
          patterns <- semantic_patterns[[category]]
          if (any(str_detect(doc_lower, patterns))) {
            matches <- c(matches, category)
          }
        }
        
        if (length(matches) == 0) return("other")
        return(paste(matches, collapse = ", "))
      })
    )
  
  return(data_categorized)
}

# Find similar functions within categories
find_similar_functions <- function(data_categorized) {
  
  similar_groups <- list()
  
  # Group by semantic category
  for (category in unique(data_categorized$semantic_category)) {
    if (category %in% c("undocumented", "other")) next
    
    category_funcs <- data_categorized %>%
      filter(semantic_category == category) %>%
      select(function_name, file_path, module, doc_first_line, doc_comment)
    
    if (nrow(category_funcs) > 1) {
      similar_groups[[category]] <- category_funcs
    }
  }
  
  return(similar_groups)
}

# Identify potential consolidation opportunities
identify_consolidation <- function(similar_groups) {
  
  consolidation_report <- list()
  
  for (category in names(similar_groups)) {
    funcs <- similar_groups[[category]]
    
    # Look for functions with very similar first lines
    first_line_groups <- funcs %>%
      group_by(doc_first_line) %>%
      filter(n() > 1) %>%
      ungroup()
    
    if (nrow(first_line_groups) > 0) {
      consolidation_report[[paste0(category, "_exact_match")]] <- first_line_groups
    }
    
    # Look for functions with similar keywords in different modules
    if (nrow(funcs) > 1) {
      # Extract key action words
      funcs_with_actions <- funcs %>%
        mutate(
          action_words = str_extract_all(
            tolower(doc_first_line), 
            "validate|check|compute|calculate|create|generate|format|export|transform|filter"
          ) %>% 
            map_chr(~ paste(.x, collapse = ", "))
        ) %>%
        filter(action_words != "")
      
      # Group by action words across modules
      action_groups <- funcs_with_actions %>%
        group_by(action_words) %>%
        filter(n() > 1, n_distinct(module) > 1) %>%
        ungroup()
      
      if (nrow(action_groups) > 0) {
        consolidation_report[[paste0(category, "_cross_module")]] <- action_groups
      }
    }
  }
  
  return(consolidation_report)
}

# Generate report
generate_semantic_report <- function(data_categorized, similar_groups, consolidation_report) {
  
  cat(cli::col_blue("========================================\n"))
  cat(cli::col_blue("SEMANTIC FUNCTION ANALYSIS\n"))
  cat(cli::col_blue("========================================\n\n"))
  
  # Summary by category
  cat(cli::col_green("FUNCTIONS BY SEMANTIC CATEGORY:\n"))
  category_summary <- data_categorized %>%
    count(semantic_category, sort = TRUE)
  print(category_summary)
  cat("\n")
  
  # Consolidation opportunities
  if (length(consolidation_report) > 0) {
    cat(cli::col_red("CONSOLIDATION OPPORTUNITIES:\n"))
    cat(cli::col_red("============================\n\n"))
    
    for (report_name in names(consolidation_report)) {
      report_data <- consolidation_report[[report_name]]
      
      cat(cli::col_yellow(paste0("\n", toupper(report_name), ":\n")))
      cat(paste0("Found ", nrow(report_data), " functions\n\n"))
      
      # Show grouped functions
      if ("doc_first_line" %in% names(report_data)) {
        for (desc in unique(report_data$doc_first_line)) {
          matching_funcs <- report_data %>% filter(doc_first_line == desc)
          cat(cli::col_cyan(paste0("Description: ", desc, "\n")))
          cat("Functions:\n")
          for (i in seq_len(nrow(matching_funcs))) {
            cat(paste0("  - ", matching_funcs$function_name[i], 
                      " (", basename(matching_funcs$file_path[i]), ")\n"))
          }
          cat("\n")
        }
      }
      
      if ("action_words" %in% names(report_data)) {
        for (action in unique(report_data$action_words)) {
          matching_funcs <- report_data %>% filter(action_words == action)
          cat(cli::col_cyan(paste0("Action pattern: ", action, "\n")))
          cat("Functions across modules:\n")
          for (i in seq_len(nrow(matching_funcs))) {
            cat(paste0("  - ", matching_funcs$function_name[i], 
                      " [", matching_funcs$module[i], "]\n"))
            cat(paste0("    ", matching_funcs$doc_first_line[i], "\n"))
          }
          cat("\n")
        }
      }
    }
  }
  
  # Save detailed results
  output_file <- here::here("semantic_analysis.csv")
  write_csv(data_categorized, output_file)
  cli::cli_alert_success(paste("Detailed categorization saved to", output_file))
}

# Main execution
main <- function() {
  cli::cli_h1("Semantic Function Analysis")
  
  cli::cli_h2("Categorizing functions by purpose...")
  data_categorized <- analyze_semantic_groups(function_data)
  
  cli::cli_h2("Finding similar functions...")
  similar_groups <- find_similar_functions(data_categorized)
  
  cli::cli_h2("Identifying consolidation opportunities...")
  consolidation_report <- identify_consolidation(similar_groups)
  
  generate_semantic_report(data_categorized, similar_groups, consolidation_report)
}

# Run
if (!interactive()) {
  main()
}
