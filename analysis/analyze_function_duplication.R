#!/usr/bin/env Rscript

# =============================================================================
# Function Duplication Analyzer for TexAn2.0
# Identifies replicated functions and suggests consolidation opportunities
# =============================================================================

# Load required libraries (minimal dependencies only)
# install.packages(c("dplyr", "purrr", "magrittr", "readr", "stringr", "here", "cli"))
library(dplyr)
library(purrr)
library(magrittr)
library(readr)
library(stringr)
library(here)
library(cli)


# =============================================================================
# Core Analysis Functions
# =============================================================================

#' Extract all function definitions from R files
#' @param root_dir Root directory to search
#' @return Tibble with function info
extract_functions <- function(root_dir = here::here("app/logic")) {
  
  # Find all R files (excluding external sources)
  r_files <- list.files(root_dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  r_files <- r_files[!str_detect(r_files, "Rallfun-v43\\.R")]
  
  # Extract function definitions from each file
  function_data <- map_dfr(r_files, function(file_path) {
    content <- readLines(file_path, warn = FALSE)
    
    # Find function definitions (multiple patterns)
    function_patterns <- c(
      "^([a-zA-Z_][a-zA-Z0-9_\\.]*)(\\s*<-\\s*function\\s*\\()",  # standard R
      "^(\\w+)(\\s*=\\s*function\\s*\\()",  # alternative assignment
      "^([a-zA-Z_][a-zA-Z0-9_\\.]*)(\\s*<-\\s*\\\\(.*\\\\)\\s*function\\s*\\()"  # complex
    )
    
    functions <- tibble(
      line_number = seq_along(content),
      line = content
    ) %>%
      mutate(
        function_name = str_extract(line, paste(function_patterns, collapse = "|")),
        function_name = str_extract(function_name, "^[a-zA-Z_][a-zA-Z0-9_\\.]*")
      ) %>%
      filter(!is.na(function_name)) %>%
      mutate(
        file_path = file_path,
        module = str_extract(file_path, "app/logic/([^/]+)"),
        submodule = str_extract(file_path, "app/logic/[^/]+/([^/]+)")
      )
    
    # Extract function signatures and documentation
    if (nrow(functions) > 0) {
      functions <- functions %>%
        mutate(
          signature = str_extract(line, "function\\s*\\([^)]*\\)"),
          has_export = str_detect(line, "@export"),
          doc_comment = map_chr(line_number, ~ extract_doc_comment(content, .x))
        )
    }
    
    return(functions)
  })
  
  return(function_data)
}

#' Extract documentation comment above function
#' @param content File content as character vector
#' @param line_num Function line number
#' @return Documentation string or NA
extract_doc_comment <- function(content, line_num) {
  if (is.na(line_num) || line_num <= 1) return(NA_character_)
  
 # Look backwards from the line before the function definition
  start_line <- line_num - 1
  if (start_line < 1) return(NA_character_)
  
  # Collect all consecutive roxygen comments going backwards
  doc_lines <- c()
  for (i in start_line:1) {
    line <- str_trim(content[i])
    if (str_detect(line, "^#'")) {
      doc_lines <- c(line, doc_lines)  # prepend to maintain order
    } else if (line == "") {
      next  # skip empty lines
    } else {
      break  # stop at non-comment, non-empty line
    }
  }
  
  if (length(doc_lines) > 0) {
    return(paste(doc_lines, collapse = "\n"))
  }
  
  return(NA_character_)
}

#' Analyze function similarity patterns
#' @param function_data Output from extract_functions
#' @return Analysis results
analyze_similarity <- function(function_data) {
  
  # 1. Exact name duplicates
  name_duplicates <- function_data %>%
    count(function_name, sort = TRUE) %>%
    filter(n > 1) %>%
    left_join(function_data, by = "function_name") %>%
    arrange(function_name, file_path)
  
  # 2. Pattern-based duplicates (common validation patterns)
  validation_patterns <- tibble(
    pattern = c(
      "validate.*input",
      "check.*data",
      "verify.*column",
      "validate.*column",
      "check.*parameter"
    ),
    pattern_name = c(
      "input_validation",
      "data_validation", 
      "column_validation",
      "column_validation_alt",
      "parameter_validation"
    )
  )
  
  pattern_matches <- function_data %>%
    mutate(
      matched_patterns = map_chr(function_name, ~{
        for (i in seq_len(nrow(validation_patterns))) {
          if (str_detect(.x, validation_patterns$pattern[i])) {
            return(validation_patterns$pattern_name[i])
          }
        }
        return(NA_character_)
      })
    ) %>%
    filter(!is.na(matched_patterns)) %>%
    select(function_name, matched_patterns, file_path, module, doc_comment)
  
  # 3. Semantic similarity based on documentation
  semantic_groups <- function_data %>%
    filter(!is.na(doc_comment)) %>%
    mutate(
      doc_keywords = str_extract_all(doc_comment, "@[a-zA-Z]+|validate|check|verify|compute|calculate|plot|generate") %>% 
        map(~ paste(.x, collapse = " ")),
      doc_group = case_when(
        str_detect(doc_comment, "validate|check|verify") ~ "validation",
        str_detect(doc_comment, "compute|calculate") ~ "computation", 
        str_detect(doc_comment, "plot|visualize") ~ "visualization",
        str_detect(doc_comment, "export|save|write") ~ "io_operations",
        TRUE ~ "other"
      )
    ) %>%
    count(doc_group, function_name, file_path, sort = TRUE) %>%
    group_by(doc_group) %>%
    filter(n() > 1) %>%
    ungroup()
  
  return(list(
    name_duplicates = name_duplicates,
    pattern_matches = pattern_matches,
    semantic_groups = semantic_groups
  ))
}

#' Generate consolidation recommendations
#' @param analysis_results Output from analyze_similarity
#' @return Detailed recommendations
generate_recommendations <- function(analysis_results) {
  
  recommendations <- list()
  
  # 1. Exact duplicates - recommend shared utility
  if (nrow(analysis_results$name_duplicates) > 0) {
    dup_names <- unique(analysis_results$name_duplicates$function_name)
    recommendations$exact_duplicates <- map_dfr(dup_names, function(func_name) {
      dup_data <- analysis_results$name_duplicates %>%
        filter(function_name == func_name)
      
      tibble(
        function_name = func_name,
        duplication_count = nrow(dup_data),
        files = paste(dup_data$file_path, collapse = "\n"),
        modules = paste(unique(dup_data$module), collapse = ", "),
        doc_comments = paste(
          paste0("[", basename(dup_data$file_path), "] ", 
                 ifelse(is.na(dup_data$doc_comment), "(no docs)", 
                        str_extract(dup_data$doc_comment, "#'[^\n]*"))), 
          collapse = "\n"
        ),
        recommendation = "Create shared utility in app/logic/shared/",
        priority = if_else(nrow(dup_data) > 3, "HIGH", "MEDIUM")
      )
    })
  }
  
  # 2. Pattern duplicates - recommend generic functions
  if (nrow(analysis_results$pattern_matches) > 0) {
    recommendations$pattern_duplicates <- analysis_results$pattern_matches %>%
    group_by(matched_patterns) %>%
    summarise(
      function_count = n(),
      functions = paste(unique(function_name), collapse = ", "),
      files = paste(unique(file_path), collapse = "\n"),
      modules = paste(unique(module), collapse = ", "),
      recommendation = paste("Create generic", unique(matched_patterns), "function"),
      priority = case_when(
        function_count > 5 ~ "HIGH",
        function_count > 3 ~ "MEDIUM", 
        TRUE ~ "LOW"
      ),
      .groups = "drop"
    )
  }
  
  # 3. Semantic groups - recommend module consolidation
  if (nrow(analysis_results$semantic_groups) > 0) {
    recommendations$semantic_consolidation <- analysis_results$semantic_groups %>%
      group_by(doc_group) %>%
      summarise(
        function_count = n(),
        functions = paste(function_name, collapse = ", "),
        recommendation = paste("Consider consolidating", doc_group, "functions"),
        priority = if_else(function_count > 5, "MEDIUM", "LOW"),
        .groups = "drop"
      )
  }
  
  return(recommendations)
}

#' Generate detailed report
#' @param analysis_results Analysis results
#' @param recommendations Consolidation recommendations
generate_report <- function(analysis_results, recommendations) {
  
  cat(cli::col_blue("========================================\n"))
  cat(cli::col_blue("TEXAN2.0 FUNCTION DUPLICATION ANALYSIS\n"))
  cat(cli::col_blue("========================================\n\n"))
  
  # Summary
  cat(cli::col_green("SUMMARY:\n"))
  cat("- Exact name duplicates:", nrow(recommendations$exact_duplicates %>% filter(!is.na(function_name))), "\n")
  cat("- Pattern-based duplicates:", nrow(recommendations$pattern_duplicates), "\n") 
  cat("- Semantic groups for consolidation:", nrow(recommendations$semantic_consolidation), "\n\n")
  
  # Exact duplicates
  if (!is.null(recommendations$exact_duplicates) && nrow(recommendations$exact_duplicates) > 0) {
    cat(cli::col_red("HIGH PRIORITY: EXACT FUNCTION NAME DUPLICATES\n"))
    cat(cli::col_red("=============================================\n\n"))
    
    for (i in seq_len(nrow(recommendations$exact_duplicates))) {
      dup <- recommendations$exact_duplicates[i, ]
      cat(cli::col_yellow(paste("Function:", dup$function_name, "(", dup$duplication_count, "occurrences)\n")))
      cat("Files:\n", dup$files, "\n")
      cat("Modules:", dup$modules, "\n")
      cat("Recommendation:", dup$recommendation, "\n")
      cat("Priority:", dup$priority, "\n\n")
    }
  }
  
  # Pattern duplicates  
  if (!is.null(recommendations$pattern_duplicates) && nrow(recommendations$pattern_duplicates) > 0) {
    cat(cli::col_yellow("MEDIUM PRIORITY: PATTERN-BASED DUPLICATES\n"))
    cat(cli::col_yellow("========================================\n\n"))
    
    for (i in seq_len(nrow(recommendations$pattern_duplicates))) {
      pat <- recommendations$pattern_duplicates[i, ]
      cat(cli::col_yellow(paste("Pattern:", pat$matched_patterns, "(", pat$function_count, "functions)\n")))
      cat("Functions:", pat$functions, "\n")
      cat("Modules:", pat$modules, "\n") 
      cat("Recommendation:", pat$recommendation, "\n")
      cat("Priority:", pat$priority, "\n\n")
    }
  }
  
  # Semantic consolidation
  if (!is.null(recommendations$semantic_consolidation) && nrow(recommendations$semantic_consolidation) > 0) {
    cat(cli::col_cyan("LOW PRIORITY: SEMANTIC CONSOLIDATION OPPORTUNITIES\n"))
    cat(cli::col_cyan("================================================\n\n"))
    
    for (i in seq_len(nrow(recommendations$semantic_consolidation))) {
      sem <- recommendations$semantic_consolidation[i, ]
      cat(cli::col_cyan(paste("Group:", sem$doc_group, "(", sem$function_count, "functions)\n")))
      cat("Functions:", sem$functions, "\n")
      cat("Recommendation:", sem$recommendation, "\n")
      cat("Priority:", sem$priority, "\n\n")
    }
  }
  
  cat(cli::col_green("ANALYSIS COMPLETE!\n"))
}

# =============================================================================
# Main Execution
# =============================================================================

# Run the analysis
main <- function() {
  cli::cli_h1("Starting TexAn2.0 Function Analysis")
  
  # Extract all functions
  cli::cli_h2("Extracting functions from R files...")
  function_data <- extract_functions()
  cli::cli_alert_info(paste("Found", nrow(function_data), "functions"))
  
  # Prepare output: all functions with their documentation
  output_data <- function_data %>%
    select(
      function_name,
      file_path,
      module,
      line_number,
      signature,
      doc_comment
    ) %>%
    arrange(module, file_path, line_number)
  
  # Save all functions to CSV
  output_file <- here::here("function_analysis.csv")
  write_csv(output_data, output_file)
  cli::cli_alert_success(paste("All functions saved to", output_file))
  
  # Print summary
  cli::cli_h2("Summary by module:")
  module_summary <- function_data %>%
    count(module, sort = TRUE)
  print(module_summary)
}

# Run if executed directly
if (!interactive()) {
  main()
}