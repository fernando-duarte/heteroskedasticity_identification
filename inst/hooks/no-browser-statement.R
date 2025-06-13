#!/usr/bin/env Rscript

# Check for browser() statements in R code
# This uses your system R installation

args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0) {
  has_browser <- FALSE
  
  for (file in args) {
    # Read file content
    content <- readLines(file, warn = FALSE)
    
    # Check for browser() statements (not in comments)
    for (i in seq_along(content)) {
      line <- content[i]
      # Remove comments first
      line_without_comment <- sub("#.*$", "", line)
      
      # Check for browser()
      if (grepl("\\bbrowser\\s*\\(", line_without_comment)) {
        has_browser <- TRUE
        cat("browser() statement found in", file, "at line", i, ":\n")
        cat("  ", content[i], "\n")
      }
    }
  }
  
  # Exit with non-zero status if browser() was found
  if (has_browser) {
    cat("\nPlease remove browser() statements before committing.\n")
    quit(status = 1)
  }
} 