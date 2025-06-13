#!/usr/bin/env Rscript

# Check for debug() and undebug() statements in R code
# This uses your system R installation

args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0) {
  has_debug <- FALSE

  for (file in args) {
    # Read file content
    content <- readLines(file, warn = FALSE)

    # Check for debug() or undebug() statements (not in comments)
    for (i in seq_along(content)) {
      line <- content[i]
      # Remove comments first
      line_without_comment <- sub("#.*$", "", line)

      # Check for debug() or undebug()
      if (grepl("\\b(un)?debug\\s*\\(", line_without_comment)) {
        has_debug <- TRUE
        cat("debug() statement found in", file, "at line", i, ":\n")
        cat("  ", content[i], "\n")
      }
    }
  }

  # Exit with non-zero status if debug() was found
  if (has_debug) {
    cat("\nPlease remove debug() and undebug() statements before committing.\n")
    quit(status = 1)
  }
}
