#!/usr/bin/env Rscript

# Spell check R files
# This uses your system R installation with working packages

args <- commandArgs(trailingOnly = TRUE)

# Check if spelling is installed
if (!requireNamespace("spelling", quietly = TRUE)) {
  stop("spelling package is not installed. Please install it with: install.packages('spelling')")
}

# Load custom wordlist if it exists
wordlist <- character()
if (file.exists("inst/WORDLIST")) {
  wordlist <- readLines("inst/WORDLIST")
}

# Spell check the files
if (length(args) > 0) {
  has_errors <- FALSE
  
  for (file in args) {
    # Check spelling
    errors <- spelling::spell_check_files(file, ignore = wordlist)
    
    if (nrow(errors) > 0) {
      has_errors <- TRUE
      cat("\nSpelling errors in", file, ":\n")
      print(errors)
    }
  }
  
  # Exit with non-zero status if spelling errors were found
  if (has_errors) {
    cat("\nTo add words to the ignore list, add them to inst/WORDLIST\n")
    quit(status = 1)
  }
} 