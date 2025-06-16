#!/usr/bin/env Rscript

# Style R files using styler
# This uses your system R installation with working packages

args <- commandArgs(trailingOnly = TRUE)

# Check if styler is installed
if (!requireNamespace("styler", quietly = TRUE)) {
  stop(
    "styler package is not installed. ",
    "Please install it with: install.packages('styler')"
  )
}

# Style the files
if (length(args) > 0) {
  changed_files <- FALSE

  for (file in args) {
    # Check if file needs styling
    original_content <- readLines(file)

    # Style the file
    styler::style_file(file, style = styler::tidyverse_style)

    # Check if file was changed
    new_content <- readLines(file)
    if (!identical(original_content, new_content)) {
      changed_files <- TRUE
      cat("Styled:", file, "\n")
    }
  }

  # Exit with non-zero status if files were changed
  if (changed_files) {
    quit(status = 1)
  }
}
