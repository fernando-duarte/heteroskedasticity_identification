#!/usr/bin/env Rscript

# Style R files using styler
# This uses your system R installation with working packages

args <- commandArgs(trailingOnly = TRUE)

# Enable caching if --cache flag is present
use_cache <- "--cache" %in% args
files <- setdiff(args, "--cache")

# Check if styler is installed
if (!requireNamespace("styler", quietly = TRUE)) {
  stop(
    "styler package is not installed. ",
    "Please install it with: install.packages('styler')"
  )
}

# Activate cache for performance if requested
if (use_cache) {
  styler::cache_activate()
  cat("Styler cache activated\n")
}

# Style the files
if (length(files) > 0) {
  changed_files <- FALSE

  for (file in files) {
    # Check if file needs styling
    original_content <- readLines(file)

    # Style the file with explicit tidyverse settings
    styler::style_file(
      file,
      style = styler::tidyverse_style(
        indent_by = 2,
        strict = TRUE
      )
    )

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
