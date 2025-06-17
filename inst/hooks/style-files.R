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

# Style the files
if (length(files) > 0) {
  changed_files <- FALSE

  for (file in files) {
    # Check if file exists
    if (!file.exists(file)) {
      cat("Warning: File does not exist:", file, "\n")
      next
    }

    # Check if file needs styling
    original_content <- tryCatch(
      {
        readLines(file, warn = FALSE)
      },
      error = function(e) {
        cat("Error reading file:", file, "-", e$message, "\n")
        NULL
      }
    )

    if (is.null(original_content)) next

    # Style the file with 120 character line length
    tryCatch(
      {
        styler::style_file(
          file,
          dry = "off",
          transformers = styler::tidyverse_style(line_length = 120L)
        )
      },
      error = function(e) {
        cat("Error styling file:", file, "-", e$message, "\n")
      }
    )

    # Check if file was changed
    new_content <- tryCatch(
      {
        readLines(file, warn = FALSE)
      },
      error = function(e) {
        cat("Error reading styled file:", file, "-", e$message, "\n")
        original_content
      }
    )

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
