#!/usr/bin/env Rscript

# Lint R files using lintr
# This uses your system R installation with working packages

args <- commandArgs(trailingOnly = TRUE)

# Check if lintr is installed
if (!requireNamespace("lintr", quietly = TRUE)) {
  stop(
    "lintr package is not installed. Please install it with: ",
    "install.packages('lintr')"
  )
}

# Lint the files
if (length(args) > 0) {
  has_lints <- FALSE

  for (file in args) {
    lints <- lintr::lint(file)

    if (length(lints) > 0) {
      has_lints <- TRUE
      print(lints)
    }
  }

  # Exit with non-zero status if lints were found
  if (has_lints) {
    quit(status = 1)
  }
}
