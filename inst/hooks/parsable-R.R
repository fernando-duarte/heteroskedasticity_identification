#!/usr/bin/env Rscript

# Check if R code is parsable
# This uses your system R installation

args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0) {
  has_errors <- FALSE

  for (file in args) {
    # Try to parse the file
    result <- tryCatch(
      {
        parse(file = file)
        NULL
      },
      error = function(e) {
        return(e)
      }
    )

    if (!is.null(result)) {
      has_errors <- TRUE
      cat("Parse error in", file, ":\n")
      cat(conditionMessage(result), "\n\n")
    }
  }

  # Exit with non-zero status if parse errors were found
  if (has_errors) {
    quit(status = 1)
  }
}
