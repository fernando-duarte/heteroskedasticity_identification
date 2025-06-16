#!/usr/bin/env Rscript

# Update roxygen documentation
# This uses your system R installation with working packages

# Check if roxygen2 is installed
if (!requireNamespace("roxygen2", quietly = TRUE)) {
  stop(
    "roxygen2 package is not installed. Please install it with: ",
    "install.packages('roxygen2')"
  )
}

# Get the current documentation state
old_rd_files <- list.files("man", pattern = "\\.Rd$", full.names = TRUE)
old_rd_content <- lapply(old_rd_files, readLines)
names(old_rd_content) <- old_rd_files

# Read NAMESPACE content
old_namespace <- readLines("NAMESPACE")

# Run roxygenize
roxygen2::roxygenise()

# Check if documentation changed
new_rd_files <- list.files("man", pattern = "\\.Rd$", full.names = TRUE)
new_namespace <- readLines("NAMESPACE")

# Check for changes
changed <- FALSE

# Check NAMESPACE
if (!identical(old_namespace, new_namespace)) {
  changed <- TRUE
  cat("NAMESPACE was updated\n")
}

# Check Rd files
if (!setequal(old_rd_files, new_rd_files)) {
  changed <- TRUE
  cat("Documentation files were added or removed\n")
} else {
  for (file in new_rd_files) {
    if (file %in% names(old_rd_content)) {
      new_content <- readLines(file)
      if (!identical(old_rd_content[[file]], new_content)) {
        changed <- TRUE
        cat("Documentation updated:", file, "\n")
      }
    }
  }
}

# Exit with non-zero status if changes were made
if (changed) {
  quit(status = 1)
}
