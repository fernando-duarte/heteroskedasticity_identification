#!/usr/bin/env Rscript

# Keep DESCRIPTION file tidy
# This uses your system R installation with working packages

# Check if desc is installed
if (!requireNamespace("desc", quietly = TRUE)) {
  stop("desc package is not installed. Please install it with: install.packages('desc')")
}

# Read current DESCRIPTION
old_content <- readLines("DESCRIPTION")

# Create desc object and normalize
desc_obj <- desc::desc()
desc_obj$normalize()

# Write back (this writes to DESCRIPTION)
desc_obj$write()

# Read new content
new_content <- readLines("DESCRIPTION")

# Check if changed
if (!identical(old_content, new_content)) {
  cat("DESCRIPTION file was tidied\n")
  quit(status = 1)
}
