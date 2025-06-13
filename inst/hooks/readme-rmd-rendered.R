#!/usr/bin/env Rscript

# Check if README.Rmd and README.md are in sync
# This uses your system R installation

# Check if README.Rmd exists
if (!file.exists("README.Rmd")) {
  # No README.Rmd, so nothing to check
  quit(status = 0)
}

# Check if rmarkdown is installed
if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("rmarkdown package is not installed. Please install it with: install.packages('rmarkdown')")
}

# Get modification times
rmd_time <- file.info("README.Rmd")$mtime
md_exists <- file.exists("README.md")
md_time <- if (md_exists) file.info("README.md")$mtime else NA

# Check if README.md needs updating
if (!md_exists || rmd_time > md_time) {
  cat("README.md is out of sync with README.Rmd\n")
  cat("Please run: rmarkdown::render('README.Rmd', output_format = 'github_document')\n")
  quit(status = 1)
}
