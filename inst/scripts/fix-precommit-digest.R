#!/usr/bin/env Rscript

# Fix for digest package compilation issue on macOS
# The issue was fixed in digest 0.6.37 (August 2024)

cat("Checking digest package version...\n")

# Get current version if installed
current_version <- tryCatch(
  packageVersion("digest"),
  error = function(e) NULL
)

if (!is.null(current_version)) {
  cat("Current digest version:", as.character(current_version), "\n")
}

# Required version that fixes the Calloc/Free issue
required_version <- "0.6.37"

if (is.null(current_version) || current_version < required_version) {
  cat("Installing digest version", required_version, "or newer...\n")

  # Try to install from CRAN
  tryCatch(
    {
      install.packages("digest", repos = "https://cloud.r-project.org/")
      new_version <- packageVersion("digest")
      cat("Successfully installed digest version:", as.character(new_version), "\n")

      if (new_version < required_version) {
        cat("WARNING: Installed version is still older than required.\n")
        cat("You may need to update your R version or wait for CRAN to update.\n")
      }
    },
    error = function(e) {
      cat("Failed to install from CRAN:", conditionMessage(e), "\n")
      cat("Trying to install from source...\n")

      # Alternative: install from GitHub
      if (requireNamespace("remotes", quietly = TRUE)) {
        remotes::install_github("eddelbuettel/digest")
      } else {
        cat("Please install the 'remotes' package first:\n")
        cat("  install.packages('remotes')\n")
        cat("Then run:\n")
        cat("  remotes::install_github('eddelbuettel/digest')\n")
      }
    }
  )
} else {
  cat("digest package is already up to date.\n")
}

# Test the package
cat("\nTesting digest package...\n")
tryCatch(
  {
    library(digest)
    test_hash <- digest::digest("test")
    cat("✓ digest package is working correctly\n")
    cat("  Test hash:", test_hash, "\n")
  },
  error = function(e) {
    cat("✗ digest package test failed:", conditionMessage(e), "\n")
  }
)
