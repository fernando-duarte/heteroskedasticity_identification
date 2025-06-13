#!/usr/bin/env Rscript

# Check if all package dependencies are declared in DESCRIPTION
# This uses your system R installation

args <- commandArgs(trailingOnly = TRUE)

# Check if desc is installed
if (!requireNamespace("desc", quietly = TRUE)) {
  stop("desc package is not installed. Please install it with: install.packages('desc')")
}

if (length(args) > 0) {
  # Get declared dependencies from DESCRIPTION
  desc_obj <- desc::desc()
  declared_deps <- unique(c(
    desc_obj$get_deps()$package,
    "base", "methods", "utils", "stats", "graphics", "grDevices", "datasets"  # Base packages
  ))
  
  missing_deps <- character()
  
  for (file in args) {
    # Read file content
    content <- readLines(file, warn = FALSE)
    
    # Find library() and require() calls
    for (i in seq_along(content)) {
      line <- content[i]
      # Remove comments
      line_without_comment <- sub("#.*$", "", line)
      
      # Check for library() or require()
      lib_match <- gregexpr("\\b(library|require)\\s*\\(\\s*['\"]?([^)'\"]+)['\"]?\\s*\\)", line_without_comment)
      if (lib_match[[1]][1] != -1) {
        # Extract package names
        matches <- regmatches(line_without_comment, lib_match)[[1]]
        for (match in matches) {
          pkg_name <- sub(".*\\(\\s*['\"]?([^)'\"]+)['\"]?\\s*\\).*", "\\1", match)
          if (!pkg_name %in% declared_deps && !pkg_name %in% missing_deps) {
            missing_deps <- c(missing_deps, pkg_name)
            cat("Package", pkg_name, "used in", file, "line", i, "but not in DESCRIPTION\n")
          }
        }
      }
      
      # Check for :: usage
      ns_match <- gregexpr("\\b([a-zA-Z][a-zA-Z0-9.]*)::", line_without_comment)
      if (ns_match[[1]][1] != -1) {
        matches <- regmatches(line_without_comment, ns_match)[[1]]
        for (match in matches) {
          pkg_name <- sub("::$", "", match)
          if (!pkg_name %in% declared_deps && !pkg_name %in% missing_deps) {
            missing_deps <- c(missing_deps, pkg_name)
            cat("Package", pkg_name, "used in", file, "line", i, "but not in DESCRIPTION\n")
          }
        }
      }
    }
  }
  
  # Exit with non-zero status if missing dependencies were found
  if (length(missing_deps) > 0) {
    cat("\nPlease add these packages to DESCRIPTION:\n")
    cat("  Imports:", paste(missing_deps, collapse = ", "), "\n")
    quit(status = 1)
  }
} 