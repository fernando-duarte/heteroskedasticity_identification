#!/usr/bin/env Rscript

#' Build Local Documentation for hetid Package
#'
#' This script builds all documentation components locally:
#' 1. Updates roxygen2 documentation
#' 2. Builds package vignettes
#' 3. Creates pkgdown website
#' 4. Optionally builds PDF manual
#'
#' Usage: Rscript build_local_documentation.R [--pdf]

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
build_pdf <- "--pdf" %in% args

# Load required packages
required_packages <- c("devtools", "roxygen2", "pkgdown", "knitr", "rmarkdown")

# Check and install missing packages
missing_packages <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages)
}

# Set up logging
log_file <- paste0("documentation_build_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
message("Starting documentation build. Logging to: ", log_file)

# Function to log messages
log_message <- function(msg, ...) {
  full_msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", msg, ...)
  message(full_msg)
  cat(full_msg, "\n", file = log_file, append = TRUE)
}

# Start documentation build
log_message("==== Starting hetid Documentation Build ====")

# Step 1: Update roxygen documentation
log_message("\n1. Updating roxygen2 documentation...")
tryCatch({
  devtools::document()
  log_message("   ✓ Roxygen documentation updated successfully")
}, error = function(e) {
  log_message("   ✗ Error updating roxygen documentation: ", e$message)
  stop("Documentation build failed at roxygen2 step")
})

# Step 2: Build vignettes
log_message("\n2. Building package vignettes...")
tryCatch({
  devtools::build_vignettes()

  # Copy built vignettes to inst/doc if they don't exist there
  if (!dir.exists("inst/doc")) {
    dir.create("inst/doc", recursive = TRUE)
  }

  # List vignettes that were built
  vignette_files <- list.files("doc", pattern = "\\.(html|pdf)$", full.names = TRUE)
  if (length(vignette_files) > 0) {
    log_message("   Built vignettes:")
    for (vf in vignette_files) {
      log_message("     - ", basename(vf))
    }
  }

  log_message("   ✓ Vignettes built successfully")
}, error = function(e) {
  log_message("   ✗ Error building vignettes: ", e$message)
  log_message("   Continuing with documentation build...")
})

# Step 3: Build pkgdown site
log_message("\n3. Building pkgdown website...")
tryCatch({
  # Clean previous build
  if (dir.exists("docs")) {
    log_message("   Cleaning previous pkgdown build...")
  }

  # Build the site
  pkgdown::build_site(preview = FALSE, quiet = FALSE)

  log_message("   ✓ pkgdown site built successfully")
  log_message("   Website available at: docs/index.html")
}, error = function(e) {
  log_message("   ✗ Error building pkgdown site: ", e$message)
  log_message("   Continuing with documentation build...")
})

# Step 4: Optionally build PDF manual
if (build_pdf) {
  log_message("\n4. Building PDF manual...")
  tryCatch({
    # Check if LaTeX is available
    if (!tinytex::is_tinytex() && !tinytex::tinytex_root()) {
      log_message("   Installing TinyTeX for PDF generation...")
      tinytex::install_tinytex()
    }

    # Build PDF manual
    devtools::build_manual(path = ".")

    # Find the generated PDF
    pdf_files <- list.files(".", pattern = "hetid.*\\.pdf$", full.names = TRUE)
    if (length(pdf_files) > 0) {
      log_message("   ✓ PDF manual built successfully: ", pdf_files[1])
    } else {
      log_message("   ⚠ PDF manual may have been built but couldn't locate it")
    }
  }, error = function(e) {
    log_message("   ✗ Error building PDF manual: ", e$message)
    log_message("   Note: PDF manual requires LaTeX. Install with: tinytex::install_tinytex()")
  })
}

# Step 5: Generate documentation summary
log_message("\n5. Generating documentation summary...")

# Count documentation items
n_functions <- length(list.files("man", pattern = "\\.Rd$"))
n_vignettes <- length(list.files("vignettes", pattern = "\\.Rmd$"))
n_examples <- length(list.files("examples", pattern = "\\.R$"))

log_message("\n==== Documentation Build Summary ====")
log_message("Package: hetid v", as.character(packageVersion("hetid")))
log_message("Documentation components:")
log_message("  - Documented functions: ", n_functions)
log_message("  - Vignettes: ", n_vignettes)
log_message("  - Example scripts: ", n_examples)
log_message("\nOutput locations:")
log_message("  - Function documentation: man/")
log_message("  - Built vignettes: doc/")
log_message("  - pkgdown website: docs/index.html")
if (build_pdf) {
  log_message("  - PDF manual: hetid_*.pdf")
}

# Step 6: Create quick access HTML page
log_message("\n6. Creating documentation index...")
tryCatch({
  index_content <- paste0('<!DOCTYPE html>
<html>
<head>
    <title>hetid Package Documentation</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 40px; }
        h1 { color: #2e5090; }
        h2 { color: #4a6fa5; margin-top: 30px; }
        ul { line-height: 1.8; }
        a { color: #2e5090; text-decoration: none; }
        a:hover { text-decoration: underline; }
        .section { margin-bottom: 30px; }
    </style>
</head>
<body>
    <h1>hetid: Identification Through Heteroskedasticity</h1>
    <p>Local documentation for the hetid R package implementing Lewbel (2012) method.</p>

    <div class="section">
        <h2>Package Website</h2>
        <ul>
            <li><a href="docs/index.html">Full pkgdown website</a></li>
            <li><a href="docs/reference/index.html">Function reference</a></li>
        </ul>
    </div>

    <div class="section">
        <h2>Vignettes</h2>
        <ul>
            <li><a href="doc/getting-started.html">Getting Started with hetid</a></li>
            <li><a href="doc/package-comparison.html">Comparing Lewbel (2012) Implementations</a></li>
            <li><a href="doc/degrees-of-freedom.html">Degrees of Freedom Adjustments</a></li>
        </ul>
    </div>

    <div class="section">
        <h2>Additional Resources</h2>
        <ul>
            <li><a href="NEWS.md">Package NEWS</a></li>
            <li><a href="README.md">README</a></li>
            <li><a href="https://github.com/fernando-duarte/heteroskedasticity_identification">GitHub Repository</a></li>
        </ul>
    </div>

    <p style="margin-top: 50px; color: #666; font-size: 0.9em;">
        Generated on ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '</p>
</body>
</html>')

  writeLines(index_content, "documentation_index.html")
  log_message("   ✓ Created documentation_index.html")
}, error = function(e) {
  log_message("   ✗ Error creating documentation index: ", e$message)
})

log_message("\n==== Documentation Build Complete ====")
log_message("View local documentation at: documentation_index.html")
log_message("Full build log available at: ", log_file)

# Open documentation in browser
if (interactive()) {
  answer <- readline("Open documentation in browser? (y/n): ")
  if (tolower(answer) == "y") {
    browseURL("documentation_index.html")
  }
}
