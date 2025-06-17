# R Profile for hetid package development
# Sets consistent repository configuration to avoid p3m.dev snapshot issues

# Set CRAN repository to the official cloud mirror (recommended 2025)
options(repos = c(
  CRAN = "https://cloud.r-project.org/"
))

# Additional development-friendly options
options(
  # Increase timeout for slow connections
  timeout = 300,

  # Use libcurl for better download reliability
  download.file.method = "libcurl",

  # Suppress startup messages for cleaner output
  repos.check = FALSE,

  # Set default number of digits for output
  digits = 4
)

# Suppress the specific p3m.dev warning if it still appears
suppressWarnings({
  # This ensures any lingering p3m.dev configurations don't cause issues
  if (any(grepl("p3m\\.dev", getOption("repos")))) {
    options(repos = c(CRAN = "https://cloud.r-project.org/"))
  }
})

# Print confirmation (only in interactive sessions)
if (interactive()) {
  cat("✓ Repository set to:", getOption("repos")["CRAN"], "\n")
}
