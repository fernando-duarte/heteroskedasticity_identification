# R Profile for hetid package development
# Sets consistent repository configuration to avoid p3m.dev snapshot issues

# Set CRAN repository to the official cloud mirror
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

# Set default test level for local development
# This ensures fast feedback during development while still running essential tests
Sys.setenv(
  HETID_TEST_LEVEL = "fast",
  NOT_CRAN = "true"
)

# Load development packages automatically in interactive sessions
if (interactive()) {
  suppressMessages({
    if (requireNamespace("devtools", quietly = TRUE)) {
      library(devtools)
    }
    if (requireNamespace("testthat", quietly = TRUE)) {
      library(testthat)
    }
  })
}

# Additional development options
options(
  # Existing options
  timeout = 300,
  download.file.method = "libcurl",
  repos.check = FALSE,
  digits = 4,

  # New development options
  warnPartialMatchDollar = TRUE,
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  useFancyQuotes = FALSE,
  deparse.max.lines = 2,
  usethis.quiet = FALSE,

  # Use parallel processing for package checks if available
  Ncpus = parallel::detectCores(logical = FALSE)
)

# Print confirmation (only in interactive sessions)
if (interactive()) {
  cat("✓ Repository set to:", getOption("repos")["CRAN"], "\n")
  message(
    "\n📦 Heteroskedasticity package development environment loaded\n",
    "   Test level: ", Sys.getenv("HETID_TEST_LEVEL"), " (fast tests + CRAN tests)\n",
    "   NOT_CRAN: ", Sys.getenv("NOT_CRAN"), "\n",
    "   \n",
    "   Quick commands:\n",
    "   • devtools::test()     - Run tests at current level\n",
    "   • devtools::check()    - Package check with fast tests\n",
    "   • make test-fast       - Run CRAN + fast tests\n",
    "   • make test-all        - Run all tests (comprehensive)\n",
    "   • make help            - Show all available commands\n"
  )
}
