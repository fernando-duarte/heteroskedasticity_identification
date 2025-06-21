# Helper functions for test categorization and skipping
# This file is automatically loaded before tests run

# Check test level environment variable
get_test_level <- function() {
  level <- Sys.getenv("HETID_TEST_LEVEL", "")
  if (level == "") {
    # Default based on environment
    if (nzchar(Sys.getenv("NOT_CRAN"))) {
      return("fast") # Local development default
    } else {
      return("cran") # CRAN default
    }
  }
  level
}

# Skip functions for different test categories
skip_if_not_cran_test <- function() {
  level <- get_test_level()
  skip_if_not(
    level %in% c("cran", "fast", "integration", "comprehensive"),
    "Skipping: not running CRAN tests"
  )
}

skip_if_not_fast_test <- function() {
  level <- get_test_level()
  skip_if_not(
    level %in% c("fast", "integration", "comprehensive"),
    "Skipping: not running fast tests"
  )
}

skip_if_not_integration_test <- function() {
  level <- get_test_level()
  skip_if_not(
    level %in% c("integration", "comprehensive"),
    "Skipping: not running integration tests"
  )
}

skip_if_not_comprehensive_test <- function() {
  level <- get_test_level()
  skip_if_not(
    level == "comprehensive",
    "Skipping: not running comprehensive tests"
  )
}

# Helper to determine if we're on CRAN
is_on_cran <- function() {
  !isTRUE(as.logical(Sys.getenv("NOT_CRAN")))
}

# Helper to determine if we're in CI/CD
is_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI"))) ||
    isTRUE(as.logical(Sys.getenv("GITHUB_ACTIONS")))
}

# For backward compatibility with existing skip_on_cran() calls
# We'll use these to categorize tests
is_extended_test_environment <- function() {
  level <- get_test_level()
  level %in% c("integration", "comprehensive")
}
