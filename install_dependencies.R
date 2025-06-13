# ============================================
# Install all dependencies for hetid package
# ============================================

cat("Installing dependencies for hetid package...\n\n")

# Core dependencies from DESCRIPTION file
required_packages <- c(
  "AER", # For instrumental variables regression (ivreg)
  "boot", # For bootstrap methods
  "dplyr", # For data manipulation
  "furrr", # For parallel processing with future
  "future", # For parallel processing backend
  "ggplot2", # For visualization
  "knitr", # For vignettes and documentation
  "magrittr", # For pipe operator %>%
  "purrr", # For functional programming
  "rlang", # For tidy evaluation
  "stats", # Base R stats (usually pre-installed)
  "tidyr", # For data tidying
  "utils" # Base R utils (usually pre-installed)
)

# Development dependencies (for testing and checking)
dev_packages <- c(
  "devtools", # For package development
  "testthat", # For unit testing
  "rmarkdown" # For rendering documentation
)

# Combine all packages
all_packages <- unique(c(required_packages, dev_packages))

# Function to install missing packages
install_if_missing <- function(packages) {
  # Check which packages are not installed
  missing <- packages[!packages %in% installed.packages()[, "Package"]]

  if (length(missing) > 0) {
    cat("Missing packages:", paste(missing, collapse = ", "), "\n")
    cat("Installing missing packages...\n\n")

    # Install missing packages
    for (pkg in missing) {
      cat("Installing", pkg, "...\n")
      install.packages(pkg, dependencies = TRUE)
    }
  } else {
    cat("All required packages are already installed!\n")
  }

  # Return list of what was installed
  missing
}

# Install missing packages
installed <- install_if_missing(all_packages)

# Verify installation
cat("\n============================================\n")
cat("Verifying installation...\n")
cat("============================================\n\n")

# Check each package
success <- TRUE
for (pkg in all_packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("✓", pkg, "- Successfully loaded\n")
  } else {
    cat("✗", pkg, "- Failed to load\n")
    success <- FALSE
  }
}

# Summary
cat("\n============================================\n")
if (success) {
  cat("All dependencies installed successfully!\n")
  cat("\nYou can now load the hetid package with:\n")
  cat("  library(devtools)\n")
  cat("  load_all()\n")
} else {
  cat("Some packages failed to install.\n")
  cat("Try installing them manually with:\n")
  cat("  install.packages('package_name')\n")
}
cat("============================================\n")

# Optional: Update all packages to latest versions
cat("\nTo update all packages to their latest versions, run:\n")
cat("  update.packages(ask = FALSE)\n")
