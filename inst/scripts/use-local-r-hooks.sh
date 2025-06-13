#!/bin/bash

# Script to switch to local R hooks that bypass renv isolation issues
# This uses your system R installation with working packages

echo "=== Switching to Local R Hooks Configuration ==="
echo

# Check if required R packages are installed
echo "Checking required R packages..."
Rscript -e "
packages <- c('styler', 'lintr', 'roxygen2', 'desc', 'spelling')
missing <- packages[!packages %in% installed.packages()[,'Package']]
if (length(missing) > 0) {
  cat('Missing packages:', paste(missing, collapse=', '), '\n')
  cat('Install them with: install.packages(c(', paste0('\"', missing, '\"', collapse=', '), '))\n')
  quit(status = 1)
} else {
  cat('All required packages are installed!\n')
}
"

if [ $? -ne 0 ]; then
  echo
  echo "Please install the missing packages before continuing."
  exit 1
fi

# Backup current configuration if it exists
if [ -f ".pre-commit-config.yaml" ]; then
  echo "Backing up current configuration..."
  cp .pre-commit-config.yaml .pre-commit-config.backup.yaml
fi

# Use the local R hooks configuration
echo "Switching to local R hooks configuration..."
cp .pre-commit-config-local-r.yaml .pre-commit-config.yaml

# Reinstall hooks
echo "Reinstalling pre-commit hooks..."
pre-commit install

echo
echo "✓ Switched to local R hooks configuration!"
echo "  - This uses your system R installation (with digest 0.6.37+)"
echo "  - No renv isolation issues"
echo "  - All R hooks should work properly now"
echo
echo "To test, run: pre-commit run --all-files"
echo "To revert, run: cp .pre-commit-config.backup.yaml .pre-commit-config.yaml" 