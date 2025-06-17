#!/bin/bash

# Setup script for R pre-commit hooks on macOS
# This helps resolve compilation issues with R packages

echo "Setting up R pre-commit hooks..."

# Check if R is installed
if ! command -v R &> /dev/null; then
    echo "R is not installed. Please install R first."
    exit 1
fi

# Check if pre-commit is installed
if ! command -v pre-commit &> /dev/null; then
    echo "pre-commit is not installed. Please install with: pip install pre-commit"
    exit 1
fi

# Install required R packages from within R
echo "Installing required R packages..."
R --vanilla << EOF
# Install packages if not already installed
packages <- c("styler", "lintr", "roxygen2", "spelling", "docopt", "desc")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages, repos = "https://cloud.r-project.org/")
}

# Check if precommit package is installed
if (!requireNamespace("precommit", quietly = TRUE)) {
  install.packages("precommit", repos = "https://cloud.r-project.org/")
}
EOF

# Clear pre-commit cache to force reinstall
echo "Clearing pre-commit cache..."
pre-commit clean

# Replace simplified config with full config
echo "Activating full pre-commit configuration..."
if [ -f ".pre-commit-config-full.yaml" ]; then
    cp .pre-commit-config-full.yaml .pre-commit-config.yaml
    echo "Full configuration activated."
else
    echo "Warning: .pre-commit-config-full.yaml not found."
fi

# Install hooks
echo "Installing pre-commit hooks..."
pre-commit install

# Run hooks on all files
echo "Testing hooks on all files..."
pre-commit run --all-files

echo "Setup complete!"
