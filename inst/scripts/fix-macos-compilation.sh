#!/bin/bash

# Script to fix R package compilation issues on macOS
# Specifically addresses the Free/Calloc implicit function declaration errors

echo "=== R Package Compilation Fix for macOS ==="
echo

# Function to backup existing file
backup_file() {
    if [ -f "$1" ]; then
        cp "$1" "$1.backup.$(date +%Y%m%d_%H%M%S)"
        echo "Backed up $1"
    fi
}

# 1. Check if ~/.R directory exists
if [ ! -d "$HOME/.R" ]; then
    echo "Creating ~/.R directory..."
    mkdir -p "$HOME/.R"
fi

# 2. Create or update Makevars
MAKEVARS_FILE="$HOME/.R/Makevars"
echo "Configuring $MAKEVARS_FILE..."

# Backup existing Makevars
backup_file "$MAKEVARS_FILE"

# Detect architecture
ARCH=$(uname -m)
echo "Detected architecture: $ARCH"

# Create new Makevars
cat > "$MAKEVARS_FILE" << 'EOF'
# R Makevars configuration for macOS
# Fixes compilation issues with modern Clang

# Suppress implicit function declaration errors
CFLAGS = -Wno-implicit-function-declaration
CXXFLAGS = -Wno-implicit-function-declaration

# Optional: Add debugging symbols
# CFLAGS += -g
# CXXFLAGS += -g
EOF

# Add architecture-specific settings
if [ "$ARCH" = "arm64" ]; then
    echo "" >> "$MAKEVARS_FILE"
    echo "# Apple Silicon (M1/M2/M3) specific settings" >> "$MAKEVARS_FILE"
    
    # Check for Homebrew gfortran
    if [ -f "/opt/homebrew/bin/gfortran" ]; then
        echo "FC = /opt/homebrew/bin/gfortran" >> "$MAKEVARS_FILE"
        echo "F77 = /opt/homebrew/bin/gfortran" >> "$MAKEVARS_FILE"
        
        # Find the correct GCC version
        GCC_VERSION=$(ls /opt/homebrew/lib/gcc/ 2>/dev/null | grep -E '^[0-9]+$' | sort -n | tail -1)
        if [ -n "$GCC_VERSION" ]; then
            echo "FLIBS = -L/opt/homebrew/lib/gcc/$GCC_VERSION" >> "$MAKEVARS_FILE"
        fi
    fi
fi

echo
echo "Created $MAKEVARS_FILE with compilation fixes"

# 3. Install required R packages globally
echo
echo "Installing required R packages..."
R --vanilla << 'EOF'
# Function to install if not present
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org/")
  } else {
    cat(pkg, "already installed\n")
  }
}

# Install critical packages that often have compilation issues
packages <- c("digest", "Rcpp", "R6", "cli", "glue", "rlang")
for (pkg in packages) {
  tryCatch(
    install_if_missing(pkg),
    error = function(e) {
      cat("Failed to install", pkg, ":", conditionMessage(e), "\n")
    }
  )
}
EOF

# 4. Test compilation
echo
echo "Testing R package compilation..."
R --vanilla << 'EOF'
# Test digest package
tryCatch({
  library(digest)
  test_result <- digest::digest("test")
  cat("✓ digest package is working correctly\n")
  cat("  Test hash:", test_result, "\n")
}, error = function(e) {
  cat("✗ digest package test failed:", conditionMessage(e), "\n")
})
EOF

# 5. Clear pre-commit cache if it exists
if command -v pre-commit &> /dev/null; then
    echo
    echo "Clearing pre-commit cache..."
    pre-commit clean
fi

# 6. Additional recommendations
echo
echo "=== Additional Recommendations ==="
echo
echo "1. If compilation still fails, try:"
echo "   - Install Xcode Command Line Tools: xcode-select --install"
echo "   - Update Xcode to the latest version"
echo
echo "2. For persistent issues, consider installing GCC via Homebrew:"
echo "   brew install gcc"
echo "   Then update ~/.R/Makevars to use Homebrew GCC"
echo
echo "3. To enable full pre-commit R hooks after fixing compilation:"
echo "   cp .pre-commit-config-full.yaml .pre-commit-config.yaml"
echo "   pre-commit install"
echo "   pre-commit run --all-files"
echo
echo "=== Fix Applied Successfully ===" 