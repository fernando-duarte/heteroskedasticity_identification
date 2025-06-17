# Pre-commit Setup for R Packages

This document explains how to set up and use pre-commit hooks for the hetid R package.

## Current Configuration

Due to compilation issues with some R packages on macOS, we're using a simplified configuration that includes:

- **Standard hooks**: trailing whitespace removal, end-of-file fixes, YAML validation, merge conflict checks, etc.
- **R-specific hooks**: Currently commented out but available in `.pre-commit-config-full.yaml`

## Basic Setup

1. Install pre-commit (requires Python):
   ```bash
   pip install pre-commit
   ```

2. Install the git hooks:
   ```bash
   pre-commit install
   ```

3. Run hooks on all files:
   ```bash
   pre-commit run --all-files
   ```

## Enabling R-specific Hooks

If you want to enable the full suite of R-specific hooks (styler, lintr, roxygen2, etc.), you have two options:

### Option 1: Use the Setup Script (Recommended)

```bash
bash inst/scripts/setup-precommit-r.sh
```

This script will:
- Install required R packages
- Clear the pre-commit cache
- Activate the full configuration
- Test the hooks

### Option 2: Manual Setup

1. Install required R packages:
   ```r
   install.packages(c("styler", "lintr", "roxygen2", "spelling", "docopt", "desc", "precommit"))
   ```

2. Copy the full configuration:
   ```bash
   cp .pre-commit-config-full.yaml .pre-commit-config.yaml
   ```

3. Clear cache and reinstall:
   ```bash
   pre-commit clean
   pre-commit install
   ```

## Troubleshooting

### Update: Solution for digest Package Error (January 2025)

The compilation error with the `digest` package (version 0.6.36) has been fixed in version 0.6.37 (released August 2024). The error was:
```
error: expected expression
   42 |   ctx = (aes_context*)Calloc(sizeof(*ctx), char);
      |                                            ^
```

**Solution**: Ensure you have digest version 0.6.37 or later:
```r
# Check current version
packageVersion("digest")

# Update if needed
install.packages("digest")

# Or run the provided script
Rscript inst/scripts/fix-precommit-digest.R
```

However, pre-commit creates its own isolated environment, so the global fix may not apply. Current best practice is to use the simplified configuration without R-specific hooks until the pre-commit R environment is updated.

## Additional Troubleshooting

### macOS Compilation Issues

If you encounter errors like:
```
error: call to undeclared function 'Free'; ISO C99 and later do not support implicit function declarations
error: call to undeclared function 'Calloc'; ISO C99 and later do not support implicit function declarations
```

This is a known issue with R packages on macOS using modern Clang compilers (16+). The issue stems from stricter C99 standard enforcement.

#### Quick Fix

Run the provided fix script:
```bash
bash inst/scripts/fix-macos-compilation.sh
```

This script will:
- Create/update `~/.R/Makevars` with the necessary compiler flags
- Install commonly problematic R packages
- Test the compilation fix
- Clear pre-commit cache

#### Manual Solutions

1. **Create ~/.R/Makevars** (Recommended):
   ```bash
   mkdir -p ~/.R
   echo "CFLAGS = -Wno-implicit-function-declaration" >> ~/.R/Makevars
   echo "CXXFLAGS = -Wno-implicit-function-declaration" >> ~/.R/Makevars
   ```

2. **Install Xcode Command Line Tools**:
   ```bash
   xcode-select --install
   ```

3. **Use Homebrew GCC** (Alternative):
   ```bash
   brew install gcc
   # Then update ~/.R/Makevars to use:
   # CC = /opt/homebrew/bin/gcc-13
   # CXX = /opt/homebrew/bin/g++-13
   ```

4. **Use the simplified configuration** (current default) - no R-specific hooks

### pre-commit.ci

The configuration includes pre-commit.ci support, which will automatically:
- Run hooks on all PRs
- Auto-fix issues where possible
- Comment on PRs with any issues found

To enable, visit https://pre-commit.ci and add the repository.

## Available Hooks

### Currently Active
- `trailing-whitespace`: Remove trailing whitespace
- `end-of-file-fixer`: Ensure files end with newline
- `check-case-conflict`: Check for case conflicts
- `check-merge-conflict`: Check for merge conflicts
- `check-yaml`: Validate YAML files
- `check-added-large-files`: Prevent large files (>500KB)
- `file-contents-sorter`: Sort .Rbuildignore
- `forbid-to-commit`: Prevent committing .Rhistory, .RData files

### Available in Full Configuration
- `style-files`: Format R code using styler
- `roxygenize`: Update roxygen documentation
- `use-tidy-description`: Format DESCRIPTION file
- `spell-check`: Check spelling
- `lintr`: R code quality checks
- `parsable-R`: Ensure R code is parsable
- `no-browser-statement`: No browser() calls
- `no-debug-statement`: No debug() calls
- `deps-in-desc`: Check dependencies are declared
