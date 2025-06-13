# macOS R Package Compilation Fix Guide

## Summary of the Issue

When using pre-commit hooks with R packages on macOS, you may encounter compilation errors like:

```
error: call to undeclared function 'Free'; ISO C99 and later do not support implicit function declarations
error: call to undeclared function 'Calloc'; ISO C99 and later do not support implicit function declarations
```

### Root Causes

1. **Modern C Standard Enforcement**: Clang 16+ (included with Xcode 15+) enforces C99 standards strictly
2. **R Internal Macros**: `Free()` and `Calloc()` are R's memory allocation macros not properly included
3. **Package Bugs**: Some packages (like digest 0.6.36) had incorrect usage of these macros
4. **Isolated Environments**: Pre-commit creates isolated R environments that don't use global settings

## Solutions (in order of recommendation)

### 1. Quick Fix - Makevars Configuration

Run the provided script:
```bash
bash inst/scripts/fix-macos-compilation.sh
```

Or manually create `~/.R/Makevars`:
```makefile
CFLAGS = -Wno-implicit-function-declaration
CXXFLAGS = -Wno-implicit-function-declaration
```

### 2. Update Problematic Packages

For the digest package specifically:
```r
# Check version (need 0.6.37 or later)
packageVersion("digest")

# Update
install.packages("digest")
```

### 3. Use Simplified Pre-commit Configuration

Currently implemented in this repository - R-specific hooks are disabled until the ecosystem catches up.

### 4. Alternative Compilers

Install and use Homebrew GCC:
```bash
brew install gcc

# Add to ~/.R/Makevars
CC = /opt/homebrew/bin/gcc-14
CXX = /opt/homebrew/bin/g++-14
```

## Technical Details

### What Changed?

- **GCC 14** (May 2024) and **Clang 16** (March 2023) made implicit function declarations errors by default
- Previously these were warnings that could be ignored
- This change exposed many latent bugs in C code

### Why Does This Matter?

Implicit function declarations can lead to:
- Memory corruption
- Security vulnerabilities
- Crashes at runtime
- Incorrect function calls

### The Pre-commit Challenge

Pre-commit creates isolated environments using `renv`, which:
- Downloads and compiles packages from source
- Doesn't use your global R package installations
- Doesn't use your global `~/.R/Makevars` settings
- May download older package versions

## Long-term Solutions

1. **Package Maintainers**: Update C code to properly include headers and use correct syntax
2. **Pre-commit**: Update default package versions in lock files
3. **R Core**: Consider providing compatibility headers or clearer migration guides

## Resources

- [GCC 14 Porting Guide](https://gcc.gnu.org/gcc-14/porting_to.html)
- [Modern C Porting - Gentoo Wiki](https://wiki.gentoo.org/wiki/Modern_C_porting)
- [R Installation Admin Guide - macOS](https://cran.r-project.org/doc/manuals/r-release/R-admin.html#macOS)
- [digest package GitHub](https://github.com/eddelbuettel/digest)

## Related Files in This Repository

- `inst/scripts/fix-macos-compilation.sh` - Automated fix script
- `inst/scripts/fix-precommit-digest.R` - Digest package updater
- `inst/scripts/setup-precommit-r.sh` - Pre-commit setup with R packages
- `inst/setup/Makevars-example` - Example Makevars configuration
- `.pre-commit-config.yaml` - Current (simplified) configuration
- `.pre-commit-config-full.yaml` - Full configuration (with R hooks)
