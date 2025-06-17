## macOS Development Guide

### Known Issues

When developing R packages on modern macOS systems (especially with Xcode 15+ / Clang 16+), you may encounter compilation errors like:

```
error: call to undeclared function 'Free'; ISO C99 and later do not support implicit function declarations
error: call to undeclared function 'Calloc'; ISO C99 and later do not support implicit function declarations
```

This happens because modern compilers (Clang 16+, GCC 14+) enforce stricter C standards, and some R packages have bugs or use outdated syntax.

### Quick Fix

Run our automated fix script:
```bash
bash inst/scripts/fix-macos-compilation.sh
```

Or manually create `~/.R/Makevars`:
```bash
mkdir -p ~/.R
cat > ~/.R/Makevars << 'EOF'
CFLAGS = -Wno-implicit-function-declaration
CXXFLAGS = -Wno-implicit-function-declaration
EOF
```

### Pre-commit Hooks on macOS

Pre-commit creates isolated R environments that may not respect your system configuration. We provide three approaches:

1. **Simplified Configuration** (Default) - Only non-R hooks (file formatting, YAML validation)
2. **Local R Hooks** (Recommended for macOS) - Uses your system R installation:
   ```bash
   bash inst/scripts/use-local-r-hooks.sh
   ```
3. **Full Configuration** - Standard R hooks (may fail on macOS):
   ```bash
   bash inst/scripts/setup-precommit-r.sh
   ```

### Additional Solutions

#### Update Packages
```r
# Ensure digest package is 0.6.37+
packageVersion("digest")
install.packages("digest")
```

#### Alternative Compiler
```bash
# Install Homebrew GCC
brew install gcc

# Add to ~/.R/Makevars
echo "CC = /opt/homebrew/bin/gcc-14" >> ~/.R/Makevars
echo "CXX = /opt/homebrew/bin/g++-14" >> ~/.R/Makevars
```

### Troubleshooting

**Common Issues:**
- **"Calloc/Free undeclared"**: Run `bash inst/scripts/fix-macos-compilation.sh`
- **Pre-commit R hooks fail**: Use local R hooks or simplified configuration
- **"Can't find gfortran"**: M1/M2: `brew install gcc`, Intel: Download from GCC website
- **digest package fails**: Update to 0.6.37+ with `install.packages("digest")`

**Verification:**
```bash
# Test compilation
R -e "install.packages('digest', type = 'source')"

# Test pre-commit
pre-commit run --all-files
```

### Related Scripts

- `inst/scripts/fix-macos-compilation.sh` - Automated fix for compilation issues
- `inst/scripts/use-local-r-hooks.sh` - Switch to local R hooks
- `inst/scripts/setup-precommit-r.sh` - Full pre-commit setup
- `dev/fix-precommit-digest.R` - Digest package updater

For more detailed information, see `dev/` directory documentation.
