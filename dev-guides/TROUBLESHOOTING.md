## Troubleshooting

### Common Issues

1. **Convergence problems**: Increase sample size or adjust parameters
2. **Memory issues**: Reduce number of simulations or use parallel processing
3. **Missing dependencies**: Install suggested packages for full functionality

### Getting Help

- Check the vignettes: `browseVignettes("hetid")`
- View function documentation: `?function_name`
- Visit the package website: https://fernando-duarte.github.io/heteroskedasticity_identification/
- Report issues: https://github.com/fernando-duarte/heteroskedasticity_identification/issues

## macOS Development Guide

### Known Issues

When developing R packages on modern macOS systems (especially with Xcode 15+ / Clang 16+), you may encounter compilation errors like:

```
error: call to undeclared function 'Free'; ISO C99 and later do not support implicit function declarations
error: call to undeclared function 'Calloc'; ISO C99 and later do not support implicit function declarations
```

This happens because modern compilers (Clang 16+, GCC 14+) enforce stricter C standards, and some R packages have bugs or use outdated syntax.

### Quick Fix

Create `~/.R/Makevars`:
```bash
mkdir -p ~/.R
cat > ~/.R/Makevars << 'EOF'
CFLAGS = -Wno-implicit-function-declaration
CXXFLAGS = -Wno-implicit-function-declaration
EOF
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
- **"Can't find gfortran"**: M1/M2: `brew install gcc`, Intel: Download from GCC website
- **digest package fails**: Update to 0.6.37+ with `install.packages("digest")`

**Verification:**
```bash
# Test compilation
R -e "install.packages('digest', type = 'source')"

# Test pre-commit
pre-commit run --all-files
```
