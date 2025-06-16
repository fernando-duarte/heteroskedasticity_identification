# Docker Optimization - Next Steps

## Current Status

1. **Cache Fix v2 Deployed**: Conservative approach to fix build failures
   - Using `cache-from: type=gha` (no scope for compatibility)
   - Using `cache-to: type=gha,mode=max,scope=${{ matrix.target }}`
   - Should prevent cache competition while maintaining compatibility

2. **Waiting for Build Confirmation**: Check GitHub Actions to ensure builds are working

## Once Builds Are Stable

### Phase 1: Implement pak for Parallel R Package Installation

**File: Dockerfile.dev (lines 58-64)**

Current slow installation:
```dockerfile
RUN R -e "install.packages(c(
    'devtools', 'remotes', 'usethis',
    'testthat', 'covr', 'lintr', 'styler',
    'roxygen2', 'pkgdown', 'rmarkdown', 'knitr',
    'profvis', 'bench', 'here', 'fs', 'glue', 'cli'
), repos='https://cloud.r-project.org/')"
```

Replace with pak (40-50% faster):
```dockerfile
# Install pak for fast parallel package installation
RUN R -e "install.packages('pak', repos='https://r-lib.github.io/p/pak/stable/')"

# Install all packages in parallel
RUN R -e "pak::pkg_install(c(
    'devtools', 'remotes', 'usethis',
    'testthat', 'covr', 'lintr', 'styler',
    'roxygen2', 'pkgdown', 'rmarkdown', 'knitr',
    'profvis', 'bench', 'here', 'fs', 'glue', 'cli'
))"
```

### Phase 2: Optimize Layer Ordering in Dockerfile.dev

Move package installation BEFORE copying source to improve cache hits:

**Current order (lines 67-77):**
1. Set working directory
2. Configure R
3. Create project directory
4. THEN install packages

**Optimized order:**
1. Install ALL R packages first
2. Configure R
3. Set working directory
4. Copy source LAST

This prevents cache invalidation when source code changes.

### Phase 3: Apply Same Optimizations to Main Dockerfile

Update the builder stage (lines 62-93) to use pak for faster builds.

### Phase 4: Consider Further Optimizations

1. **Reduce mode=max back to mode=min** once we confirm builds are stable
2. **Add cache warming job** to pre-build base layers daily
3. **Implement conditional builds** to skip unchanged components

## Monitoring Success

- Watch build times in GitHub Actions
- Target: Reduce from 20-30 minutes to 5-10 minutes
- Ensure cache hit rates improve to >90%

## Quick Test Commands

After implementing each phase:
```bash
# Test development build locally
make build-dev

# Check build time
time docker build --target development -f Dockerfile.dev .

# Verify functionality
make dev-start
make test
```
