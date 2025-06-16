# Docker Build Time Optimization Plan for hetid Package

## Executive Summary

The development Docker image currently takes 20-30 minutes to build, while the production image takes only 2-3 minutes. This document provides a validated plan based on June 2025 Docker best practices to reduce development build time to 5-10 minutes while maintaining full compatibility with existing workflows.

## Current Situation

### Build Time Comparison
- **Builder stage**: ~10-15 minutes (builds everything from source)
- **Production stage**: ~2-3 minutes (just copies from builder)
- **Development stage**: ~20-30 minutes (rebuilds everything from scratch)

### The Problem
The development image takes the longest because it:
1. Doesn't reuse any work from the builder stage
2. Installs all dependencies twice
3. Uses inefficient installation methods
4. Doesn't leverage modern Docker features like BuildKit cache mounts

## Validation Against June 2025 Best Practices

### ✅ Confirmed Best Practices
1. **Multi-stage builds with COPY --from**: Industry standard for efficient builds
2. **BuildKit enabled by default**: All scripts already use `DOCKER_BUILDKIT=1`
3. **GitHub Actions caching**: CI/CD uses `type=gha` cache mounts
4. **Non-root users**: Production already implements this security practice
5. **Health checks**: Properly implemented in production containers

### 🔧 Optimization Opportunities
1. **pak for parallel R package installation**: Faster than traditional methods
2. **BuildKit cache mounts**: Not yet used for R package caching
3. **Layer ordering**: Can be improved for better cache utilization
4. **Rocker best practices**: Leverage rocker project patterns

## Quick Wins (Implement First)

These changes can be made immediately with minimal risk:

### 1. Update Dockerfile.dev Header
```dockerfile
# syntax=docker/dockerfile:1.7
```

### 2. Replace install2.r with pak
```dockerfile
# OLD (line 225-234 in Dockerfile.dev)
RUN install2.r --error --skipinstalled \
    devtools testthat roxygen2 ...

# NEW - 40-50% faster
RUN R -e "install.packages('pak', repos='https://r-lib.github.io/p/pak/stable/')" && \
    R -e "pak::pkg_install(c('devtools', 'testthat', 'roxygen2', ...))"
```

### 3. Fix Cache Invalidation in Dockerfile.dev
Move package installation BEFORE copying source:
```dockerfile
# Install ALL R packages first (before line 237)
RUN R -e "pak::pkg_install(...)"

# THEN copy source (line 237)
COPY --chown=rstudio:rstudio . .
```

These three changes alone should reduce build time by 40-60%.

## Understanding Docker Multi-Stage Builds

### What Are Multi-Stage Builds?
Multi-stage builds allow you to use multiple FROM statements in your Dockerfile. Each FROM starts a new stage that can copy artifacts from previous stages.

```dockerfile
# Stage 1: Builder
FROM rocker/r-ver:4.4.3 AS builder
# ... build everything ...

# Stage 2: Production (copies from builder)
FROM rocker/r-ver:4.4.3 AS production
COPY --from=builder /usr/local/lib/R/site-library/ /usr/local/lib/R/site-library/

# Stage 3: Development (currently starts from scratch!)
FROM rocker/rstudio:4.4.3 AS development
# ... reinstalls everything ...
```

### Why This Matters
- Each stage that starts with FROM begins completely fresh
- The development stage currently ignores all the work done in builder
- This wastes time and computational resources

## Detailed Analysis of Current Issues

### Issue 1: No Reuse of Builder Work

**Current State:**
```dockerfile
# Development stage starts fresh
FROM rocker/rstudio:4.4.3 AS development

# Then reinstalls ALL system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    # ... 20+ more packages ...
```

**Problem:** These exact same packages were already installed in the builder stage!

### Issue 2: Duplicate R Package Installation

**Current State:**
```dockerfile
# First installation using install2.r
RUN install2.r --error --skipinstalled \
    devtools \
    testthat \
    roxygen2 \
    # ... more packages ...

# Then AGAIN using devtools
RUN R -e "devtools::install(dependencies = TRUE)"
```

**Problem:** Many packages get installed twice, and `devtools::install()` re-analyzes all dependencies.

### Issue 3: Separation of Development Dockerfile

**Current State:**
- `Dockerfile` has a development stage (lines 188-251) that's not used
- `Dockerfile.dev` is the actual development file used by all workflows
- CI/CD, docker-compose, and scripts all reference `Dockerfile.dev`

**Analysis:** This is actually a valid pattern. Keeping development separate:
- Allows independent optimization without affecting production
- Simplifies CI/CD matrix builds
- Matches the current working workflow that shouldn't be broken

### Issue 4: Inefficient Layering

**Current State:**
- Package source is copied late in the process
- Any change to source code invalidates all subsequent cache layers

**Problem:** Docker can't reuse cached layers efficiently.

## Step-by-Step Optimization Plan

### Phase 1: Optimize Package Installation with pak

**Goal:** Replace slow sequential installs with fast parallel installation.

**Steps:**
1. Install pak from r-lib development repository (stable version)
2. Replace `install.packages()` and `install2.r` with pak
3. Configure pak for parallel downloads and builds
4. Test that all packages install correctly
5. Measure time improvement

**Implementation for Dockerfile.dev:**
```dockerfile
# Install pak for fast parallel package installation
RUN R -e "install.packages('pak', repos = 'https://r-lib.github.io/p/pak/stable/')"

# Install all packages in parallel with pak
RUN R -e "pak::pkg_install(c(
    'devtools', 'remotes', 'usethis',
    'testthat', 'covr', 'lintr', 'styler',
    'roxygen2', 'pkgdown', 'rmarkdown', 'knitr',
    'profvis', 'bench', 'here', 'fs', 'glue', 'cli'
))"
```

### Phase 2: Leverage Builder Stage in Development

**Goal:** Reuse compiled packages from builder stage to avoid duplication.

**Steps:**
1. Modify Dockerfile.dev to start FROM the builder stage
2. Copy pre-built R packages from builder
3. Only install additional development-specific packages
4. Test that development environment works correctly

**Implementation for Dockerfile.dev:**
```dockerfile
# Build a temporary builder stage first
FROM rocker/r-ver:4.4.3 AS r-packages
# ... (copy system deps and R package installation from main Dockerfile)

# Then start development from rocker/rstudio but copy packages
FROM rocker/rstudio:4.4.3 AS development
# Copy pre-built packages from builder
COPY --from=r-packages /usr/local/lib/R/site-library/ /usr/local/lib/R/site-library/

# Only install additional dev-specific packages
RUN R -e "pak::pkg_install(c('profvis', 'bench', 'here'))"
```

### Phase 3: Implement BuildKit Cache Mounts

**Goal:** Use BuildKit cache mounts for persistent package caches.

**Steps:**
1. Configure R to use consistent cache directories
2. Add cache mounts for R package downloads and builds
3. Test cache effectiveness across builds

**Implementation:**
```dockerfile
# Configure R package cache location
ENV R_PKG_CACHE_DIR=/cache/R/pkg

# Use cache mount for package installation
RUN --mount=type=cache,target=/cache/R/pkg,sharing=locked \
    R -e "options(pkg.cache_dir = '/cache/R/pkg'); \
          pak::pkg_install(c('devtools', 'testthat', ...))"
```

**Note:** R package caching is challenging with BuildKit. Alternative approach using Docker volumes may be more reliable.

### Phase 4: Optimize Caching Strategy

**Goal:** Structure Dockerfile for maximum cache reuse.

**Key Principles:**
1. **Order by change frequency**: Put things that change rarely at the top
2. **Copy only what's needed**: Use specific COPY commands instead of copying everything
3. **Separate concerns**: System deps → R packages → Source code

**Implementation:**
```dockerfile
# 1. Copy only package metadata first
COPY DESCRIPTION NAMESPACE ./

# 2. Install dependencies based on DESCRIPTION
RUN R -e "remotes::install_deps('.', dependencies = TRUE)"

# 3. Copy source code last
COPY . .
```

### Phase 5: Create Shared Base Image

**Goal:** Create a shared base image for both production and development.

**Steps:**
1. Create hetid-base image with all common dependencies
2. Tag and potentially push to registry for reuse
3. Update both Dockerfile and Dockerfile.dev to use base
4. Configure CI/CD to build base image first

**Implementation:**
```dockerfile
# Dockerfile.base
FROM rocker/r-ver:4.4.3 AS hetid-base
# Install all common system deps and R packages
# ...
# Tag as hetid:base

# In other Dockerfiles
FROM hetid:base AS builder
# Just add build-specific items

FROM hetid:base AS development
# Just add dev-specific items
```

## Expected Results

### Before Optimization
- Development build: 20-30 minutes
- Rebuilds after code change: 15-20 minutes
- Docker image size: ~4GB

### After Optimization
- Development build: 5-10 minutes
- Rebuilds after code change: 1-2 minutes
- Docker image size: ~3GB

## Implementation Checklist

- [ ] Back up current Dockerfiles
- [ ] Create feature branch: `feature/optimize-docker-builds`
- [ ] Phase 1: Implement pak for parallel installation
  - [ ] Update Dockerfile.dev to use pak
  - [ ] Test all packages install correctly
  - [ ] Measure time improvement
  - [ ] Update main Dockerfile builder stage
- [ ] Phase 2: Leverage builder stage packages
  - [ ] Create intermediate build stage in Dockerfile.dev
  - [ ] Copy packages from builder
  - [ ] Test development environment functionality
- [ ] Phase 3: Implement BuildKit optimizations
  - [ ] Add cache mount directives where beneficial
  - [ ] Test cache hit rates
  - [ ] Document any R-specific caching issues
- [ ] Phase 4: Optimize layer ordering
  - [ ] Reorder instructions by change frequency
  - [ ] Separate COPY commands appropriately
  - [ ] Measure rebuild times after code changes
- [ ] Phase 5: Create shared base image (optional)
  - [ ] Evaluate if beneficial given current structure
  - [ ] Create Dockerfile.base if proceeding
  - [ ] Update CI/CD workflows
- [ ] Compatibility testing
  - [ ] Verify all Makefile commands work
  - [ ] Test docker/scripts/build.sh
  - [ ] Test docker/scripts/dev.sh
  - [ ] Verify CI/CD workflows pass
- [ ] Update documentation
  - [ ] Update inline Dockerfile comments
  - [ ] Document pak installation process
  - [ ] Note any breaking changes (should be none)
- [ ] Performance validation
  - [ ] Document before/after build times
  - [ ] Test with clean Docker cache
  - [ ] Test incremental builds
- [ ] Create pull request with benchmarks

## Testing the Optimizations

### Measure Build Time
```bash
# Before optimization
time docker build --target development -t hetid-dev:before .

# After each phase
time docker build --target development -t hetid-dev:after-phase1 .
```

### Test Cache Effectiveness
```bash
# Make a small change to R code
echo "# test" >> R/utils.R

# Time rebuild
time docker build --target development -t hetid-dev:rebuild .
```

### Verify Functionality
```bash
# Start development container
docker run -d -p 8787:8787 hetid-dev:after

# Check RStudio
open http://localhost:8787

# Run tests
docker exec -it <container-id> R -e "devtools::test()"
```

## Critical Compatibility Requirements

### Must Maintain:
1. **Dockerfile.dev remains separate**: Used by all workflows and CI/CD
2. **Build targets unchanged**: builder, production, development
3. **All Makefile commands must work**: Especially `make dev-start`, `make build-dev`
4. **CI/CD workflow compatibility**: Docker.yml expects specific structure
5. **docker-compose services**: hetid-dev must continue using Dockerfile.dev

### Do NOT:
1. **Delete or merge Dockerfile.dev**: It's referenced everywhere
2. **Change image names or tags**: Would break scripts and workflows
3. **Modify build script interfaces**: Parameters must remain the same
4. **Break health checks**: Production containers rely on them

## Common Pitfalls to Avoid

1. **Don't remove necessary dependencies**: Some packages need specific system libraries
2. **Test on clean machine**: What works on your machine might fail on CI/CD
3. **Keep production separate**: Don't let dev optimizations affect production builds
4. **Document changes**: Update README and comments for future developers
5. **R package caching complexity**: BuildKit cache mounts don't work well with R
6. **pak stability**: Use stable channel, not development version
7. **Layer invalidation**: Be careful with COPY order to preserve cache

## Additional Resources

- [Docker Best Practices for R](https://www.rocker-project.org/use/extending/)
- [Docker Multi-Stage Builds](https://docs.docker.com/develop/develop-images/multistage-build/)
- [Optimizing Docker Images](https://docs.docker.com/develop/dev-best-practices/)
- [pak: A Fast R Package Installer](https://pak.r-lib.org/)

## Recommended Implementation Priority

Based on June 2025 best practices and codebase analysis:

### 🟢 High Priority (Big Impact, Low Risk)
1. **pak installation**: 40-50% time reduction, proven stable
2. **Layer reordering**: 20-30% cache improvement, no breaking changes
3. **COPY optimization**: Faster rebuilds after code changes

### 🟡 Medium Priority (Good Impact, Some Complexity)
1. **Leverage builder packages**: Avoid duplication, requires testing
2. **BuildKit syntax update**: Use `# syntax=docker/dockerfile:1.7`
3. **Remove unused development stage from main Dockerfile**

### 🔴 Lower Priority (Marginal Gains, Higher Risk)
1. **BuildKit cache mounts for R**: Limited R support
2. **Shared base image**: Adds complexity to CI/CD
3. **Multi-platform builds for dev**: Not needed for local development

## Questions or Issues?

If you encounter problems during implementation:
1. Check Docker build logs carefully
2. Test each phase independently
3. Use `docker run -it <image> /bin/bash` to debug
4. Ask for help in the team chat with specific error messages

Remember: The goal is faster builds without breaking functionality. Take it step by step!
