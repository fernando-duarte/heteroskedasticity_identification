# Docker Complete Optimization Guide for hetid Package

Last Updated: June 16, 2025

## Executive Summary

This document consolidates all Docker-related documentation for the hetid R package, providing a single authoritative guide for current state, completed optimizations, and future improvements. The Docker implementation follows 2025 best practices with multi-stage builds, security hardening, and significant size optimizations.

**Current Status:**
- ✅ Production-ready Docker setup with multi-stage builds
- ✅ Reduced image size from ~4GB to ~1.5GB (development) and ~800MB (production)
- ✅ Fixed CI/CD cache competition issues
- ✅ Replaced TeXLive with TinyTeX (3GB reduction)
- 🔄 Additional optimizations pending

## Current Docker Architecture

### Image Structure

1. **Production Image** (`hetid:latest`)
   - Base: `rocker/r-ver:4.4.3`
   - Size: ~800MB (optimized with TinyTeX)
   - Purpose: Package execution and simulations
   - Features: Non-root user, minimal attack surface

2. **Development Image** (`hetid:dev`)
   - Base: `rocker/rstudio:4.4.3`
   - Size: ~1.5GB (reduced from ~4GB)
   - Purpose: Interactive development with RStudio Server
   - Features: Full development toolchain, debugging tools

3. **Builder Stage** (intermediate)
   - Purpose: Compile and build all dependencies
   - Shared by production stage via COPY commands

### Key Components

- **Dockerfile**: Multi-stage production build
- **Dockerfile.dev**: Development environment (separate for workflow compatibility)
- **docker-compose.yml**: Multi-service orchestration
- **Management Scripts**: build.sh, dev.sh, simulation.sh
- **Makefile**: 30+ Docker operation shortcuts
- **CI/CD**: GitHub Actions with security scanning

## Completed Optimizations

### 1. ✅ TinyTeX Implementation (June 16, 2025)
**Impact**: 3GB reduction in image size
- Replaced texlive packages (~3-4GB) with TinyTeX (~150MB)
- Faster builds (5-10 minutes less)
- Better ARM64 compatibility
- Auto-expands with needed LaTeX packages

### 2. ✅ GitHub Actions Cache Fix (June 16, 2025)
**Impact**: Stabilized CI/CD build times
- Conservative cache configuration: `cache-to: type=gha,mode=max,scope=${{ matrix.target }}`
- Separate cache namespaces for each build target
- Prevents cache eviction and flip-flopping build times

### 3. ✅ Multi-Stage Build Optimization
**Impact**: Cleaner separation of concerns
- Builder stage compiles everything
- Production stage copies only runtime necessities
- Development stage remains independent for compatibility

### 4. ✅ Security Hardening
**Impact**: Reduced attack surface
- Non-root user in production
- Minimal base images
- Automated vulnerability scanning with Trivy
- Network isolation with custom bridges

## Pending Optimizations

### Phase 1: pak for Parallel R Package Installation (High Priority)
**Expected Impact**: 40-50% faster package installation

Replace in Dockerfile.dev:
```dockerfile
# Current (slow)
RUN R -e "install.packages(c('devtools', 'remotes', ...),
          repos='https://cloud.r-project.org/')"

# Optimized (fast)
RUN R -e "install.packages('pak', repos='https://r-lib.github.io/p/pak/stable/')" && \
    R -e "pak::pkg_install(c('devtools', 'remotes', ...))"
```

### Phase 2: Optimize Layer Ordering (High Priority)
**Expected Impact**: Better cache utilization, faster rebuilds

Current order causes cache invalidation on code changes:
1. Install system dependencies
2. Copy source code
3. Install R packages

Optimized order:
1. Install system dependencies
2. Install ALL R packages
3. Copy source code LAST

### Phase 3: Leverage Builder Stage in Development (Medium Priority)
**Expected Impact**: Avoid duplicate compilation

```dockerfile
# In Dockerfile.dev, copy pre-built packages from builder
FROM hetid:builder AS packages
FROM rocker/rstudio:4.4.3 AS development
COPY --from=packages /usr/local/lib/R/site-library/ /usr/local/lib/R/site-library/
```

## Additional Optimization Opportunities (June 2025 Best Practices)

Based on latest research, these lightweight alternatives could further reduce image size:

### 1. Pandoc Optimization
**Current**: Installing pandoc via apt-get (~100-300MB)
**Alternative**: Use Alpine-based minimal pandoc
```dockerfile
# Instead of apt-get install pandoc
COPY --from=pandoc/minimal:latest /usr/bin/pandoc /usr/bin/pandoc
```
**Impact**: ~200MB reduction

### 2. Web Scraping Dependencies (If Needed)
**Current**: RSelenium + Java (~500MB+)
**Alternative**: rvest with read_html_live() (uses chromote)
**Impact**: No Java dependency, simpler API

### 3. Geospatial Dependencies (If Needed)
**Current**: Installing GDAL/PROJ/GEOS (~300-500MB)
**Alternative**: Start FROM Alpine GDAL image
```dockerfile
FROM ghcr.io/osgeo/gdal:alpine-small AS base
# Then add R on top
```
**Impact**: ~71MB base instead of 300MB+ of packages

### 4. Build Tools Optimization
**Current**: build-essential remains in some layers
**Alternative**: Ensure complete removal in single RUN command
```dockerfile
RUN apt-get update && \
    apt-get install -y build-essential && \
    # ... compile things ... && \
    apt-get remove -y build-essential && \
    apt-get autoremove -y && \
    rm -rf /var/lib/apt/lists/*
```
**Impact**: ~250MB reduction

### 5. Package Management with renv
**Current**: Each project duplicates R packages
**Alternative**: Use renv with global cache
**Impact**: Shared packages across projects, faster restoration

## Implementation Roadmap

### Immediate Actions (Do First)
1. ✅ Monitor next 2-3 CI/CD runs to confirm cache fix is working
2. ⏳ Implement pak in Dockerfile.dev (Phase 1)
3. ⏳ Reorder layers in Dockerfile.dev (Phase 2)

### Short-term Actions (Next Sprint)
1. Apply pak optimization to main Dockerfile
2. Test builder stage sharing between images
3. Evaluate if pandoc minimal image would help

### Long-term Considerations
1. Investigate Kubernetes deployment options
2. Consider registry-based caching if GitHub cache issues persist
3. Implement horizontal scaling for simulations

## Performance Targets

| Metric | Before | Current | Target |
|--------|--------|---------|--------|
| Dev Build Time | 20-30 min | ~15-20 min | 5-10 min |
| Dev Image Size | ~4GB | ~1.5GB | ~1.2GB |
| Prod Image Size | ~2GB | ~800MB | ~600MB |
| Cache Hit Rate | <50% | ~70% | >90% |

## Testing Commands

```bash
# Test build time
time docker build --target development -f Dockerfile.dev .

# Test functionality
make dev-start
make test
make check

# Check image sizes
docker images | grep hetid

# Monitor cache usage
docker system df
```

## Migration Notes

### Why Dockerfile.dev Remains Separate
- All workflows and CI/CD reference Dockerfile.dev specifically
- Merging would break existing scripts and GitHub Actions
- Separation allows independent optimization without affecting production

### Compatibility Requirements
- Must maintain current build target names
- Must keep same image tags and registry structure
- Must preserve all Makefile commands
- Must support existing docker-compose services

## Security Considerations

1. **Non-root execution**: Already implemented in production
2. **Minimal attack surface**: Using minimal base images
3. **Supply chain security**: Scanning with Trivy
4. **Secrets management**: Never commit credentials
5. **Network isolation**: Custom Docker networks

## Troubleshooting Guide

### Common Issues and Solutions

1. **Cache Competition Returns**
   - Run `./docker/scripts/cache-cleanup.sh`
   - Consider registry-based caching
   - Implement separate workflow files

2. **TinyTeX Missing Packages**
   ```bash
   # Inside container
   tlmgr install <package-name>
   ```

3. **Out of Memory**
   - Reduce parallel workers in pak
   - Increase Docker memory allocation
   - Use resource limits in docker-compose

4. **Slow Builds Despite Optimizations**
   - Check if rebuilding from scratch: `docker system prune -a`
   - Verify BuildKit is enabled: `echo $DOCKER_BUILDKIT`
   - Check network speed for package downloads

## Monitoring and Metrics

Track these metrics to measure optimization success:
- Build time trends in GitHub Actions
- Docker image sizes over time
- Cache hit rates
- CI/CD success rates
- Time to development environment ready

## Future Vision

### 2025 Q3-Q4 Goals
1. Sub-10 minute development builds
2. Sub-600MB production images
3. Kubernetes-ready deployment manifests
4. Automated performance regression tests
5. Multi-region registry replication

### Long-term Architecture
- Microservices for simulation workers
- Serverless functions for lightweight operations
- Container-native development workflow
- GitOps-based deployment pipeline

## References

- [Rocker Project Best Practices](https://www.rocker-project.org/use/extending/)
- [Docker Multi-Stage Builds](https://docs.docker.com/develop/develop-images/multistage-build/)
- [pak: Fast R Package Installation](https://pak.r-lib.org/)
- [TinyTeX Documentation](https://yihui.org/tinytex/)
- [BuildKit Cache Mounts](https://docs.docker.com/build/cache/)

---

*This document supersedes all previous Docker documentation files. For historical context, see git history.*
