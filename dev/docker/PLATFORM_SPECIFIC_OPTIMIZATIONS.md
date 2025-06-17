# Platform-Specific Docker Optimizations

## ARM64 Architecture: Using x86_64 Emulation for Faster Builds

### When to Use x86_64 Emulation

Use x86_64 emulation on ARM64 systems when:
- Building images with many R package dependencies
- pak's 40-50% speed improvement outweighs emulation overhead
- You're on Apple Silicon Mac (M1/M2/M3) or other ARM64 hardware

### Implementation Options

#### Option 1: Force Platform During Build (Recommended)
```bash
# Build with x86_64 emulation - pak will work!
DOCKER_BUILDKIT=1 docker build --platform linux/amd64 -t hetid:dev -f Dockerfile.dev .

# Run with x86_64 emulation
docker run --platform linux/amd64 -p 8787:8787 hetid:dev
```

#### Option 2: Modify docker-compose.yml
```yaml
services:
  hetid-dev:
    platform: linux/amd64  # Force x86_64
    build:
      context: .
      dockerfile: Dockerfile.dev
```

#### Option 3: Set Default Platform
```bash
# Set for current session
export DOCKER_DEFAULT_PLATFORM=linux/amd64

# Or add to ~/.bashrc or ~/.zshrc
echo 'export DOCKER_DEFAULT_PLATFORM=linux/amd64' >> ~/.zshrc
```

### Performance Comparison

| Build Stage | Native ARM64 | x86_64 Emulation |
|------------|--------------|------------------|
| System packages | 2 min | 3 min |
| R packages (30+) | 25-30 min | 12-15 min |
| Total build time | ~30 min | ~18 min |

**Result**: ~40% faster overall despite emulation overhead!

### Caveats

1. **Runtime Performance**: Emulated containers run ~20-30% slower
2. **Memory Usage**: Emulation uses more RAM (~1GB extra)
3. **Not for Production**: Use native ARM64 for deployed containers

### Detection Script

To check your current architecture:
```bash
# Check Docker's architecture
docker run --rm alpine uname -m

# Check if running emulated
docker run --rm --platform linux/amd64 alpine sh -c "uname -m && cat /proc/cpuinfo | grep -i 'model name' | head -1"
```

## TinyTeX Installation Optimization

### Current Status
TinyTeX installation is temporarily disabled to speed up testing:

```dockerfile
# Install TinyTeX for LaTeX support (much smaller than texlive)
# This replaces ~3GB of texlive packages with ~150MB TinyTeX
# TODO: Re-enable once ARM64 performance is improved
# RUN R -e "tinytex::install_tinytex(force = TRUE, dir = '/opt/TinyTeX', extra_packages = c('inconsolata', 'times', 'tex-gyre', 'fancyhdr', 'natbib', 'caption'))" && \
#     /opt/TinyTeX/bin/*/tlmgr path add
```

### Why TinyTeX is Disabled

1. **Installation Time**: Takes 20-30 minutes on ARM64
2. **Network Downloads**: Downloads ~100+ LaTeX packages
3. **Not Critical**: Only needed for vignette building

### When to Re-enable TinyTeX

Re-enable TinyTeX when:
- You need to build PDF vignettes
- You've optimized build times (using pak via emulation)
- You're using build caching effectively
- Production deployment requires LaTeX support

### How to Re-enable TinyTeX

1. **Uncomment in Dockerfile** (lines ~76-79):
```dockerfile
# Uncomment these lines:
RUN R -e "tinytex::install_tinytex(force = TRUE, dir = '/opt/TinyTeX', extra_packages = c('inconsolata', 'times', 'tex-gyre', 'fancyhdr', 'natbib', 'caption'))" && \
    /opt/TinyTeX/bin/*/tlmgr path add

# Also uncomment the PATH setting:
ENV PATH="/opt/TinyTeX/bin/x86_64-linux:${PATH}"
```

2. **Uncomment in production stage** (lines ~173-175):
```dockerfile
# Copy TinyTeX installation from builder
COPY --from=builder /opt/TinyTeX /opt/TinyTeX
```

3. **Fix PATH for ARM64**:
```dockerfile
# Current (x86_64 only):
ENV PATH="/opt/TinyTeX/bin/x86_64-linux:${PATH}"

# Better (auto-detect architecture):
RUN ARCH=$(uname -m | sed 's/aarch64/aarch64-linux/;s/x86_64/x86_64-linux/') && \
    echo "export PATH=/opt/TinyTeX/bin/$ARCH:\$PATH" >> /etc/profile.d/tinytex.sh
```

### TinyTeX Benefits When Enabled

- **Size**: ~150MB vs ~3GB for texlive-full
- **Speed**: Auto-installs only needed packages
- **Compatibility**: Works with R Markdown and knitr

### Alternative: Skip LaTeX in Docker

For faster builds, handle LaTeX outside Docker:
```r
# In Dockerfile, set option to skip vignette building
ENV _R_CHECK_VIGNETTES_SKIP_RUN_MAYBE_=true

# Build vignettes locally instead
devtools::build_vignettes()
```

## Optimization Decision Tree

```
Are you on ARM64 (Apple Silicon, etc.)?
├─ No → Use native build (fastest)
└─ Yes → Do you need fastest possible builds?
    ├─ No → Use native ARM64 (slower but simpler)
    └─ Yes → Do you need TinyTeX?
        ├─ No → Use x86_64 emulation without TinyTeX
        └─ Yes → Use x86_64 emulation + enable TinyTeX
```

## Makefile Shortcuts for Platform-Specific Builds

Add to Makefile:
```makefile
# Platform-specific builds
.PHONY: dev-start-x86
dev-start-x86:
	DOCKER_DEFAULT_PLATFORM=linux/amd64 docker-compose up -d hetid-dev

.PHONY: build-x86
build-x86:
	docker build --platform linux/amd64 -t hetid:dev-x86 -f Dockerfile.dev .

.PHONY: dev-detect-arch
dev-detect-arch:
	@echo "Host architecture: $$(uname -m)"
	@echo "Docker default: $$(docker run --rm alpine uname -m)"
```
