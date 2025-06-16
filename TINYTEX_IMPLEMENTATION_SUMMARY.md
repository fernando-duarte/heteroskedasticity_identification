# TinyTeX Implementation Summary

## What Was Changed

### 1. Replaced Heavy TeXLive Packages with TinyTeX

**Removed from Dockerfiles:**
```dockerfile
# These lines were removed (saving ~3-4GB):
texlive-latex-base \
texlive-fonts-recommended \
texlive-latex-extra \
texlive-fonts-extra \
```

**Added instead:**
- `tinytex` R package
- TinyTeX installation (~150MB)
- Common LaTeX packages for R manuals

### 2. Files Modified

#### Dockerfile (main):
- Lines 54-58: Removed texlive packages
- Line 64: Added tinytex to R packages
- Lines 67-73: Added TinyTeX installation and PATH
- Line 173: Copy TinyTeX to production stage
- Line 190: Added TinyTeX to PATH in production

#### Dockerfile.dev:
- Lines 52-54: Removed texlive packages
- Line 62: Added tinytex to R packages
- Lines 65-70: Added TinyTeX installation and PATH

### 3. Benefits

- **Image size reduction**: ~3GB smaller
- **Faster builds**: Less to download and cache
- **Better ARM64 support**: TinyTeX works cross-platform
- **Cache friendly**: Much smaller cache footprint
- **Auto-expanding**: TinyTeX downloads missing packages on demand

### 4. How It Works

TinyTeX is installed in the builder stage:
```r
tinytex::install_tinytex(
  force = TRUE,
  dir = '/opt/TinyTeX',
  extra_packages = c('inconsolata', 'times', 'tex-gyre',
                     'fancyhdr', 'natbib', 'caption')
)
```

The installation is then:
- Used directly in builder for R CMD check
- Copied to production stage for runtime
- Available in development for package building

### 5. Testing

To verify TinyTeX is working:
```bash
# In container
which pdflatex  # Should show /opt/TinyTeX/bin/x86_64-linux/pdflatex
R CMD build .   # Should generate PDF manual
R CMD check hetid_*.tar.gz  # Should pass PDF checks
```

### 6. Expected Impact

- **Docker build time**: Reduced by 5-10 minutes
- **Image size**: Reduced by ~3GB
- **Cache usage**: Significantly reduced
- **ARM64 builds**: Should now work without issues
