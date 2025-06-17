# Docker LaTeX Fix Summary

## Problem
The docker-test workflow was failing with LaTeX errors when generating PDF manuals during R CMD check:
- Error: `\textfont 0 is undefined` - indicating missing Computer Modern fonts
- TinyTeX installation via tlmgr was failing with package errors

## Root Cause
1. TinyTeX package installation was failing because:
   - Some packages don't exist as separate entities (e.g., `amssymb` is part of `amsfonts`)
   - `fontenc` and `inputenc` are LaTeX packages, not TeX Live packages
   - The tlmgr command was returning exit code 1

2. Even when TinyTeX installed successfully, it was missing essential fonts for PDF generation

## Solution
Switched from TinyTeX to TeX Live system packages:

### Before (TinyTeX approach):
```dockerfile
RUN R -e "tinytex::install_tinytex(force = TRUE, dir = '/opt/TinyTeX')" && \
    /opt/TinyTeX/bin/*/tlmgr install [packages...] && \
    /opt/TinyTeX/bin/*/tlmgr update --self --all
```

### After (TeX Live approach):
```dockerfile
RUN --mount=type=cache,target=/var/cache/apt \
    --mount=type=cache,target=/var/lib/apt \
    apt-get update && apt-get install -y --no-install-recommends \
    texlive-latex-base \
    texlive-latex-recommended \
    texlive-fonts-recommended \
    texlive-fonts-extra \
    texinfo \
    && rm -rf /var/lib/apt/lists/*
```

## Benefits of the Solution
1. **Reliability**: All required fonts and packages are included
2. **Simplicity**: No complex tlmgr commands or package management
3. **Compatibility**: Works consistently across different architectures
4. **Cache-friendly**: Uses Docker cache mounts for faster rebuilds

## Trade-offs
- **Image size**: TeX Live packages add ~1GB to the Docker image (vs ~150MB for TinyTeX)
- **Build time**: Initial installation takes longer, but subsequent builds use cache

## Result
✅ All workflows now pass successfully
✅ PDF manual generation works correctly
✅ R CMD check --as-cran completes without errors

## Commits
1. `8ffc396` - Added Computer Modern fonts to TinyTeX (failed attempt)
2. `5f7c6b9` - Used tlmgr directly to install packages (failed attempt)
3. `9b8c15b` - Switched to TeX Live packages (successful fix)

## Workflow Run
- Successful run: https://github.com/fernando-duarte/heteroskedasticity_identification/actions/runs/15709569077
- docker-test job completed in 4m28s with all checks passing
