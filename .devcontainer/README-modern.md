# Modern DevContainer Setup (R 4.5.0)

This modern devcontainer configuration provides a unified development environment with both VS Code and RStudio support, using the latest R 4.5.0 and modern DevContainer features.

## Quick Start

### Option 1: VS Code (Recommended)
1. Open VS Code
2. Install the "Dev Containers" extension
3. Open Command Palette (Cmd/Ctrl+Shift+P)
4. Select "Dev Containers: Reopen in Container"
5. Choose "From 'devcontainer-modern.json' (R 4.5.0)"

### Option 2: GitHub Codespaces
1. Click "Code" → "Codespaces" → "Create codespace"
2. Wait for environment to build (~5-10 minutes first time)
3. Access RStudio at the forwarded port 8787

## What's Included

### Base Environment
- **R 4.5.0** (released April 11, 2025)
- **RStudio Server** (accessible on port 8787)
- **Radian** (enhanced R console)
- **Python 3.11** (for pre-commit hooks)
- **Quarto** (for document generation)
- **Pandoc** (for document conversion)

### R Packages Pre-installed
All hetid dependencies plus development tools:
- tidyverse ecosystem
- AER, boot, furrr, future (hetid dependencies)
- devtools, testthat, covr (development)
- lintr, styler (code quality)
- pkgdown (documentation)
- httpgd (modern plotting)

### VS Code Extensions
- R language support with LSP
- R debugger
- Jupyter notebooks
- GitHub Copilot
- Live Share (collaborative coding)
- Python support

## Key Features

### 1. Dual IDE Support
- **VS Code**: Native integration with R extension
- **RStudio Server**: Full RStudio IDE at http://localhost:8787
- **Terminal**: Both R and radian available

### 2. Modern R Features
- **httpgd**: Interactive graphics at http://localhost:8080
- **R Language Server**: IntelliSense, diagnostics, formatting
- **Radian**: Enhanced R REPL with syntax highlighting

### 3. Persistent Storage
- R packages persist across container rebuilds
- renv cache persists
- User configurations persist

### 4. Pre-configured Development
- Pre-commit hooks auto-installed
- Package dependencies auto-installed
- Git and GitHub CLI configured
- Quarto for reproducible documents

## Comparison with Traditional Setup

| Feature | Traditional Docker | Modern DevContainer |
|---------|-------------------|---------------------|
| **R Version** | 4.4.3 | 4.5.0 |
| **IDE** | RStudio only | VS Code + RStudio |
| **Setup Time** | Manual steps | Automatic |
| **Graphics** | Standard | httpgd + standard |
| **Console** | Basic R | R + radian |
| **Extensions** | None | Pre-configured |
| **Python** | Not included | Included |

## Usage Guide

### For RStudio Users
1. Open http://localhost:8787 in your browser
2. Username/password not required (disabled for development)
3. Use RStudio as normal

### For VS Code Users
1. Use the R extension for interactive development
2. Run R code with Shift+Enter
3. View plots in httpgd at http://localhost:8080
4. Use integrated terminal for R/radian console

### Terminal Options
```bash
# Standard R console
R

# Enhanced radian console (recommended)
radian

# Run R commands directly
Rscript -e "devtools::test()"
```

### Package Development Commands
```bash
# Run tests
make test

# Check package
make check

# Build documentation
make docs

# Start simulation
make simulation-quick
```

## Switching Between Setups

### Use Modern DevContainer (R 4.5.0)
```bash
# In VS Code
mv .devcontainer/devcontainer.json .devcontainer/devcontainer-old.json
mv .devcontainer/devcontainer-modern.json .devcontainer/devcontainer.json
# Then reopen in container
```

### Use Traditional Setup (R 4.4.3)
```bash
# In VS Code
mv .devcontainer/devcontainer.json .devcontainer/devcontainer-modern.json
mv .devcontainer/devcontainer-old.json .devcontainer/devcontainer.json
# Then reopen in container
```

### Local Docker Development
```bash
# Still works as before
make dev-start    # RStudio at http://localhost:8787
make dev-stop     # Stop containers
```

## ARM64/Apple Silicon Notes

The modern setup has improved ARM64 support:
- R 4.5.0 has native ARM64 optimizations
- pak package works better on ARM64
- TinyTeX installation is faster
- Better performance overall

For best performance on Apple Silicon:
```bash
# Use native ARM64 (default)
# No special configuration needed!
```

## Troubleshooting

### Port Already in Use
If port 8787 is already in use:
1. VS Code will automatically forward to a different port
2. Check the "Ports" tab in VS Code
3. Or stop conflicting service: `lsof -ti:8787 | xargs kill`

### Package Installation Issues
```R
# If a package fails to install
install.packages("package_name", repos = "https://cloud.r-project.org")

# For system dependencies
sudo apt-get update && sudo apt-get install -y libxml2-dev
```

### RStudio Not Loading
1. Check if service is running: `sudo rstudio-server status`
2. Restart if needed: `sudo rstudio-server restart`
3. Check logs: `sudo journalctl -u rstudio-server`

## Migration from R 4.4.3 to 4.5.0

Most code should work without changes. Key improvements in R 4.5.0:
- Better pipe operator (`|>`) performance
- Improved error messages
- Native ARM64 optimizations
- Enhanced memory management

To test compatibility:
```R
# Check your package with R 4.5.0
devtools::check()

# Run tests
devtools::test()
```

## Next Steps

1. Try the modern devcontainer with your preferred IDE
2. Run package tests to ensure compatibility
3. Explore new R 4.5.0 features
4. Consider updating CI/CD to test both R versions

## Resources

- [Rocker DevContainer Images](https://rocker-project.org/images/devcontainer/)
- [VS Code R Extension](https://github.com/REditorSupport/vscode-R)
- [R 4.5.0 Release Notes](https://cran.r-project.org/doc/manuals/r-release/NEWS.html)
- [DevContainer Specification](https://containers.dev/)
