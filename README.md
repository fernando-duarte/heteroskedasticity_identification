# hetid: Heteroskedasticity Identification

<!-- badges: start -->
[![R-CMD-check](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/R-CMD-check.yml)
[![Docker Build and Test](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/docker.yml/badge.svg)](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/docker.yml)
[![CodeQL Security Analysis](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/codeql.yml/badge.svg)](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/codeql.yml)
[![pkgdown](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/pkgdown.yml/badge.svg)](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/pkgdown.yml)
[![pre-commit](https://img.shields.io/badge/pre--commit-enabled-brightgreen?logo=pre-commit&logoColor=white)](https://github.com/pre-commit/pre-commit)
<!-- badges: end -->

## Overview

The `hetid` package implements the identification through heteroskedasticity method of Lewbel (2012) for time-series models with endogenous regressors. It provides tools for estimation and inference when traditional instruments are not available.

## Installation

You can install the development version of hetid from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("fernando-duarte/heteroskedasticity_identification")
```

### Note on Vignettes

If you're installing from a path containing spaces (e.g., "Dropbox (Personal)"), vignettes may not build correctly during installation. To view the vignette after installation:

```r
# Build vignettes manually if needed
devtools::build_vignettes()

# View available vignettes
browseVignettes("hetid")
```

### Optional Dependencies

For enhanced functionality, you may want to install these optional packages:

```r
# For enhanced table formatting in analysis output
install.packages("knitr")
```

The package will work without these dependencies, but installing them provides:
- **knitr**: Nicely formatted tables in analysis functions (when `verbose = TRUE`)

## Project Structure

```
heteroskedasticity_identification/
├── R/                    # R functions implementing the methodology
│   ├── messager.R       # Utility functions for messaging
│   └── stopper.R        # Utility functions for error handling
├── lewbel2012/          # Implementation and analysis of Lewbel (2012)
│   ├── lewbel2012.tex   # LaTeX source file
│   ├── lewbel2012.pdf   # Compiled PDF document
│   └── ...              # Supporting LaTeX files
├── tests/               # Unit tests
│   └── testthat/        # testthat test files
├── vignettes/           # Package vignettes
│   └── getting-started.Rmd
├── man/                 # Documentation (generated)
├── docs/                # pkgdown site (generated)
├── .github/             # GitHub Actions workflows
│   ├── workflows/
│   │   ├── R-CMD-check.yml   # R package CI/CD
│   │   ├── docker.yml        # Docker build and security
│   │   ├── codeql.yml        # Security analysis
│   │   └── pkgdown.yml       # Documentation site
│   └── dependabot.yml       # Automated dependency updates
└── inst/                # Installed files
    └── hooks/           # Pre-commit hook scripts
```

## Development

This package uses modern development practices to ensure code quality and consistency.

### Development Environment Setup

Choose your preferred development environment:

#### Option 1: GitHub Codespaces (Recommended for Quick Start) ☁️

**Zero setup required** - fully configured cloud development environment:

1. **Launch Codespace**:
   - Click "Code" → "Codespaces" → "Create codespace on main"
   - Wait 2-3 minutes for automatic setup

2. **Start Developing**:
   - RStudio Server auto-opens on port 8787
   - Package pre-loaded and ready to use
   - All tools and dependencies included

**Features**: RStudio Server, VS Code, R 4.5.0, all dependencies, Git integration, Docker support

#### Option 2: Local Docker Development 🐳

**Consistent environment** across all machines:

1. **Prerequisites**: Docker Engine 28.0+ and docker-compose

2. **Quick Start**:
   ```bash
   git clone https://github.com/fernando-duarte/heteroskedasticity_identification.git
   cd heteroskedasticity_identification
   make dev-start  # Launches RStudio at http://localhost:8787
   ```

See [DOCKER.md](DOCKER.md) for comprehensive Docker documentation.

#### Option 3: Local R Installation 💻

**Traditional setup** for local development:

1. **Clone the repository**:
   ```bash
   git clone https://github.com/fernando-duarte/heteroskedasticity_identification.git
   cd heteroskedasticity_identification
   ```

2. **Install development dependencies**:
   ```r
   # Install devtools if not already installed
   install.packages("devtools")

   # Install package dependencies
   devtools::install_deps(dependencies = TRUE)
   ```

3. **Install pre-commit hooks** (requires Python):
   ```bash
   pip install pre-commit
   pre-commit install
   ```

#### Additional Tools

- **(Optional) For Cursor AI users**: The repository includes a `.cursorignore` file that prevents the AI from accessing sensitive files and improves performance by excluding large generated files.

### Code Quality Tools

The project uses several tools to maintain code quality:

- **Pre-commit hooks**: Automatically format code, check for issues, and ensure consistency
- **R CMD check**: Standard R package checks via GitHub Actions
- **Docker security scanning**: Trivy vulnerability assessment with SARIF integration
- **CodeQL analysis**: Static security analysis of source code and workflows
- **Automated dependency updates**: Dependabot for GitHub Actions and Docker images
- **Code coverage**: Test coverage reporting (to be implemented)
- **pkgdown**: Automatic documentation website generation

For detailed information about our CI/CD workflows, see [WORKFLOWS.md](WORKFLOWS.md).

### Pre-commit Hooks

The pre-commit hooks will automatically:
- Format R code using the tidyverse style guide (`styler`)
- Check for code quality issues (`lintr`)
- Check spelling
- Ensure R code is parsable
- Remove trailing whitespace and fix file endings
- Prevent committing large files or common R artifacts
- Sort entries in `.Rbuildignore`

To manually run hooks on all files:
```bash
pre-commit run --all-files
```

### Ignore Files

The project includes comprehensive ignore files following 2025 best practices:

- **`.gitignore`**: Excludes common R, OS-specific, and IDE files from version control
- **`.Rbuildignore`**: Excludes development files from the built R package
- **`.cursorignore`**: Controls which files Cursor AI can access (security & performance)
- **`.dockerignore`**: Optimizes Docker build context

## Usage

### GitHub Codespaces (Easiest) ☁️

**Zero-setup cloud development**:

1. **Launch**: Click "Code" → "Codespaces" → "Create codespace"
2. **Access RStudio**: Click port 8787 in VS Code Ports tab
3. **Start coding**: Package pre-loaded, all tools ready

```bash
# In Codespaces terminal - helpful shortcuts available:
hetid_demo    # Run package demonstration
hetid_test    # Run comprehensive tests
hetid_sim     # Run quick Monte Carlo simulation
rdev          # Load package in development mode
```

### Local Installation

```r
library(hetid)

# Example usage will be added as functions are developed
```

### Degrees of Freedom Adjustment

The package supports both asymptotic and finite sample standard errors through the `df_adjust` parameter:

```r
# Asymptotic standard errors (default, matches Stata's ivreg2h)
result_asymp <- run_single_lewbel_simulation(
  sim_id = 1, 
  params = params, 
  df_adjust = "asymptotic"
)

# Finite sample standard errors (matches R's lm())
result_finite <- run_single_lewbel_simulation(
  sim_id = 1, 
  params = params, 
  df_adjust = "finite"
)

# Compare methods using the helper function
comparison <- compare_df_adjustments(config)
```

For more details, see `vignette("degrees-of-freedom")`.

### Docker Usage (Recommended for Local Development)

The package includes a complete Docker setup with 2025 best practices for easy development and deployment.

#### Quick Start Guide

**Prerequisites**: Docker Engine 28.0+ and docker-compose

1. **Start Development Environment**:
   ```bash
   make dev-start
   ```
   This launches RStudio Server at http://localhost:8787 with the package pre-loaded.

2. **Run Package Tests**:
   ```bash
   make test
   ```

3. **Execute Monte Carlo Simulations**:
   ```bash
   # Quick simulation (100 iterations)
   make simulation-quick

   # Full simulation (1000 iterations)
   make simulation

   # Sensitivity analysis
   make simulation-sensitivity
   ```

4. **Interactive Development**:
   ```bash
   # Open R console in container
   make dev-r

   # Open bash shell
   make dev-shell

   # View logs
   make dev-logs
   ```

5. **Stop Environment**:
   ```bash
   make dev-stop
   ```

#### Alternative Commands

If you prefer direct Docker commands:

```bash
# Development with RStudio
docker-compose -f docker-compose.yml -f docker-compose.dev.yml up -d hetid-dev

# Run production simulation
docker run --rm hetid:latest R -e "library(hetid); run_lewbel_monte_carlo()"

# Build production image
docker build -t hetid:latest --target production .
```

#### Docker Features

- **Multi-stage builds** for optimized production images
- **RStudio Server** for interactive development
- **Parallel processing** optimized for simulations
- **Automated testing** and quality checks
- **Security hardened** with non-root users
- **Multi-platform** support (AMD64/ARM64)

See [DOCKER.md](DOCKER.md) for comprehensive documentation, troubleshooting, and advanced usage.

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines on:
- Our development workflow
- Coding standards
- How to submit pull requests
- Setting up your development environment

## Research Focus

This project explores various approaches to identifying and testing for heteroskedasticity in econometric models, with particular attention to modern identification strategies. The package aims to provide:

- Implementation of Lewbel (2012) methodology
- Tools for heteroskedasticity testing
- Methods for identification through heteroskedasticity
- Time-series specific adaptations

## Current Status

The package is in early development. Current priorities include:
- Implementing core Lewbel (2012) estimators
- Adding comprehensive tests
- Developing user-friendly vignettes
- Creating simulation studies

See the issues page on GitHub for specific development tasks.

## References

- Lewbel, A. (2012). Using heteroscedasticity to identify and estimate mismeasured and endogenous regressor models. *Journal of Business & Economic Statistics*, 30(1), 67-80.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

This package is part of ongoing research in econometric identification strategies.
