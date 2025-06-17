# hetid: Heteroskedasticity Identification

<!-- badges: start -->
[![R-CMD-check](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/R-CMD-check.yml)
[![Docker Build and Test](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/docker.yml/badge.svg)](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/docker.yml)
[![pkgdown](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/pkgdown.yml/badge.svg)](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/pkgdown.yml)
[![pre-commit](https://img.shields.io/badge/pre--commit-enabled-brightgreen?logo=pre-commit&logoColor=white)](https://github.com/pre-commit/pre-commit)
[![Codecov test coverage](https://codecov.io/gh/fernando-duarte/heteroskedasticity_identification/graph/badge.svg)](https://app.codecov.io/gh/fernando-duarte/heteroskedasticity_identification)
[![Security Checks](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/r-security.yml/badge.svg)](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/r-security.yml)
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

# For comparison with other Lewbel implementations
install.packages("REndo")  # Version >= 2.4.0 required
install.packages("AER")    # For ivreg function

# For Stata comparison (if Stata is available)
install.packages("RStata")
install.packages("haven")
```

The package will work without these dependencies, but installing them provides:
- **knitr**: Nicely formatted tables in analysis functions (when `verbose = TRUE`)
- **REndo**: Comparison with alternative Lewbel implementation
- **AER**: Additional IV regression capabilities
- **RStata/haven**: Stata integration for validation studies

## Project Structure

```
heteroskedasticity_identification/
├── R/                    # R functions implementing the methodology
│   ├── analysis.R       # Results analysis and performance metrics
│   ├── data-generation.R # Data generation and validation
│   ├── estimation.R     # Core Lewbel estimation functions
│   ├── lewbel-monte-carlo.R # Main simulation orchestrator
│   ├── simulation.R     # Parallel simulation infrastructure
│   ├── visualization.R  # ggplot2-based plotting functions
│   ├── utils.R          # Configuration and utilities
│   ├── messager.R       # Messaging utilities
│   └── stopper.R        # Error handling utilities
├── lewbel2012/          # Academic paper and LaTeX source
│   ├── lewbel2012.tex   # LaTeX source file
│   ├── lewbel2012.lyx   # LyX source file
│   └── refs.bib         # Bibliography
├── tests/               # Comprehensive unit tests
│   └── testthat/        # testthat test files
├── vignettes/           # Package vignettes
│   ├── getting-started.Rmd      # Introduction to hetid
│   ├── package-comparison.Rmd   # Comparing implementations
│   └── degrees-of-freedom.Rmd   # DF adjustment methods
├── dev/                 # Development documentation
│   └── internal-docs/   # Internal development guides
├── data/                # Package datasets
│   └── lewbel_sim.rda   # Example simulation data
├── man/                 # Documentation (generated)
├── docs/                # pkgdown site (generated)
├── .github/             # GitHub Actions workflows
│   ├── workflows/
│   │   ├── R-CMD-check.yml   # R package CI/CD
│   │   ├── docker.yml        # Docker build and security
│   │   └── pkgdown.yml       # Documentation site
│   └── dependabot.yml       # Automated dependency updates
└── inst/                # Installed files
    ├── hooks/           # Pre-commit hook scripts
    └── WORDLIST         # Spell check dictionary
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
   - VS Code with R extensions ready to use
   - Package pre-loaded and ready to use
   - All tools and dependencies included

**Features**: RStudio Server, VS Code, R 4.5.0, all dependencies, Git integration, Docker support

> **New**: Modern devcontainer with R 4.5.0 available! Includes both VS Code and RStudio in one environment. See [.devcontainer/README-modern.md](.devcontainer/README-modern.md) for details.

#### Option 2: Local Docker Development 🐳

**Consistent environment** across all machines:

1. **Prerequisites**: Docker Engine 28.0+ and docker-compose

2. **Quick Start**:
   ```bash
   git clone https://github.com/fernando-duarte/heteroskedasticity_identification.git
   cd heteroskedasticity_identification
   make dev-start  # Launches RStudio at http://localhost:8787
   ```

**R Versions**:
- Production images: R 4.5.0 (updated June 2025)
- CI/CD testing: Both R 4.5.0 (release) and R 4.4.x (oldrel)
- Full compatibility maintained

See [dev/internal-docs/DOCKER.md](dev/internal-docs/DOCKER.md) for comprehensive Docker documentation.

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
- **Automated dependency updates**: Dependabot for GitHub Actions and Docker images
- **Code coverage**: Test coverage reporting (to be implemented)
- **pkgdown**: Automatic documentation website generation

For detailed information about our CI/CD workflows, see [dev/internal-docs/WORKFLOWS.md](dev/internal-docs/WORKFLOWS.md).

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

# Quick demonstration
run_lewbel_demo()

# Create default configuration
config <- create_default_config()

# Generate example data
data <- generate_lewbel_data(n = 100, config = config)

# Run single simulation
result <- run_single_lewbel_simulation(1, config)

# Run full Monte Carlo study
mc_results <- run_lewbel_monte_carlo(config)
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

## Documentation and Vignettes

The package includes comprehensive documentation with three detailed vignettes:

### 1. Getting Started (`vignette("getting-started")`)
- Introduction to the hetid package
- Basic usage examples
- Understanding Lewbel (2012) methodology
- Quick start guide for new users

### 2. Package Comparison (`vignette("package-comparison")`)
- Detailed comparison with REndo package
- Validation against Stata's ivreg2h
- Understanding implementation differences
- Choosing the right approach for your research

### 3. Degrees of Freedom (`vignette("degrees-of-freedom")`)
- Asymptotic vs. finite sample standard errors
- When to use each approach
- Practical implications for inference
- Comparison with other software

### Additional Documentation
- **Function reference**: Complete documentation for all exported functions
- **pkgdown website**: [https://fernando-duarte.github.io/heteroskedasticity_identification/](https://fernando-duarte.github.io/heteroskedasticity_identification/)
- **Development guides**: See `dev/internal-docs/` for technical documentation

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

#### Platform-Specific Optimizations

**Apple Silicon (M1/M2/M3) Users**: For 40% faster builds, use x86_64 emulation:

```bash
# Force x86_64 architecture for pak compatibility
docker build --platform linux/amd64 -t hetid:dev-x86 -f Dockerfile.dev .
docker run --platform linux/amd64 -p 8787:8787 hetid:dev-x86
```

This works because pak's parallel installation (even with emulation overhead) is much faster than sequential `install.packages()` on native ARM64.

#### Docker Features

- **Multi-stage builds** for optimized production images
- **RStudio Server** for interactive development
- **Parallel processing** optimized for simulations
- **Automated testing** and quality checks
- **Security hardened** with non-root users
- **Multi-platform** support (AMD64/ARM64)

See [dev/internal-docs/DOCKER.md](dev/internal-docs/DOCKER.md) for comprehensive documentation, troubleshooting, and advanced usage.

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines on:
- Our development workflow
- Coding standards
- How to submit pull requests
- Setting up your development environment

### Security Setup for Contributors

This repository uses automated security scanning with `oysteR` to check for vulnerabilities in R package dependencies. To avoid rate limiting issues when running security checks:

1. **Register for OSS Index credentials** (free):
   - Go to https://ossindex.sonatype.org/
   - Create an account
   - Get your API token from User Settings

2. **For local development**, add to your `.Renviron`:
   ```
   OSSINDEX_USER=your-email@example.com
   OSSINDEX_TOKEN=your-api-token
   ```

3. **For GitHub Actions** (maintainers only):
   - Add `OSSINDEX_USER` and `OSSINDEX_TOKEN` as repository secrets
   - Go to Settings → Secrets and variables → Actions

See [dev/internal-docs/CI_CD_SUCCESS_SUMMARY.md](dev/internal-docs/CI_CD_SUCCESS_SUMMARY.md) for detailed information about our security practices and CI/CD setup.

## Research Focus

This project implements the identification through heteroskedasticity methodology of Lewbel (2012) for econometric models with endogenous regressors. The package provides:

- **Complete implementation** of Lewbel (2012) methodology
- **Set identification** when point identification fails
- **Comparison tools** with other implementations (REndo, Stata)
- **Monte Carlo validation** of theoretical results
- **Practical guidance** on degrees of freedom adjustments
- **Visualization tools** for understanding estimator performance

### Academic Applications
- Identification in models with endogenous regressors
- Situations where traditional instruments are unavailable
- Comparison of identification strategies
- Understanding finite sample vs. asymptotic properties

## Current Status

The package is **feature-complete** and implements the full Lewbel (2012) methodology. Current capabilities include:

### ✅ Implemented Features
- **Core Lewbel (2012) estimators** with set identification
- **Comprehensive Monte Carlo simulations** with parallel processing
- **Multiple analysis types**: main simulation, bootstrap, sample size, sensitivity
- **Visualization suite** with ggplot2-based plotting functions
- **Comparison tools** for different implementations (REndo, Stata)
- **Degrees of freedom adjustments** (asymptotic vs. finite sample)
- **Comprehensive test suite** with >95% code coverage
- **Three detailed vignettes** with examples and comparisons
- **Docker development environment** with RStudio Server
- **CI/CD workflows** with automated testing and security scanning

### 📊 Available Functions
- `run_lewbel_demo()` - Quick package demonstration
- `run_lewbel_monte_carlo()` - Full Monte Carlo study
- `calculate_lewbel_bounds()` - Set identification bounds
- `generate_lewbel_data()` - Simulate data under Lewbel assumptions
- `plot_estimator_distributions()` - Visualize estimator performance
- `compare_df_adjustments()` - Compare standard error methods

### 🔬 Research Applications
- Identification through heteroskedasticity
- Set identification when point identification fails
- Comparison with traditional IV methods
- Finite sample vs. asymptotic inference

See the [package website](https://fernando-duarte.github.io/heteroskedasticity_identification/) for complete documentation and examples.

## References

- Lewbel, A. (2012). Using heteroscedasticity to identify and estimate mismeasured and endogenous regressor models. *Journal of Business & Economic Statistics*, 30(1), 67-80.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

This package is part of ongoing research in econometric identification strategies.
