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

# For comparison with other Lewbel (2012) implementations
install.packages("REndo")  # Version >= 2.4.0 required
install.packages("AER")    # For ivreg function

# For Stata comparison (if Stata is available)
install.packages("RStata")
install.packages("haven")
```

The package will work without these dependencies, but installing them provides:
- **knitr**: Nicely formatted tables in analysis functions (when `verbose = TRUE`)
- **REndo**: Comparison with alternative R implementation of Lewbel (2012)
- **AER**: Additional IV regression capabilities
- **RStata/haven**: Comparison with alternative Stata implementation of Lewbel (2012)

## Development

For comprehensive development setup instructions, code quality tools, and contribution guidelines, see **[DEVELOPMENT.md](DEVELOPMENT.md)**.

## Usage

For comprehensive usage examples, advanced features, and troubleshooting, see **[USAGE.md](USAGE.md)**.

### Quick Start

```r
# Install and load
devtools::install_github("fernando-duarte/heteroskedasticity_identification")
library(hetid)

# Quick demonstration
run_lewbel_demo()

# Basic workflow
config <- create_default_config()
data <- generate_lewbel_data(n = 100, config = config)
result <- run_single_lewbel_simulation(1, config)
```

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
