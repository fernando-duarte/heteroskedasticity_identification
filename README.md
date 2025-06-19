# hetid: Heteroskedasticity Identification

<!-- badges: start -->
[![R-CMD-check](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/R-CMD-check.yml)
[![Docker Build and Test](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/docker.yml/badge.svg)](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/docker.yml)
[![pkgdown](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/pkgdown.yml/badge.svg)](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/pkgdown.yml)
[![pre-commit](https://img.shields.io/badge/pre--commit-enabled-brightgreen?logo=pre-commit&logoColor=white)](https://github.com/pre-commit/pre-commit)
[![codecov](https://codecov.io/gh/fernando-duarte/heteroskedasticity_identification/graph/badge.svg?token=2PGQW4CZHL)](https://codecov.io/gh/fernando-duarte/heteroskedasticity_identification)
[![Security Checks](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/r-security.yml/badge.svg)](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/r-security.yml)
<!-- badges: end -->

## Overview

The `hetid` package implements identification through heteroskedasticity methods for models with endogenous regressors. It provides tools for estimation and inference using:

- **Lewbel (2012)**: Identification using continuous heteroskedasticity drivers
- **Rigobon (2003)**: Identification using discrete regime indicators
- **Set identification** when point identification hypotheses are not met
- **Comparison tools** with other implementations (REndo, Stata)
- **Monte Carlo validation** of theoretical results
- **Visualization tools** for understanding estimator performance

### Applications
- Identification in models with endogenous regressors by heteroskedasticity
- Useful when traditional instruments are unavailable

See the [package website](https://fernando-duarte.github.io/heteroskedasticity_identification/) for complete documentation and examples.

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

### Quick Start

```r
# Install and load
devtools::install_github("fernando-duarte/heteroskedasticity_identification")
library(hetid)

# Quick demonstration
run_lewbel_demo()

# Basic workflow
config <- create_default_config()
data <- generate_lewbel_data(100, config)
result <- run_single_lewbel_simulation(1, config)
```

The package also implements **Rigobon (2003)** regime-based identification. See the [Rigobon Guide](dev-guides/RIGOBON.md) for detailed usage.

## Guides

**[Development](dev-guides/DEVELOPMENT.md)**.
**[Usage](dev-guides/USAGE.md)**.
**[Troubleshooting](dev-guides/TROUBLESHOOTING.md)**.
**[Rigobon Method](dev-guides/RIGOBON.md)**.

## References

- Lewbel, A. (2012). Using heteroscedasticity to identify and estimate mismeasured and endogenous regressor models. *Journal of Business & Economic Statistics*, 30(1), 67-80.
- Rigobon, R. (2003). Identification through heteroskedasticity. *The Review of Economics and Statistics*, 85(4), 777-792.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
