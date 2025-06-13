# hetid: Heteroskedasticity Identification

<!-- badges: start -->
[![R-CMD-check](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/rworkflows.yml/badge.svg)](https://github.com/fernando-duarte/heteroskedasticity_identification/actions/workflows/rworkflows.yml)
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
│   └── workflows/
│       └── rworkflows.yml
└── inst/                # Installed files
    └── hooks/           # Pre-commit hook scripts
```

## Development

This package uses modern development practices to ensure code quality and consistency.

### Development Environment Setup

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

4. **(Optional) For Cursor AI users**: The repository includes a `.cursorignore` file that prevents the AI from accessing sensitive files and improves performance by excluding large generated files.

5. **(Optional) For Docker users**: A `.dockerignore` file is included to optimize Docker builds.

### Code Quality Tools

The project uses several tools to maintain code quality:

- **Pre-commit hooks**: Automatically format code, check for issues, and ensure consistency
- **R CMD check**: Standard R package checks via GitHub Actions
- **Code coverage**: Test coverage reporting (to be implemented)
- **pkgdown**: Automatic documentation website generation

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

(To be expanded as more functions are implemented)

```r
library(hetid)

# Example usage will be added as functions are developed
```

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
