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

## Contents

- `R/` - R functions implementing the methodology
- `lewbel2012/` - Implementation and analysis related to Lewbel (2012) methodology
  - `lewbel2012.tex` - LaTeX source file
  - `lewbel2012.pdf` - Compiled PDF document
  - Supporting files (`.aux`, `.log`, `.synctex.gz`)
- `tests/` - Unit tests
- `vignettes/` - Package vignettes

## Development

This package uses pre-commit hooks to ensure code quality and consistency.

### Setting up pre-commit hooks

1. Install pre-commit (requires Python):
   ```bash
   pip install pre-commit
   ```

2. Install the git hooks:
   ```bash
   pre-commit install
   ```

3. (Optional) Run hooks on all files:
   ```bash
   pre-commit run --all-files
   ```

The pre-commit hooks will automatically:
- Format R code using the tidyverse style guide (`styler`)
- Check for code quality issues (`lintr`)
- Check spelling
- Ensure R code is parsable
- Remove trailing whitespace and fix file endings
- Prevent committing large files or common R artifacts

### Contributing

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for details on our code of conduct and the process for submitting pull requests.

## Research Focus

This project explores various approaches to identifying and testing for heteroskedasticity in econometric models, with particular attention to modern identification strategies.

## References

- Lewbel, A. (2012). Using heteroscedasticity to identify and estimate mismeasured and endogenous regressor models. *Journal of Business & Economic Statistics*, 30(1), 67-80.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.
