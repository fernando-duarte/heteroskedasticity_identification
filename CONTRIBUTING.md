# Contributing to heteroskedasticity_identification

Thank you for your interest in contributing to the heteroskedasticity_identification R package! This document provides guidelines and workflows for contributing.

## Development Workflow

We follow the **GitHub Flow** branching strategy, which is simple and effective for R package development.

### Branching Strategy

- **`main`** - The stable, production-ready branch. All releases are made from this branch.
- **Feature branches** - Short-lived branches for developing new features or fixes.

### Branch Naming Conventions

Use descriptive branch names with prefixes:

- `feature/` - New features (e.g., `feature/add-robust-variance`)
- `fix/` - Bug fixes (e.g., `fix/issue-42`)
- `docs/` - Documentation updates (e.g., `docs/improve-vignette`)
- `test/` - Test additions or improvements (e.g., `test/coverage-improvement`)

## How to Contribute

### 1. Fork and Clone

```bash
# Fork the repository on GitHub, then:
git clone https://github.com/YOUR-USERNAME/heteroskedasticity_identification.git
cd heteroskedasticity_identification
git remote add upstream https://github.com/fernando-duarte/heteroskedasticity_identification.git
```

### 2. Set Up Development Environment

```bash
# Install R package dependencies
Rscript -e "devtools::install_deps(dependencies = TRUE)"

# Install pre-commit (requires Python)
pip install pre-commit
pre-commit install

# Verify setup
pre-commit run --all-files
```

#### Development Tools

The project uses several modern development tools:

- **Cursor AI**: If using Cursor, the `.cursorignore` file prevents AI access to sensitive files
- **Docker**: Optional `.dockerignore` file is included for containerized development
- **Pre-commit hooks**: Automated code quality checks

### 3. Create a Feature Branch

```bash
# Ensure your main branch is up to date
git checkout main
git pull upstream main

# Create a new branch for your feature
git checkout -b feature/your-feature-name
```

### 4. Make Your Changes

- Write clear, documented code following the tidyverse style guide
- Add or update tests for new functionality
- Update documentation (roxygen2 comments, vignettes, NEWS.md)
- Ensure your code passes all checks

### 5. Test Locally

```r
# Run checks locally before pushing
devtools::check()
devtools::test()
devtools::document()

# Check code coverage (if covr is installed)
covr::package_coverage()
```

#### Pre-commit Hooks

This project uses pre-commit hooks to maintain code quality. The hooks will automatically:

- Format R code using `styler` (tidyverse style guide)
- Check code quality with `lintr`
- Check spelling
- Ensure R code is parsable
- Remove trailing whitespace and fix file endings
- Sort `.Rbuildignore` entries
- Prevent committing large files or common R artifacts

To manually run hooks:
```bash
# Run on staged files
pre-commit run

# Run on all files
pre-commit run --all-files

# Run specific hook
pre-commit run styler --all-files
```

### 6. Commit Your Changes

```bash
# Stage and commit with a clear message
git add .
git commit -m "Add feature: brief description of changes"
```

The pre-commit hooks will run automatically. If they make changes, review them and commit again.

### 7. Push and Create Pull Request

```bash
# Push to your fork
git push origin feature/your-feature-name
```

Then create a pull request on GitHub from your branch to `main`.

## Pull Request Guidelines

### PR Title

Use a clear, descriptive title:
- ✅ "Add function for White's heteroskedasticity test"
- ✅ "Fix: Correct variance calculation in lewbel_estimator()"
- ❌ "Update code"
- ❌ "Fixed stuff"

### PR Description

Include:
- Summary of changes
- Related issue numbers (fixes #123)
- Testing performed
- Any breaking changes
- Screenshots/examples if applicable

### PR Checklist

Before submitting, ensure:
- [ ] Pre-commit hooks pass (`pre-commit run --all-files`)
- [ ] Code follows tidyverse style guide (enforced by styler)
- [ ] All tests pass (`devtools::test()`)
- [ ] R CMD check passes (`devtools::check()`)
- [ ] Documentation is updated
- [ ] NEWS.md is updated (for user-facing changes)
- [ ] Examples work correctly
- [ ] No sensitive information in code or comments

## Automated Checks

When you open a PR, GitHub Actions will automatically:
- Run R CMD check on multiple platforms (Windows, macOS, Linux)
- Run tests with coverage reporting
- Check code coverage
- Build documentation
- Deploy pkgdown site (for merged PRs)

All checks must pass before merging.

## Code Style

We follow the [tidyverse style guide](https://style.tidyverse.org/). Key points:

- Use `<-` for assignment (not `=`)
- Use spaces around operators
- Use descriptive variable names (prefer `heteroskedasticity_test` over `het_test`)
- Document all exported functions with roxygen2
- Include examples in documentation

## Testing

- Write tests for all new functionality using `testthat`
- Test files should be named `test-<functionality>.R`
- Aim for high test coverage (>80%)
- Test edge cases and error conditions
- Use meaningful test descriptions

Example test structure:
```r
test_that("lewbel_estimator handles missing data correctly", {
  # Test setup
  data <- generate_test_data()

  # Test expectation
  expect_error(
    lewbel_estimator(data_with_na),
    "Data contains missing values"
  )
})
```

## Documentation

- Use roxygen2 for function documentation
- Include `@examples` in all exported functions
- Update vignettes for major features
- Keep README.md current
- Document any breaking changes in NEWS.md

Example documentation:
```r
#' Estimate model using Lewbel (2012) method
#'
#' @param y Numeric vector of dependent variable
#' @param X Matrix of explanatory variables
#' @param Z Matrix of instruments (optional)
#'
#' @return A list of class "lewbel" containing estimates
#' @export
#'
#' @examples
#' data <- generate_lewbel_data(n = 100)
#' fit <- lewbel_estimator(data$y, data$X)
#' summary(fit)
```

## Project Structure

Understanding the project structure helps navigate the codebase:

```
.
├── R/                    # R source code
├── tests/               # Test files
│   └── testthat/       # testthat tests
├── man/                # Documentation (generated)
├── vignettes/          # Long-form documentation
├── inst/               # Installed files
├── data/               # Package data (if any)
├── .github/            # GitHub Actions workflows
└── ignore files:       # Development configuration
    ├── .gitignore      # Git ignore patterns
    ├── .Rbuildignore   # R build ignore patterns
    ├── .cursorignore   # Cursor AI ignore patterns
    └── .dockerignore   # Docker ignore patterns
```

## Release Process

Releases are managed by maintainers:

1. Features are developed on feature branches
2. PRs are merged to `main` after review
3. When ready for release:
   - Version is bumped in DESCRIPTION
   - NEWS.md is updated with all changes
   - R CMD check passes on all platforms
   - GitHub release is created with tag
   - Package is submitted to CRAN (if applicable)
   - pkgdown site is automatically updated

## Getting Help

If you need help:
- Check existing issues and discussions
- Review the package documentation
- Open a new issue with a minimal reproducible example
- Ask questions in discussions

For bugs, please include:
- Your R version and OS
- Minimal code to reproduce the issue
- Expected vs actual behavior
- Any error messages

## Code of Conduct

Please note that this project follows a Contributor Code of Conduct. By participating in this project you agree to:
- Use welcoming and inclusive language
- Be respectful of differing viewpoints
- Gracefully accept constructive criticism
- Focus on what is best for the community
- Show empathy towards other community members

## Current Development Priorities

Key areas include:
- Implementing core Lewbel (2012) functionality
- Adding comprehensive test coverage
- Creating user-friendly vignettes
- Performance optimization for large datasets

For detailed tasks and priorities, check the GitHub issues page.

Thank you for contributing! 🎉
