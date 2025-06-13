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

### 2. Create a Feature Branch

```bash
# Ensure your main branch is up to date
git checkout main
git pull upstream main

# Create a new branch for your feature
git checkout -b feature/your-feature-name
```

### 3. Make Your Changes

- Write clear, documented code following the tidyverse style guide
- Add or update tests for new functionality
- Update documentation (roxygen2 comments, vignettes, NEWS.md)
- Ensure your code passes all checks

### 4. Test Locally

```r
# Run checks locally before pushing
devtools::check()
devtools::test()
devtools::document()
```

#### Pre-commit Hooks

This project uses pre-commit hooks to maintain code quality. After cloning:

```bash
# Install pre-commit (requires Python)
pip install pre-commit

# Install the git hooks
pre-commit install

# (Optional) Run hooks on all files to check
pre-commit run --all-files
```

The hooks will automatically format your code and check for common issues before each commit.

### 5. Commit Your Changes

```bash
# Stage and commit with a clear message
git add .
git commit -m "Add feature: brief description of changes"
```

### 6. Push and Create Pull Request

```bash
# Push to your fork
git push origin feature/your-feature-name
```

Then create a pull request on GitHub from your branch to `main`.

## Pull Request Guidelines

### PR Title

Use a clear, descriptive title:
- ✅ "Add function for White's heteroskedasticity test"
- ❌ "Update code"

### PR Description

Include:
- Summary of changes
- Related issue numbers (fixes #123)
- Testing performed
- Any breaking changes

### PR Checklist

Before submitting, ensure:
- [ ] Pre-commit hooks pass (`pre-commit run --all-files`)
- [ ] Code follows tidyverse style guide (enforced by styler)
- [ ] All tests pass (`devtools::test()`)
- [ ] R CMD check passes (`devtools::check()`)
- [ ] Documentation is updated
- [ ] NEWS.md is updated (for user-facing changes)
- [ ] Examples work correctly

## Automated Checks

When you open a PR, GitHub Actions will automatically:
- Run R CMD check on multiple platforms (Windows, macOS, Linux)
- Run tests with coverage reporting
- Check code coverage
- Build documentation

All checks must pass before merging.

## Code Style

We follow the [tidyverse style guide](https://style.tidyverse.org/). Key points:

- Use `<-` for assignment (not `=`)
- Use spaces around operators
- Use descriptive variable names
- Document all exported functions with roxygen2

## Testing

- Write tests for all new functionality
- Use `testthat` for unit tests
- Aim for high test coverage
- Test edge cases and error conditions

## Documentation

- Use roxygen2 for function documentation
- Include examples in function documentation
- Update vignettes for major features
- Keep README.md current

## Release Process

Releases are managed by maintainers:

1. Features are developed on feature branches
2. PRs are merged to `main` after review
3. When ready for release:
   - Version is bumped in DESCRIPTION
   - NEWS.md is updated
   - GitHub release is created with tag
   - pkgdown site is automatically updated

## Getting Help

If you need help:
- Check existing issues and discussions
- Open a new issue with a minimal reproducible example
- Ask questions in discussions

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

Thank you for contributing! 🎉
