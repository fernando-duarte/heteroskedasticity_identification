# hetid (development version)

## Infrastructure and Development Environment (2025-01-01)

### New Features
* Added comprehensive ignore files following 2025 best practices:
  - Updated `.gitignore` with patterns from GitHub's official R template
  - Restructured `.Rbuildignore` with clear sections and modern patterns
  - Added `.cursorignore` for Cursor AI editor (security & performance)
  - Added `.dockerignore` for optimized Docker builds

### Development Tools
* Configured pre-commit hooks for code quality:
  - R code formatting with `styler`
  - Code linting with `lintr`
  - Spell checking
  - File formatting (trailing whitespace, end-of-file)
  - Prevention of large files and R artifacts in commits

* Set up GitHub Actions workflow:
  - R CMD check on multiple platforms
  - Automated testing
  - Documentation building

### Documentation
* Updated README.md with:
  - Comprehensive project structure
  - Development environment setup instructions
  - Information about new ignore files
  - Current development status

* Enhanced CONTRIBUTING.md with:
  - Detailed development workflow
  - Code style guidelines
  - Testing requirements
  - Documentation standards

### Initial Package Structure
* Basic package skeleton with:
  - Utility functions (`messager.R`, `stopper.R`)
  - Test infrastructure (`testthat`)
  - Vignette structure (`getting-started.Rmd`)
  - pkgdown configuration

---

# hetid 0.1.0 (Planned Release)

This will be the first release of hetid, implementing the core functionality of Lewbel (2012) identification through heteroskedasticity method.

## Planned Features
* Core Lewbel (2012) estimator implementation
* Functions for heteroskedasticity testing
* Time-series adaptations
* Comprehensive documentation and vignettes
* Full test coverage
