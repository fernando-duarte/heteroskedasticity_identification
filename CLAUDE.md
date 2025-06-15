# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is **hetid**, an R package implementing Lewbel (2012)'s identification through heteroskedasticity method for econometric models with endogenous regressors. The package provides tools for estimation and inference when traditional instrumental variables are unavailable.

## Development Commands

### Docker-based Development (Recommended)
```bash
# Start development environment with RStudio Server at http://localhost:8787
make dev-start

# Run package tests
make test

# Run R CMD check
make check

# Stop development environment
make dev-stop

# Open R console in container
make dev-r

# Open shell in development container
make dev-shell
```

### Simulation Commands
```bash
# Quick simulation (100 iterations)
make simulation-quick

# Full simulation (1000 iterations) 
make simulation

# Bootstrap demonstration
make simulation-bootstrap

# Sensitivity analysis
make simulation-sensitivity

# Show simulation status
make simulation-status
```

### Code Quality
```bash
# Format R code with styler
make format

# Run lintr code quality checks
make lint

# Generate package documentation
make docs

# Build pkgdown documentation site
make docs-site
```

### Traditional R Development
```r
# Install dependencies
devtools::install_deps(dependencies = TRUE)

# Load package for development
devtools::load_all()

# Run tests
devtools::test()

# Run R CMD check
devtools::check()

# Generate documentation
devtools::document()
```

## Architecture Overview

### Core Econometric Implementation

The package implements a **triangular econometric model** with single-factor error structure following Lewbel (2012):

```
Y₁ = β₁₀ + β₁₁X + γ₁Y₂ + ε₁  (outcome equation)
Y₂ = β₂₀ + β₂₁X + ε₂           (endogenous regressor equation)
```

**Key Functions:**
- `generate_lewbel_data()`: Creates synthetic data satisfying Lewbel assumptions
- `calculate_lewbel_bounds()`: Computes set identification bounds with bootstrap SEs
- `verify_lewbel_assumptions()`: Tests identifying assumptions (covariance restriction, instrument relevance)

### Monte Carlo Simulation Framework

The package includes comprehensive parallel simulation infrastructure:

**Main Orchestrator:** `run_lewbel_monte_carlo()` - coordinates all simulation components
**Parallel Execution:** Uses `furrr` and `future` packages for efficient parallel processing
**Configuration Management:** `create_default_config()` provides centralized parameter control
**Reproducibility:** `generate_all_seeds()` ensures reproducible parallel execution

### Function Organization (R/ Directory)

- **`estimation.R`**: Core estimation functions (`calculate_lewbel_bounds`, `run_single_lewbel_simulation`)
- **`data-generation.R`**: Data generation and validation (`generate_lewbel_data`, `verify_lewbel_assumptions`)  
- **`lewbel-monte-carlo.R`**: Main simulation orchestrator (`run_lewbel_monte_carlo`, `run_lewbel_demo`)
- **`simulation.R`**: Parallel simulation infrastructure (main, bootstrap, sample size, sensitivity analysis)
- **`utils.R`**: Configuration and seed management
- **`analysis.R`**: Results analysis with performance metrics (coverage, bias, RMSE)
- **`visualization.R`**: ggplot2-based plotting functions
- **`messager.R`**, **`stopper.R`**: Messaging and error handling utilities

### Testing Structure

Comprehensive test suite covering:
- Basic functionality (`test-basic.R`, `test-data-generation.R`)
- Edge cases (`test-edge-cases.R`, `test-estimation-errors.R`) 
- Component-specific tests (`test-simulations.R`, `test-analysis.R`)
- Utility functions (`test-messager.R`, `test-stopper.R`)

**Warning Handling:** Expected warnings in edge cases (e.g., mean() on empty columns, zero variance in cor()) are properly suppressed and documented in tests.

## Key Development Patterns

### Lewbel Estimation Pattern
```r
# Standard pattern for Lewbel estimation functions
data <- validate_lewbel_data(data) 
instruments <- construct_lewbel_instruments(data)
results <- estimate_with_instruments(data, instruments)
results <- add_diagnostics(results)
class(results) <- c("lewbel_fit", class(results))
return(results)
```

### Simulation Pattern  
```r
# Standard pattern for Monte Carlo simulations
set.seed(seed)
params <- validate_simulation_params(params)
results <- furrr::future_map_dfr(
  1:n_sims,
  ~ run_single_simulation(.x, params),
  .options = furrr::furrr_options(seed = TRUE)
)
summary_stats <- compute_summary_statistics(results)
return(list(results = results, summary = summary_stats))
```

### Parallel Processing Setup
```r
# Setup parallel processing
future::plan(future::multisession, workers = 4)

# Use furrr for parallel execution
results <- furrr::future_map(
  .x = simulations,
  .f = run_single_simulation,
  .options = furrr::furrr_options(seed = TRUE),
  .progress = TRUE
)

# Always reset plan
future::plan(future::sequential)
```

## Package Dependencies

**Core Statistical:** `AER` (instrumental variables), `boot` (bootstrap), `stats`
**Data Manipulation:** `dplyr`, `tidyr`, `purrr` 
**Parallel Processing:** `future`, `furrr`
**Visualization:** `ggplot2`
**Development:** `testthat` (≥3.0.0), `knitr`, `rmarkdown`

## CI/CD Workflows

- **R-CMD-check**: Multi-platform R package checking (Ubuntu, Windows, macOS)
- **Docker**: Build, test, security scanning, multi-platform deployment
- **CodeQL**: Security analysis
- **pkgdown**: Automated documentation site generation

## Code Style Requirements

The package enforces **tidyverse style guide** via pre-commit hooks:
- Assignment: Always use `<-`, never `=`
- Line length: Maximum 80 characters
- Object names: snake_case only
- Spacing: Follow tidyverse rules (spaces after commas, around operators)
- No debug code: Never include `browser()` or `debug()` statements
- Use explicit namespacing: `package::function()` for external functions

## Special Considerations

**Methodological:** Implements specific Lewbel (2012) assumptions including triangular model structure and heteroskedasticity-based identification
**Computational:** Optimized for parallel Monte Carlo simulations with comprehensive seed management  
**Reproducibility:** All simulations use controlled seeding for reproducible results
**Error Handling:** Robust error handling throughout with informative error messages
**Testing:** Comprehensive edge case coverage including proper handling of expected warnings