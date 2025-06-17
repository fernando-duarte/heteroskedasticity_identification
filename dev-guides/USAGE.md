# Usage Guide

This guide provides comprehensive examples and instructions for using the hetid package.

## Quick Start

### Installation

```r
# Install from GitHub
devtools::install_github("fernando-duarte/heteroskedasticity_identification")

# Load the package
library(hetid)
```

## GitHub Codespaces (Easiest) ☁️

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

## Local Installation

### Basic Usage Examples

```r
library(hetid)

# Quick demonstration
run_lewbel_demo()

# Create default configuration
config <- create_default_config()

# Generate example data
data <- generate_lewbel_data(100, config)

# Run single simulation
result <- run_single_lewbel_simulation(1, config)

# Run full Monte Carlo study
mc_results <- run_lewbel_monte_carlo(config)
```

### Advanced Usage

#### Configuration and Data Generation

```r
# Create custom configuration
config <- create_default_config()
config$n_obs <- 500
config$n_sims <- 1000
config$gamma <- 0.5

# Generate data with specific parameters
data <- generate_lewbel_data(config$n_obs, config)

# Verify Lewbel assumptions
assumptions <- verify_lewbel_assumptions(data, config)
print(assumptions)
```

#### Monte Carlo Simulations

```r
# Run different types of simulations
seeds <- generate_all_seeds(config)
main_results <- run_main_simulation(config, seeds)
bootstrap_results <- run_bootstrap_demonstration(config, seeds)
sample_size_results <- run_sample_size_analysis(config, seeds)
sensitivity_results <- run_sensitivity_analysis(config, seeds)
```

#### Analysis and Visualization

```r
# Analyze results
analysis <- analyze_main_results(main_results, config)
bootstrap_analysis <- analyze_bootstrap_results(bootstrap_results, config)

# Create visualizations
plot_estimator_distributions(main_results, config)
plot_sample_size_consistency(sample_size_results, config)
plot_het_sensitivity(sensitivity_results, config)
plot_bootstrap_ci(bootstrap_results, config)

# Generate all plots at once
all_plots <- generate_all_plots(main_results, config)
```

## Degrees of Freedom Adjustment

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

## Set Identification

When point identification fails, the package provides set identification:

```r
# Calculate Lewbel bounds
bounds <- calculate_lewbel_bounds(data, config)

# Check if point identified
if (bounds$point_identified) {
  cat("Point identified! Estimate:", bounds$point_estimate)
} else {
  cat("Set identified. Bounds:", bounds$lower_bound, "to", bounds$upper_bound)
}
```

## Comparison with Other Implementations

### REndo Package Comparison

```r
# Compare with REndo (if available)
if (has_rendo()) {
  rendo_comparison <- compare_with_rendo(data, config)
  print(rendo_comparison)
}
```

### Stata Comparison

```r
# Compare with Stata (if available)
if (has_stata()) {
  stata_comparison <- compare_with_stata(data, config)
  print(stata_comparison)
}
```

## Parallel Processing

The package uses parallel processing for Monte Carlo simulations:

```r
# Set up parallel processing
library(future)
plan(multisession, workers = 4)

# Run simulation (will use parallel processing automatically)
results <- run_lewbel_monte_carlo(config)

# Reset to sequential processing
plan(sequential)
```

## Error Handling and Diagnostics

```r
# Check for common issues
assumptions_result <- verify_lewbel_assumptions(data, config, verbose = FALSE)
if (assumptions_result$p_value < 0.05) {
  warning("Lewbel covariance restriction may be violated (p < 0.05)")
}

# Handle missing dependencies gracefully
if (!has_curl()) {
  message("Install curl package for enhanced functionality")
}

# Verbose output for debugging
config$verbose <- TRUE
result <- run_single_lewbel_simulation(1, config)
```

## Working with Results

```r
# Extract key statistics
point_estimates <- results$tsls_gamma1
standard_errors <- results$tsls_se
ols_estimates <- results$ols_gamma1

# Summary statistics
summary_stats <- analyze_main_results(results, config, verbose = FALSE)
print(summary_stats)
```

## Tips and Best Practices

1. **Start with the demo**: Always run `run_lewbel_demo()` first to understand the package
2. **Check assumptions**: Use `verify_lewbel_assumptions()` before analysis
3. **Use appropriate sample sizes**: Lewbel method works better with larger samples
4. **Consider degrees of freedom**: Choose between asymptotic and finite sample adjustments
5. **Parallel processing**: Use `future::plan()` for faster Monte Carlo simulations
6. **Visualization**: Always plot results to understand estimator behavior
