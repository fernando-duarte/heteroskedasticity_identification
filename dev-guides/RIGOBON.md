# Rigobon (2003) Regime-Based Identification Guide

This guide covers the implementation and usage of Rigobon's (2003) identification through heteroskedasticity method in the `hetid` package.

## Overview

The package implements Rigobon's (2003) identification strategy, which uses discrete regime indicators (e.g., policy periods, market conditions) as heteroskedasticity drivers. This method is particularly useful when:

- You have clear regime changes (policy shifts, market conditions, time periods)
- Heteroskedasticity varies across these discrete regimes
- Traditional instruments are unavailable
- The variance structure changes discretely across identifiable states

## Quick Start

```r
# Quick Rigobon analysis with default parameters
results <- run_rigobon_analysis()

# Custom regime structure
params <- list(
  beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
  beta2_0 = 1.0, beta2_1 = -1.0,
  alpha1 = -0.5, alpha2 = 1.0,
  regime_probs = c(0.3, 0.7),    # 30% in regime 1, 70% in regime 2
  sigma2_regimes = c(1.0, 3.0)    # Variance 3x higher in regime 2
)
results <- run_rigobon_analysis(n_obs = 1000, params = params)
```

## Using Your Own Data

If you have data with regime indicators:

```r
# Using your own data with regime indicators
results <- run_rigobon_analysis(
  data = my_data,
  regime_var = "period",      # Name of your regime variable
  endog_var = "Y2",          # Endogenous variable
  exog_vars = c("X1", "X2")  # Exogenous variables
)
```

## Validating Assumptions

Before trusting the results, validate the key assumptions:

```r
# Validate assumptions
validation <- validate_rigobon_assumptions(data)

# The function tests:
# 1. Heteroskedasticity across regimes
# 2. Covariance restriction (centered regime dummies uncorrelated with error product)
# 3. Constant covariance between errors across regimes
```

## Comparing Methods

Compare Rigobon with OLS and Lewbel methods:

```r
# Compare methods
comparison <- compare_rigobon_methods(
  data = my_data,
  true_gamma1 = -0.8,  # If known
  methods = c("OLS", "Rigobon", "Lewbel")
)
```

## Main Functions

### `run_rigobon_analysis()`

Complete Rigobon analysis workflow:

```r
results <- run_rigobon_analysis(
  n_obs = 1000,           # Sample size
  params = NULL,          # Parameters (NULL for defaults)
  data = NULL,            # Pre-existing data (optional)
  regime_var = "regime",  # Name of regime variable
  verbose = TRUE,         # Print detailed results
  return_all = FALSE      # Return all intermediate results
)
```

### `generate_rigobon_data()`

Generate data with regime-based heteroskedasticity:

```r
data <- generate_rigobon_data(
  n_obs = 1000,
  params = list(
    # Structural parameters
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    # Error structure
    alpha1 = -0.5, alpha2 = 1.0,
    # Regime structure
    regime_probs = c(0.4, 0.6),
    sigma2_regimes = c(1.0, 2.5)
  )
)
```

### `run_rigobon_estimation()`

Core estimation function:

```r
results <- run_rigobon_estimation(
  data = data,
  endog_var = "Y2",
  exog_vars = "Xk",
  regime_var = "regime",
  return_diagnostics = TRUE
)
```

## Examples

### Example 1: Two-Regime Model

```r
# Policy change example
params_policy <- list(
  beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
  beta2_0 = 1.0, beta2_1 = -1.0,
  alpha1 = -0.5, alpha2 = 1.0,
  regime_probs = c(0.5, 0.5),     # Equal split
  sigma2_regimes = c(1.0, 2.0)     # Variance doubles after policy
)

data_policy <- generate_rigobon_data(n_obs = 1000, params = params_policy)
results <- run_rigobon_analysis(data = data_policy)
```

### Example 2: Multiple Regimes

```r
# Quarterly volatility patterns
params_quarterly <- list(
  beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
  beta2_0 = 1.0, beta2_1 = -1.0,
  alpha1 = -0.5, alpha2 = 1.0,
  regime_probs = rep(0.25, 4),           # Four quarters
  sigma2_regimes = c(0.8, 1.0, 1.5, 1.2) # Different volatility by quarter
)

data_quarterly <- generate_rigobon_data(n_obs = 1200, params = params_quarterly)
results <- run_rigobon_analysis(data = data_quarterly)
```

### Example 3: Financial Crisis Application

```r
# Replicating financial contagion analysis
# Create crisis indicator: 1 = tranquil, 2 = crisis
my_financial_data$regime <- ifelse(
  my_financial_data$date %in% crisis_dates, 2, 1
)

# Run analysis
results <- run_rigobon_analysis(
  data = my_financial_data,
  regime_var = "regime",
  endog_var = "country2_returns",
  exog_vars = c("global_factor", "lag_returns")
)

# Check if crisis provides enough heteroskedasticity
cat("Variance ratio (crisis/tranquil):",
    results$diagnostics$heteroskedasticity_test$variance_ratio)
```

## Key Differences from Lewbel (2012)

| Aspect | Rigobon (2003) | Lewbel (2012) |
|--------|----------------|---------------|
| Heteroskedasticity source | Discrete regimes | Continuous functions |
| Typical Z variables | Regime dummies | X², X³, interactions |
| Best for | Clear regime changes | Smooth variations |
| Minimum requirements | 2 regimes | Continuous variation |

## Troubleshooting

### Weak Identification

If you get weak first-stage F-statistics:

1. **Check variance differences**: Ensure regimes have substantially different variances
2. **Verify regime assignment**: Make sure regimes are correctly identified
3. **Consider pooling regimes**: If you have many regimes with similar variances, pool them

### Failed Rank Condition

If the rank condition fails:

1. **Examine relative variances**: The relative importance of shocks must change across regimes
2. **Check for proportional changes**: If all variances change proportionally, identification fails
3. **Try different regime definitions**: Alternative ways to define regimes may work better

## References

Rigobon, R. (2003). Identification through heteroskedasticity. *The Review of Economics and Statistics*, 85(4), 777-792.

## See Also

- [Package vignette](../vignettes/rigobon-method.Rmd) for theoretical background and detailed examples
- [Main usage guide](USAGE.md) for general package usage
- [Development guide](DEVELOPMENT.md) for contributing to the package
