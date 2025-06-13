# Testing the hetid Package in RStudio

This guide shows you how to test the `hetid` package thoroughly in RStudio.

## Quick Start

1. **Open RStudio** in the package directory (`heteroskedasticity/`)

2. **Run the test script**:
   ```r
   source("test_hetid_package.R")
   ```

## Manual Testing Steps

### 1. Load the Package

```r
library(devtools)
load_all()  # Loads all package functions
```

### 2. Check Package Structure

```r
# Full package check (takes ~1 minute)
check()

# Quick check of available functions
ls("package:hetid")
```

### 3. Test Core Functionality

```r
# Set parameters
params <- list(
  beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
  beta2_0 = 1.0, beta2_1 = -1.0,
  alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
)

# Generate data
data <- generate_lewbel_data(1000, params)
head(data)

# Verify assumptions
verify_lewbel_assumptions(params)

# Run single simulation
sim_params <- c(params, list(
  sample_size = 1000,
  tau_set_id = 0.2,
  bootstrap_reps = 50
))

result <- run_single_lewbel_simulation(1, sim_params)
print(result)
```

### 4. Run Quick Demo

```r
# Quick demo with 100 simulations
demo_results <- run_lewbel_demo(num_simulations = 100)

# Note: There's a bug in run_lewbel_demo, use this workaround:
config <- create_default_config(num_simulations = 100)
config$n_reps_by_n <- 50
config$n_reps_by_delta <- 50
config$bootstrap_demo_size <- 3

demo_results <- run_lewbel_monte_carlo(
  config = config,
  run_sample_analysis = FALSE,
  run_sensitivity = FALSE,
  verbose = TRUE
)
```

### 5. Visualize Results

```r
# If you ran multiple simulations, create plots
if (exists("results") && nrow(results) > 0) {
  par(mfrow = c(1, 2))

  # OLS estimates
  hist(results$ols_gamma1, main = "OLS Estimates",
       xlab = "Estimate", col = "lightblue")
  abline(v = -0.8, col = "red", lwd = 2)  # True value

  # TSLS estimates
  hist(results$tsls_gamma1, main = "TSLS (Lewbel) Estimates",
       xlab = "Estimate", col = "lightgreen")
  abline(v = -0.8, col = "red", lwd = 2)  # True value

  par(mfrow = c(1, 1))
}
```

## Interactive Testing in RStudio

### Using the Console

Test individual functions interactively:

```r
# Test data generation
test_data <- generate_lewbel_data(500, params)
str(test_data)

# Test bounds calculation
bounds <- calculate_lewbel_bounds(test_data, tau = 0.1)
bounds$bounds

# Test with different parameters
params$gamma1 <- -0.5  # Change true value
new_data <- generate_lewbel_data(1000, params)
```

### Using the Environment Pane

- Watch variables appear in the Environment pane as you run code
- Click on data frames to view them in the data viewer
- Use `View(results)` to see simulation results in a spreadsheet format

### Using Plots Pane

All plots will appear in the Plots pane. You can:
- Zoom in/out
- Export as PDF or PNG
- Navigate through plot history

## Debugging Tips

1. **If functions aren't found**: Make sure you ran `load_all()`

2. **If you get errors about missing packages**:
   ```r
   install.packages(c("AER", "boot", "dplyr", "furrr", "future",
                      "ggplot2", "magrittr", "purrr", "tidyr"))
   ```

3. **To see function documentation**:
   ```r
   ?generate_lewbel_data
   ?run_single_lewbel_simulation
   ?calculate_lewbel_bounds
   ```

4. **To view function source code**:
   ```r
   View(generate_lewbel_data)
   ```

## Performance Testing

For larger simulations:

```r
# Time a simulation
system.time({
  results <- run_lewbel_monte_carlo(
    create_default_config(num_simulations = 1000),
    run_bootstrap_demo = FALSE,
    generate_plots = FALSE
  )
})
```

## Clean Up

After testing:

```r
# Remove all objects
rm(list = ls())

# Unload package
detach("package:hetid", unload = TRUE)
```

## Expected Results

When everything works correctly, you should see:
- No errors when loading the package
- Data generation creates 6 variables: Y1, Y2, Xk, Z, epsilon1, epsilon2
- OLS estimates show bias due to endogeneity
- TSLS (Lewbel) estimates are closer to the true value
- First-stage F-statistics are very large (>10,000)
- Coverage rates around 85-95%

## Next Steps

Once basic testing works:
1. Run the full Monte Carlo simulation
2. Test the visualization functions
3. Try different parameter configurations
4. Test edge cases (small samples, weak instruments, etc.)
