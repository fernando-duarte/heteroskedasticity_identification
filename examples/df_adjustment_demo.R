# Demonstration of degrees of freedom adjustment in hetid
# This script shows how to use the new df_adjust parameter

# Load the package in development mode
if (file.exists("DESCRIPTION")) {
  library(devtools)
  load_all()
} else {
  library(hetid)
}

# Create configuration
config <- create_default_config(
  num_simulations = 500,
  main_sample_size = 100  # Small sample to see the difference
)

# 1. Run simulation with asymptotic SEs (default)
cat("1. Running with asymptotic standard errors (default)...\n")
result_asymp <- run_single_lewbel_simulation(
  sim_id = 1,
  params = list(
    sample_size = 100,
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2,
    tau_set_id = 0,
    bootstrap_reps = 50
  ),
  df_adjust = "asymptotic"
)

cat("OLS SE (asymptotic):", result_asymp$ols_se, "\n")
cat("TSLS SE (asymptotic):", result_asymp$tsls_se, "\n\n")

# 2. Run simulation with finite sample SEs
cat("2. Running with finite sample standard errors...\n")
result_finite <- run_single_lewbel_simulation(
  sim_id = 1,
  params = list(
    sample_size = 100,
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2,
    tau_set_id = 0,
    bootstrap_reps = 50
  ),
  df_adjust = "finite"
)

cat("OLS SE (finite):", result_finite$ols_se, "\n")
cat("TSLS SE (finite):", result_finite$tsls_se, "\n\n")

# 3. Calculate the adjustment factor
n <- 100
k <- 3  # intercept + 2 regressors
adjustment_factor <- sqrt(n / (n - k))
cat("Expected adjustment factor:", adjustment_factor, "\n")
cat("Actual OLS SE ratio (finite/asymptotic):", result_finite$ols_se / result_asymp$ols_se, "\n")
cat("Actual TSLS SE ratio (finite/asymptotic):", result_finite$tsls_se / result_asymp$tsls_se, "\n\n")

# 4. Compare critical values
crit_asymp <- get_critical_value(n, k, df_adjust = "asymptotic")
crit_finite <- get_critical_value(n, k, df_adjust = "finite")
cat("Critical value (asymptotic):", crit_asymp, "\n")
cat("Critical value (finite):", crit_finite, "\n")
cat("Critical value ratio:", crit_finite / crit_asymp, "\n\n")

# 5. Run comparison using the helper function
cat("5. Running full comparison of DF adjustments...\n")
config_small <- create_default_config(
  num_simulations = 100,
  main_sample_size = 50  # Very small sample
)

comparison_results <- compare_df_adjustments(config_small, verbose = TRUE)

# 6. Demonstrate with Stata comparison
cat("\n6. For Stata comparison, use asymptotic (the default):\n")
cat("This matches Stata's ivreg2h which reports asymptotic SEs\n")