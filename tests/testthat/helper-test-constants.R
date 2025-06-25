# Test Constants for hetid Package
# This file contains constant values used across test files

# Default column names expected in various data structures
lewbel_data_cols <- c("Y1", "Y2", "Xk", "Z", "epsilon1", "epsilon2")
rigobon_data_cols <- c("Y1", "Y2", "Xk", "regime", "epsilon1", "epsilon2")

# Common test parameters
test_gamma1_true <- -0.8
test_tolerance_tight <- 0.1
test_tolerance_normal <- 0.2
test_tolerance_loose <- 0.5

# Sample sizes for different test levels
n_tiny <- 10
n_small <- 50
n_medium <- 100
n_large <- 500

# Simulation counts for different test levels
n_sim_tiny <- 5
n_sim_small <- 10
n_sim_medium <- 50
n_sim_large <- 100

# Bootstrap replication counts
n_boot_tiny <- 5
n_boot_small <- 10
n_boot_medium <- 20
n_boot_large <- 100

# Common seeds for reproducibility
seed_default <- 123
seed_alt <- 42
seed_series <- c(123, 456, 789, 111, 222)

# Weak instrument threshold
weak_f_threshold <- 10

# Significance levels
alpha_default <- 0.05
alpha_conservative <- 0.01

# Heteroskedasticity strength values
delta_het_none <- 0
delta_het_weak <- 0.1
delta_het_moderate <- 1.2
delta_het_strong <- 3.0
delta_het_extreme <- 5.0

# Parameter sets for edge cases
params_no_het <- list(delta_het = 0, alpha1 = 0, alpha2 = 0)
params_weak_het <- list(delta_het = 0.01, alpha1 = 0.01, alpha2 = 0.01)
params_extreme_het <- list(delta_het = 5.0, alpha1 = 2.0, alpha2 = 2.0)

# Expected error message patterns
error_patterns <- list(
  missing_var = "not found in data|Missing required variables",
  weak_id = "weak identification|Weak instruments",
  invalid_param = "must be|invalid",
  package_missing = "is required but not installed",
  convergence = "did not converge|convergence"
)
