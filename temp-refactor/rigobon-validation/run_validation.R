# Test script to validate the refactored rigobon_moment_conditions function

# Load necessary libraries
if (!requireNamespace("testthat", quietly = TRUE)) {
  install.packages("testthat")
}
library(testthat)

# Source the functions to be tested
source("temp-refactor/rigobon-validation/original_functions.R")
source("temp-refactor/rigobon-validation/unified_function.R")

# --- 1. Test Data and Parameters Setup ---
# Create a reproducible testing environment
set.seed(42)
n_obs <- 100
test_data <- data.frame(
  Y1 = rnorm(n_obs),
  Y2 = rnorm(n_obs),
  Xk = rnorm(n_obs),
  regime = sample(1:3, n_obs, replace = TRUE) # 3 regimes
)

# Define parameters for the models
# For a model with an intercept and one exogenous variable, k=2
k <- 2
# Triangular system parameters: beta1 (2), gamma1 (1), beta2 (2) -> Total 5
theta_tri <- c(beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8, beta2_0 = 1.0, beta2_1 = -1.0)
# Simultaneous system adds gamma2 -> Total 6
theta_sim <- c(theta_tri, gamma2 = 0.5)


# --- 2. Define Test Cases ---
test_that("Refactored function matches original for TRIANGULAR system", {
  # Run both the original and the new unified function
  original_moments <- rigobon_triangular_moments(
    theta = theta_tri,
    data = test_data,
    y1_var = "Y1", y2_var = "Y2", x_vars = "Xk",
    regime_var = "regime", add_intercept = TRUE
  )

  unified_moments <- rigobon_moment_conditions(
    theta = theta_tri,
    data = test_data,
    system = "triangular",
    y1_var = "Y1", y2_var = "Y2", x_vars = "Xk",
    regime_var = "regime", add_intercept = TRUE
  )

  # Assert that the outputs are identical
  expect_equal(original_moments, unified_moments,
               label = "Triangular system moments should be identical")
})

test_that("Refactored function matches original for SIMULTANEOUS system", {
  # Run both the original and the new unified function
  original_moments <- rigobon_simultaneous_moments(
    theta = theta_sim,
    data = test_data,
    y1_var = "Y1", y2_var = "Y2", x_vars = "Xk",
    regime_var = "regime", add_intercept = TRUE
  )

  # The main unified function is now correct, so we test it directly.
  unified_moments <- rigobon_moment_conditions(
    theta = theta_sim,
    data = test_data,
    system = "simultaneous",
    y1_var = "Y1", y2_var = "Y2", x_vars = "Xk",
    regime_var = "regime", add_intercept = TRUE
  )

  # Assert that the outputs are identical
  expect_equal(original_moments, unified_moments,
               label = "Simultaneous system moments should be identical")
})

# --- 3. Run Tests ---
cat("\nRunning validation tests for refactored Rigobon moment functions...\n")
# Use a "stop" reporter to halt on the first failure
test_file("temp-refactor/rigobon-validation/run_validation.R", reporter = "stop")
cat("\n Validation successful: The refactored function is equivalent to the originals.\n")
