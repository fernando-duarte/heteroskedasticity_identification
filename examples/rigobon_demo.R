# Rigobon (2003) Regime-Based Identification Demo
#
# This example demonstrates how to use Rigobon's heteroskedasticity-based
# identification method, which uses discrete regime indicators as instruments.

library(hetid)

# Example 1: Basic two-regime case (e.g., pre/post policy change)
# ----------------------------------------------------------------
cat("Example 1: Two-regime identification\n")
cat("====================================\n\n")

# Set parameters for data generation
params_2reg <- list(
  beta1_0 = 0.5,      # Intercept in structural equation
  beta1_1 = 1.5,      # Coefficient on X
  gamma1 = -0.8,      # TRUE endogenous parameter (what we want to estimate)
  beta2_0 = 1.0,      # First-stage intercept
  beta2_1 = -1.0,     # First-stage X coefficient
  alpha1 = -0.5,      # Factor loading for equation 1
  alpha2 = 1.0,       # Factor loading for equation 2
  regime_probs = c(0.4, 0.6),  # 40% in regime 1, 60% in regime 2
  sigma2_regimes = c(1.0, 2.5) # Variance 2.5x higher in regime 2
)

# Generate data
set.seed(42)
data_2reg <- generate_rigobon_data(n_obs = 1000, params = params_2reg)

# Quick look at the data
cat("Data structure:\n")
str(data_2reg)
cat("\nRegime distribution:\n")
table(data_2reg$regime)

# Run Rigobon estimation
results_2reg <- run_rigobon_estimation(data_2reg, return_diagnostics = TRUE)

# Display results
cat("\n\nEstimation Results:\n")
cat("-------------------\n")
cat(sprintf("True gamma1: %.3f\n", params_2reg$gamma1))
cat(sprintf("OLS estimate: %.3f (SE: %.3f)\n",
            results_2reg$ols$estimates["gamma1"],
            results_2reg$ols$se["gamma1"]))
cat(sprintf("Rigobon 2SLS: %.3f (SE: %.3f)\n",
            results_2reg$tsls$estimates["gamma1"],
            results_2reg$tsls$se["gamma1"]))

cat("\nFirst-stage F-statistics:\n")
print(results_2reg$first_stage_F)

cat("\nHeteroskedasticity test:\n")
cat(sprintf("p-value: %.4f - %s\n",
            results_2reg$heteroskedasticity_test$p_value,
            results_2reg$heteroskedasticity_test$interpretation))

# Example 2: Three-regime case (e.g., low/medium/high volatility periods)
# -----------------------------------------------------------------------
cat("\n\nExample 2: Three-regime identification\n")
cat("======================================\n\n")

params_3reg <- list(
  beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
  beta2_0 = 1.0, beta2_1 = -1.0,
  alpha1 = -0.5, alpha2 = 1.0,
  regime_probs = c(0.3, 0.4, 0.3),    # 30%, 40%, 30% distribution
  sigma2_regimes = c(0.5, 1.0, 2.0)   # Increasing variance
)

data_3reg <- generate_rigobon_data(n_obs = 1500, params = params_3reg)
results_3reg <- run_rigobon_estimation(data_3reg)

cat("Three-regime results:\n")
cat(sprintf("True gamma1: %.3f\n", params_3reg$gamma1))
cat(sprintf("OLS estimate: %.3f (SE: %.3f)\n",
            results_3reg$ols$estimates["gamma1"],
            results_3reg$ols$se["gamma1"]))
cat(sprintf("Rigobon 2SLS: %.3f (SE: %.3f)\n",
            results_3reg$tsls$estimates["gamma1"],
            results_3reg$tsls$se["gamma1"]))

# Example 3: Using pre-existing regime indicators
# ------------------------------------------------
cat("\n\nExample 3: Using your own regime indicators\n")
cat("============================================\n\n")

# Suppose you have data with existing regime indicators
# (e.g., from market conditions, policy periods, etc.)
n <- 1000
existing_data <- data.frame(
  Y1 = rnorm(n),
  Y2 = rnorm(n),
  Xk = rnorm(n, mean = 2),
  regime = sample(c("bear", "normal", "bull"), n,
                  replace = TRUE, prob = c(0.2, 0.6, 0.2))
)

# Convert string regimes to numeric if needed
existing_data$regime <- as.numeric(factor(existing_data$regime))

# Generate first-stage errors to simulate endogeneity
existing_data$Y2 <- 1.0 - 1.0 * existing_data$Xk +
  rnorm(n, sd = ifelse(existing_data$regime == 1, 0.5,
                       ifelse(existing_data$regime == 2, 1.0, 2.0)))

# Create structural equation with endogeneity
existing_data$Y1 <- 0.5 + 1.5 * existing_data$Xk - 0.8 * existing_data$Y2 +
  0.5 * rnorm(n)  # Some correlation with Y2 errors creates endogeneity

# Run Rigobon estimation
results_existing <- run_rigobon_estimation(existing_data)
cat("Results with existing regime indicators:\n")
cat(sprintf("OLS estimate: %.3f\n", results_existing$ols$estimates["gamma1"]))
cat(sprintf("Rigobon 2SLS: %.3f\n", results_existing$tsls$estimates["gamma1"]))

# Example 4: Quick demo function
# ------------------------------
cat("\n\nExample 4: Using the built-in demo function\n")
cat("===========================================\n")

# Run the comprehensive demo (silent mode)
demo_results <- run_rigobon_demo(n_obs = 500, n_regimes = 2, verbose = FALSE)

# Display comparison table
cat("\nComparison of methods:\n")
print(demo_results$comparison, row.names = FALSE)

# The demo returns all results for further analysis
cat("\nDemo returns: data, params, results, and comparison table\n")
