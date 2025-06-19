#' @title Example: GMM Estimation for Prono (2014) GARCH-Based Identification
#' @description This example demonstrates how to use the GMM implementation
#' for Prono's GARCH-based heteroskedasticity identification method.
#' @author Your Name
#' @date 2024

# Load required packages
library(heteroskedasticity)
library(gmm)
library(ggplot2)

# Set seed for reproducibility
set.seed(42)

cat("================================================\n")
cat("GMM Estimation for Prono (2014) Model\n")
cat("================================================\n\n")

# -----------------------------------------------------------------------------
# 1. Generate Example Data with GARCH Heteroskedasticity
# -----------------------------------------------------------------------------

cat("1. Generating time-series data with GARCH heteroskedasticity...\n")

# Configuration for asset pricing application (returns in percent)
config <- create_prono_config(
  n = 500,
  k = 1,
  gamma1 = 1.0,  # True portfolio beta
  garch_params = list(
    omega = 0.2,   # Unconditional variance ~2% (weekly returns)
    alpha = 0.1,   # ARCH effect
    beta = 0.85    # GARCH persistence
  ),
  sigma1 = 1.5,    # Portfolio idiosyncratic risk
  rho = 0.3        # Endogeneity (correlation between errors)
)

# Generate data
data <- generate_prono_data(
  n = config$n,
  beta1 = config$beta1,
  beta2 = config$beta2,
  gamma1 = config$gamma1,
  k = config$k,
  garch_params = config$garch_params,
  sigma1 = config$sigma1,
  rho = config$rho,
  seed = config$seed
)

# Show data structure
cat("\nData structure:\n")
str(data[, c("Y1", "Y2", "X1", "sigma2_sq")])

# -----------------------------------------------------------------------------
# 2. Visualize GARCH Heteroskedasticity
# -----------------------------------------------------------------------------

cat("\n2. Visualizing time-varying volatility...\n")

# Create time series plots
data$time <- seq_len(nrow(data))

# Plot returns and conditional volatility
p1 <- ggplot(data, aes(x = time)) +
  geom_line(aes(y = Y2), alpha = 0.7) +
  labs(
    title = "Market Returns (Y2) Over Time",
    x = "Time",
    y = "Return (%)"
  ) +
  theme_minimal()

p2 <- ggplot(data, aes(x = time)) +
  geom_line(aes(y = sqrt(sigma2_sq)), color = "red") +
  labs(
    title = "Conditional Volatility (GARCH)",
    x = "Time",
    y = "Volatility (%)"
  ) +
  theme_minimal()

# Display plots
library(gridExtra)
grid.arrange(p1, p2, ncol = 1)

# -----------------------------------------------------------------------------
# 3. Compare Estimation Methods
# -----------------------------------------------------------------------------

cat("\n3. Comparing estimation methods...\n")

# Method 1: OLS (biased due to endogeneity)
ols_model <- lm(Y1 ~ Y2 + X1, data = data)
ols_est <- coef(ols_model)["Y2"]
ols_se <- summary(ols_model)$coefficients["Y2", "Std. Error"]

# Method 2: Prono 2SLS (using run_single_prono_simulation)
prono_2sls <- run_single_prono_simulation(config, return_details = TRUE)
tsls_est <- prono_2sls$gamma1_iv
tsls_se <- prono_2sls$se_iv

# Method 3: Prono GMM
cat("\nFitting GARCH model and running GMM...\n")
prono_gmm_result <- prono_gmm(
  data,
  gmm_type = "twoStep",
  verbose = FALSE
)
gmm_est <- prono_gmm_result$coefficients["gamma1"]
gmm_se <- sqrt(diag(prono_gmm_result$vcov))["gamma1"]

# Create comparison table
comparison <- data.frame(
  Method = c("OLS (Biased)", "Prono 2SLS", "Prono GMM"),
  Estimate = c(ols_est, tsls_est, gmm_est),
  StdError = c(ols_se, tsls_se, gmm_se),
  Bias = c(ols_est, tsls_est, gmm_est) - config$gamma1,
  RelativeBias = ((c(ols_est, tsls_est, gmm_est) - config$gamma1) /
                   abs(config$gamma1)) * 100
)

cat("\nEstimation results (True gamma1 =", config$gamma1, "):\n")
print(comparison, digits = 4)

# Report efficiency gain
eff_gain <- (tsls_se^2 / gmm_se^2 - 1) * 100
cat(sprintf("\nEfficiency gain of GMM over 2SLS: %.1f%%\n", eff_gain))

# -----------------------------------------------------------------------------
# 4. Diagnostic Tests
# -----------------------------------------------------------------------------

cat("\n4. Diagnostic tests...\n")

# First-stage F-statistic
cat(sprintf("\nFirst-stage F-statistic: %.2f\n", prono_gmm_result$first_stage_F))
if (prono_gmm_result$first_stage_F < 10) {
  cat("WARNING: Weak instrument (F < 10)\n")
}

# J-test for overidentification
if (!is.null(prono_gmm_result$J_test)) {
  cat("\nJ-test for overidentifying restrictions:\n")
  cat(sprintf("  J-statistic: %.4f\n", prono_gmm_result$J_test$J_stat))
  cat(sprintf("  p-value: %.4f\n", prono_gmm_result$J_test$p_value))

  if (prono_gmm_result$J_test$p_value > 0.05) {
    cat("  Interpretation: Fail to reject null (moment conditions valid)\n")
  } else {
    cat("  Interpretation: Reject null (possible model misspecification)\n")
  }
}

# GARCH diagnostics
if (!is.null(prono_gmm_result$garch_fit)) {
  cat("\nGARCH model diagnostics:\n")
  garch_coef <- coef(prono_gmm_result$garch_fit)
  cat(sprintf("  omega: %.4f\n", garch_coef["omega"]))
  cat(sprintf("  alpha: %.4f\n", garch_coef["alpha1"]))
  cat(sprintf("  beta:  %.4f\n", garch_coef["beta1"]))
  cat(sprintf("  Persistence (alpha + beta): %.4f\n",
              garch_coef["alpha1"] + garch_coef["beta1"]))
}

# -----------------------------------------------------------------------------
# 5. Alternative GMM Specifications
# -----------------------------------------------------------------------------

cat("\n5. Comparing different GMM specifications...\n")

# Try different variance-covariance specifications
gmm_hac <- prono_gmm(data, vcov_type = "HAC", fit_garch = FALSE, verbose = FALSE)
gmm_iid <- prono_gmm(data, vcov_type = "iid", fit_garch = FALSE, verbose = FALSE)

# Compare standard errors
se_comparison <- data.frame(
  VCov_Type = c("HAC", "IID"),
  Estimate = c(
    gmm_hac$coefficients["gamma1"],
    gmm_iid$coefficients["gamma1"]
  ),
  StdError = c(
    sqrt(diag(gmm_hac$vcov))["gamma1"],
    sqrt(diag(gmm_iid$vcov))["gamma1"]
  )
)

cat("\nVariance-covariance comparison:\n")
print(se_comparison, digits = 4)

# -----------------------------------------------------------------------------
# 6. Monte Carlo Evidence
# -----------------------------------------------------------------------------

cat("\n6. Running small Monte Carlo simulation...\n")

# Run 100 simulations
mc_results <- run_prono_monte_carlo(
  config = config,
  n_sims = 100,
  parallel = FALSE,
  progress = FALSE
)

# Summarize results
mc_summary <- data.frame(
  Method = c("OLS", "Prono IV"),
  Mean_Estimate = c(mean(mc_results$gamma1_ols), mean(mc_results$gamma1_iv)),
  Mean_Bias = c(mean(mc_results$bias_ols), mean(mc_results$bias_iv)),
  RMSE = c(
    sqrt(mean(mc_results$bias_ols^2)),
    sqrt(mean(mc_results$bias_iv^2))
  ),
  Mean_F_Stat = c(NA, mean(mc_results$f_stat))
)

cat("\nMonte Carlo results (100 simulations):\n")
print(mc_summary, digits = 4)

# -----------------------------------------------------------------------------
# 7. Sensitivity to GARCH Specification
# -----------------------------------------------------------------------------

cat("\n7. Sensitivity to GARCH order...\n")

# Try GARCH(2,1)
prono_gmm_21 <- prono_gmm(
  data,
  garch_order = c(2, 1),
  verbose = FALSE
)

# Try GARCH(1,2)
prono_gmm_12 <- prono_gmm(
  data,
  garch_order = c(1, 2),
  verbose = FALSE
)

# Compare results
garch_comparison <- data.frame(
  GARCH_Order = c("(1,1)", "(2,1)", "(1,2)"),
  Estimate = c(
    prono_gmm_result$coefficients["gamma1"],
    prono_gmm_21$coefficients["gamma1"],
    prono_gmm_12$coefficients["gamma1"]
  ),
  StdError = c(
    sqrt(diag(prono_gmm_result$vcov))["gamma1"],
    sqrt(diag(prono_gmm_21$vcov))["gamma1"],
    sqrt(diag(prono_gmm_12$vcov))["gamma1"]
  )
)

cat("\nGARCH order sensitivity:\n")
print(garch_comparison, digits = 4)

# -----------------------------------------------------------------------------
# 8. Summary
# -----------------------------------------------------------------------------

cat("\n================================================\n")
cat("Summary of Prono GMM Analysis\n")
cat("================================================\n")

cat("\n1. Key findings:\n")
cat(sprintf("   - True gamma1: %.4f\n", config$gamma1))
cat(sprintf("   - OLS estimate (biased): %.4f\n", ols_est))
cat(sprintf("   - 2SLS estimate: %.4f\n", tsls_est))
cat(sprintf("   - GMM estimate: %.4f\n", gmm_est))

cat("\n2. GARCH-based identification:\n")
cat("   - Exploits time-varying conditional heteroskedasticity\n")
cat("   - Suitable for financial time series\n")
cat("   - Requires sufficient volatility clustering\n")

cat("\n3. Advantages of GMM:\n")
cat(sprintf("   - Efficiency gain over 2SLS: %.1f%%\n", eff_gain))
cat("   - Optimal weighting of moment conditions\n")
cat("   - Robust to different variance specifications\n")

cat("\n4. Practical considerations:\n")
cat("   - Check GARCH model convergence\n")
cat("   - Verify first-stage strength (F > 10)\n")
cat("   - Consider alternative GARCH specifications\n")

cat("\nAnalysis complete.\n")
