#' @title Example: GMM Estimation for Rigobon (2003) Regime-Based Identification
#' @description This example demonstrates how to use the GMM implementation
#' for Rigobon's regime-based heteroskedasticity identification method.
#' @author Your Name
#' @date 2024

# Load required packages
library(heteroskedasticity)
library(gmm)
library(ggplot2)

# Set seed for reproducibility
set.seed(42)

cat("================================================\n")
cat("GMM Estimation for Rigobon (2003) Model\n")
cat("================================================\n\n")

# -----------------------------------------------------------------------------
# 1. Generate Example Data with Regime Heteroskedasticity
# -----------------------------------------------------------------------------

cat("1. Generating data with regime-based heteroskedasticity...\n")

# Parameters for data generation
params <- list(
  # Structural parameters
  beta1_0 = 0.5,    # Intercept in first equation
  beta1_1 = 1.5,    # Coefficient on X in first equation
  gamma1 = -0.8,    # Coefficient on endogenous Y2 (true parameter of interest)
  beta2_0 = 1.0,    # Intercept in second equation
  beta2_1 = -1.0,   # Coefficient on X in second equation

  # Error distribution parameters
  alpha1 = -0.5,    # Common factor loading for error 1
  alpha2 = 1.0,     # Common factor loading for error 2

  # Regime parameters
  regime_probs = c(0.4, 0.6),     # Probability of each regime
  sigma2_regimes = c(1.0, 3.0)    # Error variance in each regime (heteroskedasticity)
)

# Generate data
n_obs <- 1000
data <- generate_rigobon_data(n_obs, params)

# Show data structure
cat("\nData structure:\n")
str(data)

# Show regime distribution
cat("\nRegime distribution:\n")
print(table(data$regime) / n_obs)

# -----------------------------------------------------------------------------
# 2. Visualize Heteroskedasticity Across Regimes
# -----------------------------------------------------------------------------

cat("\n2. Visualizing heteroskedasticity across regimes...\n")

# Estimate reduced-form residuals
e2_hat <- residuals(lm(Y2 ~ Xk, data = data))
data$e2_hat <- e2_hat

# Plot residuals by regime
p1 <- ggplot(data, aes(x = factor(regime), y = e2_hat^2)) +
  geom_boxplot(fill = "lightblue") +
  geom_jitter(alpha = 0.1, width = 0.2) +
  labs(
    title = "Heteroskedasticity Across Regimes",
    x = "Regime",
    y = "Squared Residuals",
    subtitle = "Evidence of different error variances across regimes"
  ) +
  theme_minimal()

print(p1)

# -----------------------------------------------------------------------------
# 3. Compare Different Estimation Methods
# -----------------------------------------------------------------------------

cat("\n3. Comparing estimation methods...\n")

# Method 1: OLS (biased due to endogeneity)
ols_model <- lm(Y1 ~ Y2 + Xk, data = data)
ols_est <- coef(ols_model)["Y2"]
ols_se <- summary(ols_model)$coefficients["Y2", "Std. Error"]

# Method 2: Rigobon 2SLS
rigobon_2sls <- run_rigobon_estimation(data, return_diagnostics = TRUE)
tsls_est <- rigobon_2sls$tsls$estimates["gamma1"]
tsls_se <- rigobon_2sls$tsls$se["gamma1"]

# Method 3: Rigobon GMM (triangular system)
rigobon_gmm_tri <- rigobon_gmm(
  data,
  system = "triangular",
  gmm_type = "twoStep",
  verbose = FALSE
)
gmm_tri_est <- rigobon_gmm_tri$coefficients["gamma1"]
gmm_tri_se <- sqrt(diag(rigobon_gmm_tri$vcov))["gamma1"]

# Create comparison table
comparison <- data.frame(
  Method = c("OLS (Biased)", "Rigobon 2SLS", "Rigobon GMM"),
  Estimate = c(ols_est, tsls_est, gmm_tri_est),
  StdError = c(ols_se, tsls_se, gmm_tri_se),
  Bias = c(ols_est, tsls_est, gmm_tri_est) - params$gamma1,
  RelativeBias = ((c(ols_est, tsls_est, gmm_tri_est) - params$gamma1) /
                   abs(params$gamma1)) * 100
)

cat("\nEstimation results (True gamma1 =", params$gamma1, "):\n")
print(comparison, digits = 4)

# Report efficiency gain
eff_gain <- (tsls_se^2 / gmm_tri_se^2 - 1) * 100
cat(sprintf("\nEfficiency gain of GMM over 2SLS: %.1f%%\n", eff_gain))

# -----------------------------------------------------------------------------
# 4. Test Overidentification
# -----------------------------------------------------------------------------

cat("\n4. Testing overidentification...\n")

if (!is.null(rigobon_gmm_tri$J_test)) {
  cat("\nJ-test for overidentifying restrictions:\n")
  cat(sprintf("  J-statistic: %.4f\n", rigobon_gmm_tri$J_test$J_stat))
  cat(sprintf("  p-value: %.4f\n", rigobon_gmm_tri$J_test$p_value))
  cat(sprintf("  Degrees of freedom: %d\n", rigobon_gmm_tri$J_test$df))

  if (rigobon_gmm_tri$J_test$p_value > 0.05) {
    cat("  Interpretation: Fail to reject null (moment conditions valid)\n")
  } else {
    cat("  Interpretation: Reject null (possible model misspecification)\n")
  }
}

# -----------------------------------------------------------------------------
# 5. Simultaneous Equations System
# -----------------------------------------------------------------------------

cat("\n5. Estimating simultaneous equations system...\n")

# Generate data with more regimes for simultaneous system
params_sim <- params
params_sim$regime_probs <- c(0.3, 0.4, 0.3)
params_sim$sigma2_regimes <- c(1.0, 2.0, 3.0)

data_sim <- generate_rigobon_data(n_obs, params_sim)

# Estimate simultaneous system
rigobon_gmm_sim <- rigobon_gmm(
  data_sim,
  system = "simultaneous",
  gmm_type = "twoStep",
  verbose = FALSE
)

cat("\nSimultaneous system estimates:\n")
cat(sprintf("  gamma1 = %.4f (SE: %.4f)\n",
            rigobon_gmm_sim$coefficients["gamma1"],
            sqrt(diag(rigobon_gmm_sim$vcov))["gamma1"]))
cat(sprintf("  gamma2 = %.4f (SE: %.4f)\n",
            rigobon_gmm_sim$coefficients["gamma2"],
            sqrt(diag(rigobon_gmm_sim$vcov))["gamma2"]))

# Check identification condition
gamma1_sim <- rigobon_gmm_sim$coefficients["gamma1"]
gamma2_sim <- rigobon_gmm_sim$coefficients["gamma2"]
cat(sprintf("\n  gamma1 * gamma2 = %.4f (must not equal 1)\n",
            gamma1_sim * gamma2_sim))

# -----------------------------------------------------------------------------
# 6. Different GMM Specifications
# -----------------------------------------------------------------------------

cat("\n6. Comparing different GMM specifications...\n")

# Two-step GMM (default)
gmm_twostep <- rigobon_gmm(data, gmm_type = "twoStep", verbose = FALSE)

# Iterative GMM
gmm_iter <- rigobon_gmm(data, gmm_type = "iterative", verbose = FALSE)

# CUE (Continuously Updated Estimator)
gmm_cue <- rigobon_gmm(data, gmm_type = "cue", verbose = FALSE)

# Compare results
gmm_comparison <- data.frame(
  GMM_Type = c("Two-step", "Iterative", "CUE"),
  Estimate = c(
    gmm_twostep$coefficients["gamma1"],
    gmm_iter$coefficients["gamma1"],
    gmm_cue$coefficients["gamma1"]
  ),
  StdError = c(
    sqrt(diag(gmm_twostep$vcov))["gamma1"],
    sqrt(diag(gmm_iter$vcov))["gamma1"],
    sqrt(diag(gmm_cue$vcov))["gamma1"]
  )
)

cat("\nGMM type comparison:\n")
print(gmm_comparison, digits = 4)

# -----------------------------------------------------------------------------
# 7. Bootstrap Inference
# -----------------------------------------------------------------------------

cat("\n7. Bootstrap inference for GMM estimates...\n")

# Bootstrap function
bootstrap_rigobon_gmm <- function(data, indices) {
  boot_data <- data[indices, ]

  # Check if we have enough regimes in bootstrap sample
  if (length(unique(boot_data$regime)) < 2) {
    return(NA)
  }

  tryCatch({
    result <- rigobon_gmm(boot_data, verbose = FALSE)
    return(result$coefficients["gamma1"])
  }, error = function(e) {
    return(NA)
  })
}

# Run bootstrap
library(boot)
boot_results <- boot(
  data = data,
  statistic = bootstrap_rigobon_gmm,
  R = 100  # Use more replications in practice
)

# Remove NA values
valid_boots <- boot_results$t[!is.na(boot_results$t)]

if (length(valid_boots) > 10) {
  # Bootstrap confidence interval
  boot_ci <- quantile(valid_boots, c(0.025, 0.975))

  cat("\nBootstrap results:\n")
  cat(sprintf("  Bootstrap mean: %.4f\n", mean(valid_boots)))
  cat(sprintf("  Bootstrap SE: %.4f\n", sd(valid_boots)))
  cat(sprintf("  95%% Bootstrap CI: [%.4f, %.4f]\n", boot_ci[1], boot_ci[2]))
}

# -----------------------------------------------------------------------------
# 8. Diagnostic Plots
# -----------------------------------------------------------------------------

cat("\n8. Creating diagnostic plots...\n")

# Extract moment conditions at estimated parameters
theta_hat <- rigobon_gmm_tri$gmm_result$coefficients
moments <- rigobon_triangular_moments(theta_hat, data)

# Plot average moments
avg_moments <- colMeans(moments)
moment_names <- c(
  paste0("E[X*e1]_", 1:2),
  paste0("E[X*e2]_", 1:2),
  paste0("E[Z", 1:length(unique(data$regime)), "*e1*e2]")
)

p2 <- ggplot(data.frame(
  Moment = factor(moment_names, levels = moment_names),
  Value = avg_moments
), aes(x = Moment, y = Value)) +
  geom_col(fill = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Sample Average of GMM Moment Conditions",
    subtitle = "Values should be close to zero at optimal parameters",
    y = "Average Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)

# -----------------------------------------------------------------------------
# 9. Summary
# -----------------------------------------------------------------------------

cat("\n================================================\n")
cat("Summary of Rigobon GMM Analysis\n")
cat("================================================\n")

cat("\n1. Key findings:\n")
cat(sprintf("   - True gamma1: %.4f\n", params$gamma1))
cat(sprintf("   - OLS estimate (biased): %.4f\n", ols_est))
cat(sprintf("   - 2SLS estimate: %.4f\n", tsls_est))
cat(sprintf("   - GMM estimate: %.4f\n", gmm_tri_est))

cat("\n2. Advantages of GMM over 2SLS:\n")
cat(sprintf("   - Efficiency gain: %.1f%%\n", eff_gain))
cat("   - Optimal weighting of moment conditions\n")
cat("   - J-test for overidentification available\n")

cat("\n3. Regime-based identification:\n")
cat("   - Number of regimes:", length(unique(data$regime)), "\n")
cat("   - Variance ratio between regimes:",
    params$sigma2_regimes[2] / params$sigma2_regimes[1], "\n")

cat("\n4. Diagnostics:\n")
cat("   - First-stage F-statistics:\n")
print(rigobon_2sls$first_stage_F)

cat("\nAnalysis complete.\n")
