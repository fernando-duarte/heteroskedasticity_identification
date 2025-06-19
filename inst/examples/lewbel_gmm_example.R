#' @title Example: GMM Estimation for Lewbel (2012) Heteroskedasticity-Based Identification
#' @description This example demonstrates how to use the GMM implementation
#' for Lewbel's heteroskedasticity-based identification method.
#' @author Your Name
#' @date 2024

# Load required packages
library(heteroskedasticity)
library(gmm)

# Set seed for reproducibility
set.seed(42)

cat("========================================\n")
cat("GMM Estimation for Lewbel (2012) Model\n")
cat("========================================\n\n")

# -----------------------------------------------------------------------------
# 1. Generate Example Data
# -----------------------------------------------------------------------------

cat("1. Generating example data...\n")

# Parameters for data generation
n <- 1000  # Sample size
params <- list(
  beta1_0 = 0.5,    # Intercept in first equation
  beta1_1 = 1.5,    # Coefficient on X in first equation
  gamma1 = -0.8,    # Coefficient on Y2 (endogenous)
  beta2_0 = 1.0,    # Intercept in second equation
  beta2_1 = -1.0,   # Coefficient on X in second equation
  alpha1 = -0.5,    # Controls heteroskedasticity
  alpha2 = 1.0,     # Controls heteroskedasticity
  delta_het = 1.2   # Heteroskedasticity strength
)

# Generate data using Lewbel DGP
data <- generate_lewbel_data(n, params)

cat("  Sample size:", n, "\n")
cat("  True gamma1:", params$gamma1, "\n\n")

# -----------------------------------------------------------------------------
# 2. Basic GMM Estimation - Triangular System
# -----------------------------------------------------------------------------

cat("2. Estimating triangular system using GMM...\n\n")

# Estimate using two-step GMM (default)
gmm_tri <- lewbel_gmm(data, system = "triangular")

# Print results
print(gmm_tri)

# More detailed summary
summary(gmm_tri)

# Extract key results
gamma1_gmm <- coef(gmm_tri)["gamma1"]
gamma1_se <- sqrt(diag(vcov(gmm_tri)))["gamma1"]

cat("\nGMM Results:\n")
cat("  gamma1 estimate:", round(gamma1_gmm, 4), "\n")
cat("  Standard error:", round(gamma1_se, 4), "\n")
cat("  Bias:", round(gamma1_gmm - params$gamma1, 4), "\n")

# -----------------------------------------------------------------------------
# 3. Compare Different GMM Types
# -----------------------------------------------------------------------------

cat("\n3. Comparing different GMM estimation methods...\n\n")

# Two-step GMM (already done above)
gmm_twostep <- gmm_tri

# Iterative GMM
cat("  Estimating with iterative GMM...\n")
gmm_iter <- lewbel_gmm(data, system = "triangular", gmm_type = "iterative")

# Continuous Updating Estimator (CUE)
cat("  Estimating with CUE...\n")
gmm_cue <- lewbel_gmm(data, system = "triangular", gmm_type = "cue")

# Compare results
comparison_gmm_types <- data.frame(
  Method = c("Two-step", "Iterative", "CUE"),
  gamma1 = c(
    coef(gmm_twostep)["gamma1"],
    coef(gmm_iter)["gamma1"],
    coef(gmm_cue)["gamma1"]
  ),
  SE = c(
    sqrt(diag(vcov(gmm_twostep)))["gamma1"],
    sqrt(diag(vcov(gmm_iter)))["gamma1"],
    sqrt(diag(vcov(gmm_cue)))["gamma1"]
  ),
  J_stat = c(
    gmm_twostep$test$test[1],
    gmm_iter$test$test[1],
    gmm_cue$test$test[1]
  ),
  p_value = c(
    gmm_twostep$test$test[2],
    gmm_iter$test$test[2],
    gmm_cue$test$test[2]
  )
)

print(comparison_gmm_types)

# -----------------------------------------------------------------------------
# 4. Compare GMM with 2SLS
# -----------------------------------------------------------------------------

cat("\n4. Comparing GMM with traditional 2SLS...\n\n")

# Run comparison
comparison <- compare_gmm_2sls(data)
print(comparison)

# -----------------------------------------------------------------------------
# 5. Simultaneous Equations System
# -----------------------------------------------------------------------------

cat("\n5. Estimating simultaneous equations system...\n\n")

# Generate data with multiple X variables (needed for simultaneous system)
n_sim <- 1000
data_sim <- data.frame(
  X1 = rnorm(n_sim),
  X2 = rnorm(n_sim),
  X3 = rnorm(n_sim)
)

# Create endogenous variables with simultaneity
gamma1_true <- -0.6
gamma2_true <- 0.3
beta1 <- c(0.5, 1.0, -0.5, 0.8)
beta2 <- c(1.0, -0.5, 1.2, -0.3)

# Generate errors with heteroskedasticity
u1 <- rnorm(n_sim) * exp(0.5 * data_sim$X1)
u2 <- rnorm(n_sim) * exp(-0.5 * data_sim$X1)

# Solve simultaneous system
# Y1 = X*beta1 + gamma1*Y2 + u1
# Y2 = X*beta2 + gamma2*Y1 + u2
# Reduced form: Y = (I - Gamma)^(-1) * (X*Beta + U)

X_mat <- as.matrix(cbind(1, data_sim[, c("X1", "X2", "X3")]))
Gamma <- matrix(c(0, gamma1_true, gamma2_true, 0), 2, 2)
Beta <- cbind(beta1, beta2)
U <- cbind(u1, u2)

# Check identification condition
if (abs(gamma1_true * gamma2_true) >= 1) {
  stop("Model not identified: gamma1 * gamma2 >= 1")
}

# Solve for Y
I_minus_Gamma_inv <- solve(diag(2) - Gamma)
Y <- (X_mat %*% Beta + U) %*% t(I_minus_Gamma_inv)

data_sim$Y1 <- Y[, 1]
data_sim$Y2 <- Y[, 2]

# Estimate simultaneous system
cat("  True parameters:\n")
cat("    gamma1 =", gamma1_true, "\n")
cat("    gamma2 =", gamma2_true, "\n")
cat("    gamma1 * gamma2 =", gamma1_true * gamma2_true, "\n\n")

gmm_sim <- lewbel_gmm(
  data_sim,
  system = "simultaneous",
  y1_var = "Y1",
  y2_var = "Y2",
  x_vars = c("X1", "X2", "X3")
)

print(gmm_sim)

# -----------------------------------------------------------------------------
# 6. Using Custom Heteroskedasticity Drivers
# -----------------------------------------------------------------------------

cat("\n6. Using custom heteroskedasticity drivers (Z variables)...\n\n")

# Add custom Z variables to data
data$Z1 = data$Xk^2  # Squared X
data$Z2 = abs(data$Xk)  # Absolute value of X

# Estimate with custom Z
gmm_custom_z <- lewbel_gmm(
  data,
  system = "triangular",
  z_vars = c("Z1", "Z2")
)

cat("Results with custom Z variables:\n")
cat("  gamma1 estimate:", round(coef(gmm_custom_z)["gamma1"], 4), "\n")
cat("  Standard error:", round(sqrt(diag(vcov(gmm_custom_z)))["gamma1"], 4), "\n")

# -----------------------------------------------------------------------------
# 7. Diagnostic Tests
# -----------------------------------------------------------------------------

cat("\n7. Diagnostic tests...\n\n")

# J-test for overidentifying restrictions
j_test <- gmm_tri$test
cat("J-test for overidentifying restrictions:\n")
cat("  J-statistic:", round(j_test$test[1], 4), "\n")
cat("  p-value:", round(j_test$test[2], 4), "\n")
cat("  Degrees of freedom:", j_test$df, "\n")

if (j_test$test[2] > 0.05) {
  cat("  => Fail to reject null: moment conditions are valid\n")
} else {
  cat("  => Reject null: potential model misspecification\n")
}

# Check first-stage strength (construct Lewbel instruments manually)
e2_hat <- residuals(lm(Y2 ~ Xk, data = data))
z_centered <- scale(data$Xk, center = TRUE, scale = FALSE)
lewbel_iv <- z_centered * e2_hat

first_stage <- lm(Y2 ~ Xk + lewbel_iv, data = data)
f_stat <- summary(first_stage)$fstatistic[1]

cat("\nFirst-stage F-statistic:", round(f_stat, 2), "\n")
if (f_stat > 10) {
  cat("  => Strong instruments\n")
} else {
  cat("  => Weak instruments warning\n")
}

# -----------------------------------------------------------------------------
# 8. Bootstrap Inference
# -----------------------------------------------------------------------------

cat("\n8. Bootstrap inference for GMM estimates...\n\n")

# Function to perform one bootstrap iteration
boot_gmm <- function(data, indices) {
  d <- data[indices, ]
  gmm_boot <- lewbel_gmm(d, system = "triangular")
  return(coef(gmm_boot)["gamma1"])
}

# Run bootstrap
library(boot)
set.seed(123)
boot_results <- boot(data, boot_gmm, R = 100)

# Bootstrap confidence interval
boot_ci <- boot.ci(boot_results, type = "perc")
cat("Bootstrap 95% CI for gamma1:",
    round(boot_ci$percent[4], 4), "to", round(boot_ci$percent[5], 4), "\n")

# -----------------------------------------------------------------------------
# 9. Summary and Recommendations
# -----------------------------------------------------------------------------

cat("\n========================================\n")
cat("Summary and Recommendations\n")
cat("========================================\n\n")

cat("1. GMM vs 2SLS:\n")
cat("   - GMM can be more efficient than 2SLS\n")
cat("   - GMM allows for optimal weighting of moment conditions\n")
cat("   - 2SLS is a special case of GMM\n\n")

cat("2. Choice of GMM type:\n")
cat("   - Two-step: Fast, good for most applications\n")
cat("   - Iterative: May improve finite sample properties\n")
cat("   - CUE: Theoretically optimal but computationally intensive\n\n")

cat("3. Key diagnostics:\n")
cat("   - Always check J-test for overidentification\n")
cat("   - Monitor first-stage F-statistic\n")
cat("   - Consider bootstrap for small samples\n\n")

cat("4. When to use simultaneous system:\n")
cat("   - When both Y1 and Y2 are endogenous\n")
cat("   - Requires at least 2 heteroskedasticity drivers\n")
cat("   - Check that gamma1 * gamma2 != 1\n\n")

# Clean up
rm(list = ls())
