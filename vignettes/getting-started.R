## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(hetid)
library(gmm) # For lewbel_gmm, though not directly used in this vignette's runnable code

## ----generate-data------------------------------------------------------------
# Set parameters for the data generating process
params <- list(
  beta1_0 = 0.5, # Intercept in first equation
  beta1_1 = 1.5, # Coefficient on X in first equation
  gamma1 = -0.8, # Endogenous parameter (true value)
  beta2_0 = 1.0, # Intercept in second equation
  beta2_1 = -1.0, # Coefficient on X in second equation
  alpha1 = -0.5, # Factor loading for error correlation
  alpha2 = 1.0, # Factor loading for error correlation
  delta_het = 1.2 # Heteroskedasticity strength
)

# Generate a sample
set.seed(123)
data <- generate_lewbel_data(n_obs = 1000, params = params)

# Display basic information about the generated data
cat("Generated data dimensions:", dim(data), "\n")
cat("Variable names:", names(data), "\n\n")
head(data)

## ----verify-assumptions-------------------------------------------------------
# Verify the assumptions using the generated data
# The function tests three key conditions:
# 1. Cov(Z, ε₁ε₂) = 0 (covariance restriction)
# 2. Cov(Z, ε₂²) ≠ 0 (instrument relevance)
# 3. Cov(ε₁, ε₂) ≠ 0 (endogeneity)
verification_result <- verify_lewbel_assumptions(
  data = data, config = params, verbose = TRUE
)

## ----point-estimation---------------------------------------------------------
# Run a single simulation to demonstrate estimation
# This compares OLS (biased) vs 2SLS with Lewbel instruments (consistent)
result <- run_single_lewbel_simulation(
  sim_id = 1,
  params = c(params, list(
    sample_size = 1000,
    tau_set_id = 0.2,
    bootstrap_reps = 100
  ))
)

# Display results
cat("True gamma1:", params$gamma1, "\n")
cat("OLS estimate:", round(result$ols_gamma1, 3), "\n")
cat("2SLS (Lewbel) estimate:", round(result$tsls_gamma1, 3), "\n")
cat("First-stage F-statistic:", round(result$first_stage_F, 1), "\n")

# Check if instruments are strong (F > 10 is conventional threshold)
if (result$first_stage_F > 10) {
  cat("✓ Strong instruments (F > 10)\n")
} else {
  cat("⚠ Weak instruments (F ≤ 10)\n")
}

## ----set-identification-------------------------------------------------------
# Calculate bounds under exact identification (tau = 0)
# When tau = 0, we get point identification
bounds_exact <- calculate_lewbel_bounds(data, tau = 0)
cat("Bounds under exact identification (tau = 0):\n")
cat(
  "[", round(bounds_exact$bounds[1], 3), ",",
  round(bounds_exact$bounds[2], 3), "]\n"
)

# Calculate bounds under relaxed assumption (tau = 0.2)
# When tau > 0, we allow some violation of the covariance restriction
bounds_relaxed <- calculate_lewbel_bounds(data, tau = 0.2)
cat("\nBounds under relaxed assumption (tau = 0.2):\n")
cat(
  "[", round(bounds_relaxed$bounds[1], 3), ",",
  round(bounds_relaxed$bounds[2], 3), "]\n"
)

# With bootstrap standard errors for inference
bounds_with_se <- calculate_lewbel_bounds(
  data,
  tau = 0.2,
  compute_se = TRUE,
  b_reps = 100
)
cat("\nBounds with bootstrap standard errors:\n")
cat(
  "Lower bound:", round(bounds_with_se$bounds[1], 3),
  "(SE:", round(bounds_with_se$se[1], 3), ")\n"
)
cat(
  "Upper bound:", round(bounds_with_se$bounds[2], 3),
  "(SE:", round(bounds_with_se$se[2], 3), ")\n"
)

# Check if true value is contained in the bounds
true_gamma <- params$gamma1
in_bounds_exact <- (true_gamma >= bounds_exact$bounds[1] &&
                    true_gamma <= bounds_exact$bounds[2])
in_bounds_relaxed <- (true_gamma >= bounds_relaxed$bounds[1] &&
                      true_gamma <= bounds_relaxed$bounds[2])

cat("\nTrue value γ₁ =", true_gamma, "\n")
cat("Contained in exact bounds:", ifelse(in_bounds_exact, "✓", "✗"), "\n")
cat("Contained in relaxed bounds:", ifelse(in_bounds_relaxed, "✓", "✗"), "\n")

## ----monte-carlo, eval=FALSE--------------------------------------------------
# # Example: Run a small Monte Carlo study
# # (Not executed in vignette to save time)
# library(purrr)
# library(dplyr)
#
# # Run 100 simulations to assess estimator performance
# n_sims <- 100
# sim_params <- c(params, list(
#   sample_size = 500,
#   tau_set_id = 0.2,
#   bootstrap_reps = 50
# ))
#
# cat("Running", n_sims, "Monte Carlo simulations...\n")
# results <- map_dfr(1:n_sims, ~ run_single_lewbel_simulation(.x, sim_params))
#
# # Summarize performance metrics
# summary_stats <- results |>
#   summarise(
#     # Bias (should be close to 0 for consistent estimators)
#     ols_bias = mean(ols_gamma1 - params$gamma1, na.rm = TRUE),
#     tsls_bias = mean(tsls_gamma1 - params$gamma1, na.rm = TRUE),
#
#     # Coverage rates (should be close to 0.95 for 95% confidence intervals)
#     ols_coverage = mean(ols_coverage, na.rm = TRUE),
#     tsls_coverage = mean(tsls_coverage, na.rm = TRUE),
#
#     # Instrument strength
#     avg_first_stage_F = mean(first_stage_F, na.rm = TRUE),
#     weak_iv_rate = mean(first_stage_F < 10, na.rm = TRUE)
#   )
#
# cat("\nMonte Carlo Results:\n")
# print(summary_stats)
#
# # Additional analysis: Distribution of estimates
# cat("\nDistribution of 2SLS estimates:\n")
# cat("Mean:", round(mean(results$tsls_gamma1, na.rm = TRUE), 3), "\n")
# cat("Median:", round(median(results$tsls_gamma1, na.rm = TRUE), 3), "\n")
# cat("True Value:", params$gamma1, "\n")
# cat("Mean Bias:", round(mean(results$tsls_gamma1 - params$gamma1, na.rm = TRUE), 3), "\n")
#
# cat("\n--- Monte Carlo Distribution ---\n")
# # Plot the distribution of estimates
# # hist(results$tsls_gamma1, breaks = 30, main = "Distribution of gamma1 Estimates", xlab = "gamma1")
# # abline(v = params$gamma1, col = "red", lwd = 2)

# Run a small Monte Carlo simulation (conceptual)
# The actual run_lewbel_monte_carlo function would be used for a proper simulation

# Example parameters for the simulation (adjust as needed)
params <- list(
  n = 200,             # Sample size
  beta1_true = c(0.5, 1.0), # True beta1 coefficients (intercept, Xk)
  beta2_true = c(1.0, -0.5),# True beta2 coefficients
  gamma1_true = -0.7,    # True gamma1 coefficient
  rho_uz = 0.3,          # Correlation between u1 and Z*u2 (endogeneity strength)
  hetero_strength = 2.0  # Strength of heteroskedasticity
)

# Conceptual structure for a single simulation run (simplified)
run_single_simulation_conceptual <- function(sim_params) {
  # Generate data based on sim_params
  # For this conceptual example, we'll reuse the earlier 'data' for structure
  # In a real MC, data would be generated here using generate_lewbel_data()
  temp_data <- generate_lewbel_data(n = sim_params$n, params = sim_params) # Placeholder

  # Estimate OLS
  ols_fit <- lm(Y1 ~ Y2 + Xk, data = temp_data)
  ols_gamma1 <- coef(ols_fit)["Y2"]

  # Estimate 2SLS (Lewbel)
  # This is a simplified representation; lewbel_2sls or lewbel_gmm would be used
  e2_hat <- residuals(lm(Y2 ~ Xk, data = temp_data))
  z_centered <- scale(temp_data$Xk, center = TRUE, scale = FALSE)
  lewbel_iv_est <- z_centered * e2_hat
  first_stage_fit <- lm(Y2 ~ Xk + lewbel_iv_est, data = temp_data)

  # Check if IV is significant in first stage (proxy for strength)
  f_stat_val <- NA
  if("lewbel_iv_est" %in% rownames(summary(first_stage_fit)$coefficients)){
      f_stat_val <- summary(first_stage_fit)$coefficients["lewbel_iv_est", "t value"]^2
  }

  y2_fitted <- fitted(first_stage_fit)
  tsls_fit <- lm(Y1 ~ y2_fitted + Xk, data = temp_data)
  tsls_gamma1 <- coef(tsls_fit)["y2_fitted"]

  # Return relevant results
  list(
    ols_gamma1 = ols_gamma1,
    tsls_gamma1 = tsls_gamma1,
    first_stage_F = f_stat_val
    # In a real MC, would also return SEs, coverage, etc.
  )
}

# This is a conceptual placeholder and will not be run in the vignette build
# because it's computationally intensive and for illustration.

## Advanced Topics

### Set Identification

When the covariance restriction $\text{Cov}(Z, \epsilon_1 \epsilon_2) = 0$ might be violated, but boundedly so, Lewbel (2012) also provides a method for set identification. The parameter $\gamma_1$ is identified to lie within an interval.

```{r set-id, eval=TRUE}
# Using the calculate_lewbel_bounds function
# Assume a tau value (strength of correlation between Z and error product)
# tau = 0 means point identification, tau > 0 gives a set.
tau_value <- 0.1
lewbel_bounds <- calculate_lewbel_bounds(data = data, tau = tau_value)

cat("\n--- Lewbel Set Identification (tau = ", tau_value, ") ---\n")
cat(sprintf("Identified set for gamma1: [%.4f, %.4f]\n",
            lewbel_bounds$bounds[1], lewbel_bounds$bounds[2]))

# The true gamma1 was -0.8
cat(sprintf("True gamma1: %.4f. Is it in the set? %s\n",
            params$gamma1,
            params$gamma1 >= lewbel_bounds$bounds[1] && params$gamma1 <= lewbel_bounds$bounds[2]))
```

### Degrees of Freedom Adjustment

The package offers several ways to adjust standard errors for degrees of freedom, which can be important in smaller samples.

```{r df-adjust, eval=FALSE}
# This is a conceptual demonstration of how one might compare adjustments
# The actual comparison would use run_lewbel_monte_carlo_df

cat("For df adjustment comparison, see run_lewbel_monte_carlo_df() docs.\n")
```
