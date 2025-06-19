## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(hetid)

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
# cat("Std Dev:", round(sd(results$tsls_gamma1, na.rm = TRUE), 3), "\n")
# cat("Min:", round(min(results$tsls_gamma1, na.rm = TRUE), 3), "\n")
# cat("Max:", round(max(results$tsls_gamma1, na.rm = TRUE), 3), "\n")

