# Scale Comparison: Our Implementation vs Prono's Data

library(hetid)

cat("=== SCALE COMPARISON ===\n\n")

# 1. Generate data using our Prono function
cat("1. Our generate_prono_data() output:\n")
set.seed(123)
our_data <- generate_prono_data(
  n = 2166,
  beta1 = c(1, 0.5),
  beta2 = c(2, -0.3),
  gamma1 = 1,
  garch_params = list(omega = 0.0001, alpha = 0.1, beta = 0.85),
  sigma1 = 1,
  rho = 0.3
)

cat(sprintf("   Y2 mean: %.3f\n", mean(our_data$Y2)))
cat(sprintf("   Y2 SD: %.3f\n", sd(our_data$Y2)))
cat(sprintf("   Range: [%.1f, %.1f]\n\n", min(our_data$Y2), max(our_data$Y2)))

# 2. Prono's reported scale
cat("2. Prono (2014) reports:\n")
cat("   Market excess return mean: 0.097%\n")
cat("   Returns are in PERCENT\n")
cat("   Typical weekly volatility: ~2%\n\n")

# 3. Scale adjustment needed
cat("3. Scale mismatch analysis:\n")
cat(sprintf("   Our Y2 mean (%.1f) vs Prono's (0.097%%)\n", mean(our_data$Y2)))
cat(sprintf("   Scale factor: %.0fx difference\n\n", abs(mean(our_data$Y2)) / 0.097))

# 4. Generate data at the correct scale (percentage returns)
cat("4. Generating data at percentage scale:\n")

# For asset returns in percent, we need much smaller parameters
percent_scale_params <- list(
  beta1 = c(0.01, 0.005),  # Small intercept and slope
  beta2 = c(0.097, -0.001), # Mean of 0.097% with small X effect  
  gamma1 = 1,               # Beta coefficient (unitless)
  garch_params = list(
    omega = 0.01,    # Variance parameters for percent returns
    alpha = 0.1,
    beta = 0.85
  ),
  sigma1 = 0.5,      # Error SD in percent
  rho = 0.3
)

# Generate with percent-scale parameters
percent_data <- generate_prono_data(
  n = 2166,
  beta1 = percent_scale_params$beta1,
  beta2 = percent_scale_params$beta2,
  gamma1 = percent_scale_params$gamma1,
  garch_params = percent_scale_params$garch_params,
  sigma1 = percent_scale_params$sigma1,
  rho = percent_scale_params$rho,
  k = 1,
  seed = 123
)

cat(sprintf("   Y2 mean: %.3f%%\n", mean(percent_data$Y2)))
cat(sprintf("   Y2 SD: %.3f%%\n", sd(percent_data$Y2)))
cat(sprintf("   Range: [%.1f%%, %.1f%%]\n\n", 
            min(percent_data$Y2), max(percent_data$Y2)))

# 5. Check if this matches Prono better
cat("5. Match assessment:\n")
cat(sprintf("   Target mean: 0.097%%\n"))
cat(sprintf("   Our mean: %.3f%%\n", mean(percent_data$Y2)))
cat(sprintf("   Difference: %.3f%%\n", mean(percent_data$Y2) - 0.097))
cat(sprintf("   Match quality: %s\n\n", 
            ifelse(abs(mean(percent_data$Y2) - 0.097) < 0.01, "EXCELLENT", "NEEDS TUNING")))

# 6. What about volatility clustering?
cat("6. GARCH effects check:\n")
resid2 <- percent_data$eps2
squared_resid <- resid2^2
acf_result <- acf(squared_resid, lag.max = 5, plot = FALSE)
cat("   Autocorrelation of squared residuals:\n")
for (i in 1:5) {
  cat(sprintf("   Lag %d: %.3f\n", i, acf_result$acf[i+1]))
}

# 7. Implications for replication
cat("\n=== IMPLICATIONS ===\n")
cat("1. Our default parameters generate data at the wrong scale\n")
cat("2. For asset pricing applications, Y2 should represent percent returns\n")
cat("3. Parameters need to be scaled appropriately:\n")
cat("   - Intercepts ~0.01-0.1 (for percent returns)\n")
cat("   - GARCH omega ~0.01-0.1 (for percent variance)\n")
cat("   - Error SD ~0.5-2.0 (for percent returns)\n")
cat("4. The gamma parameter (beta) remains unitless\n")

# 8. Quick test with corrected scale
cat("\n=== CORRECTED SCALE TEST ===\n")

# Configure for percent returns matching Prono
prono_config <- create_prono_config(
  n = 500,
  k = 1,
  beta1 = c(0.05, 0.01),      # Portfolio excess return params
  beta2 = c(0.097, -0.005),   # Market excess return params  
  gamma1 = 1,                 # True beta
  garch_params = list(omega = 0.05, alpha = 0.1, beta = 0.85),
  sigma1 = 1,                 # Portfolio idiosyncratic vol
  rho = 0.4,                  # Endogeneity
  verbose = FALSE
)

# Run single simulation
result <- run_single_prono_simulation(prono_config, return_details = TRUE)

cat("\nEstimation with percent-scale data:\n")
cat(sprintf("True gamma (beta): %.3f\n", result$gamma1_true))
cat(sprintf("OLS estimate: %.3f (bias: %.3f)\n", 
            result$gamma1_ols, result$bias_ols))
cat(sprintf("Prono IV estimate: %.3f (bias: %.3f)\n", 
            result$gamma1_iv, result$bias_iv))
cat(sprintf("First-stage F-stat: %.1f\n", result$f_stat))