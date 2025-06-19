# Example: Prono (2014) GARCH-Based Heteroskedasticity Identification
# This example demonstrates the implementation of Prono's method for
# using conditional heteroskedasticity to identify endogenous parameters

library(hetid)

# Create configuration for Prono simulation
# Now uses percent-scale returns by default (matching Prono 2014)
config <- create_prono_config(
  n = 500,        # Sample size
  k = 1,          # Number of exogenous variables
  gamma1 = 1.0,   # True beta (unitless)
  rho = 0.5,      # Correlation between errors
  se_type = "asymptotic"
  # Default parameters now match Prono's scale:
  # - beta2 intercept = 0.097 (mean market excess return 0.097%)
  # - GARCH omega = 0.04 (for ~2% weekly volatility)
  # - sigma1 = 1.5 (portfolio idiosyncratic vol in percent)
)

# Run a single simulation
cat("Running single Prono simulation...\n")
single_result <- run_single_prono_simulation(config, return_details = TRUE)

cat("\nResults from single simulation:\n")
cat(sprintf("True gamma1: %.3f\n", single_result$gamma1_true))
cat(sprintf("OLS estimate: %.3f (bias: %.3f)\n",
            single_result$gamma1_ols, single_result$bias_ols))
cat(sprintf("Prono IV estimate: %.3f (bias: %.3f)\n",
            single_result$gamma1_iv, single_result$bias_iv))
cat(sprintf("First-stage F-statistic: %.2f\n", single_result$f_stat))

# Run Monte Carlo simulation
# Note: Using only 100 simulations for example. For research, use 1000+
cat("\n\nRunning Monte Carlo simulation (100 replications)...\n")
mc_results <- run_prono_monte_carlo(
  config,
  n_sims = 100,
  parallel = FALSE,
  progress = TRUE
)

# Analyze results
cat("\nMonte Carlo Results:\n")
cat(sprintf("Mean OLS bias: %.3f\n", mean(mc_results$bias_ols)))
cat(sprintf("Mean IV bias: %.3f\n", mean(mc_results$bias_iv)))
cat(sprintf("SD of OLS estimates: %.3f\n", sd(mc_results$gamma1_ols)))
cat(sprintf("SD of IV estimates: %.3f\n", sd(mc_results$gamma1_iv)))
cat(sprintf("Proportion of weak instruments (F < 10): %.1f%%\n",
            100 * mean(mc_results$f_stat < 10)))

# Plot results if ggplot2 is available
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  # Create data for plotting
  plot_data <- data.frame(
    Estimator = rep(c("OLS", "Prono IV"), each = nrow(mc_results)),
    Estimate = c(mc_results$gamma1_ols, mc_results$gamma1_iv)
  )

  # Create density plot
  p <- ggplot(plot_data, aes(x = Estimate, fill = Estimator)) +
    geom_density(alpha = 0.5) +
    geom_vline(xintercept = config$gamma1, linetype = "dashed", color = "red") +
    labs(title = "Distribution of Estimates: Prono (2014) Method",
         x = "Estimate of gamma1",
         y = "Density",
         caption = paste("True value (red line):", config$gamma1)) +
    theme_minimal() +
    scale_fill_manual(values = c("OLS" = "blue", "Prono IV" = "green"))

  print(p)
}

# Note about GARCH implementation
cat("\n\nNote: For full GARCH implementation, install the 'rugarch' package:\n")
cat("install.packages('rugarch')\n")
cat("Without rugarch, the method uses squared residuals as a proxy for conditional variance.\n")
