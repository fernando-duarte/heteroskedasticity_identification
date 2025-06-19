## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(hetid)
library(ggplot2)

## ----demo, message=FALSE------------------------------------------------------
# Run demonstration with 300 observations
run_prono_demo(n = 300)

## ----generate-data------------------------------------------------------------
# Set parameters
params <- list(
  n = 500,
  k = 1,  # number of exogenous variables
  beta1 = c(1, 0.5),
  beta2 = c(2, -0.3),
  gamma1 = 0.7,  # true endogenous parameter
  garch_params = list(
    omega = 0.1,
    alpha = 0.15,
    beta = 0.75
  ),
  sigma1 = 1,
  rho = 0.6,  # correlation between errors (endogeneity)
  seed = 123
)

# Generate data
data <- do.call(generate_prono_data, params)
head(data)

## ----plot-variance------------------------------------------------------------
plot(data$time, data$sigma2_sq, type = "l",
     main = "Conditional Variance (GARCH Effect)",
     xlab = "Time", ylab = "Sigma^2",
     col = "blue", lwd = 2)
grid()

## ----single-estimation--------------------------------------------------------
# Create configuration
config <- create_prono_config(
  n = 500,
  gamma1 = 0.7,
  garch_params = list(omega = 0.1, alpha = 0.15, beta = 0.75),
  rho = 0.6,
  seed = 123
)

# Run estimation
result <- run_single_prono_simulation(config, return_details = TRUE)

# Compare estimates
cat("Estimation Results:\n")
cat(sprintf("True gamma1:    %.4f\n", result$gamma1_true))
cat(sprintf("OLS estimate:   %.4f (bias: %+.4f)\n", 
            result$gamma1_ols, result$bias_ols))
cat(sprintf("Prono estimate: %.4f (bias: %+.4f)\n", 
            result$gamma1_iv, result$bias_iv))
cat(sprintf("Bias reduction: %.1f%%\n", 
            100 * (1 - abs(result$bias_iv)/abs(result$bias_ols))))

## ----monte-carlo, cache=TRUE--------------------------------------------------
# Run 500 simulations
mc_results <- run_prono_monte_carlo(
  config, 
  n_sims = 500,
  parallel = FALSE,
  progress = FALSE
)

# Summary statistics
summary_stats <- data.frame(
  Method = c("OLS", "Prono IV"),
  Mean_Bias = c(mean(mc_results$bias_ols), mean(mc_results$bias_iv)),
  SD_Bias = c(sd(mc_results$bias_ols), sd(mc_results$bias_iv)),
  RMSE = c(sqrt(mean(mc_results$bias_ols^2)), 
           sqrt(mean(mc_results$bias_iv^2)))
)

print(summary_stats, digits = 4)

## ----plot-mc-results----------------------------------------------------------
# Prepare data for plotting
plot_data <- data.frame(
  Method = rep(c("OLS", "Prono IV"), each = nrow(mc_results)),
  Estimate = c(mc_results$gamma1_ols, mc_results$gamma1_iv),
  Bias = c(mc_results$bias_ols, mc_results$bias_iv)
)

# Density plot of estimates
p1 <- ggplot(plot_data, aes(x = Estimate, fill = Method)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = config$gamma1, linetype = "dashed", size = 1) +
  labs(title = "Distribution of Estimates",
       subtitle = sprintf("True value = %.1f (dashed line)", config$gamma1),
       x = "Estimate", y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("OLS" = "#E74C3C", "Prono IV" = "#3498DB"))

print(p1)

# Box plot of bias
p2 <- ggplot(plot_data, aes(x = Method, y = Bias, fill = Method)) +
  geom_boxplot(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Bias Distribution",
       subtitle = "Unbiased = 0 (dashed line)",
       y = "Bias") +
  theme_minimal() +
  scale_fill_manual(values = c("OLS" = "#E74C3C", "Prono IV" = "#3498DB")) +
  theme(legend.position = "none")

print(p2)

## ----f-stats------------------------------------------------------------------
# Analyze first-stage F-statistics
f_summary <- data.frame(
  Mean_F = mean(mc_results$f_stat),
  Median_F = median(mc_results$f_stat),
  Prop_Weak = mean(mc_results$f_stat < 10),
  Prop_Strong = mean(mc_results$f_stat > 30)
)

print(f_summary, digits = 2)

# Plot F-statistic distribution
ggplot(mc_results, aes(x = f_stat)) +
  geom_histogram(bins = 30, fill = "#2ECC71", alpha = 0.7) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Distribution of First-Stage F-Statistics",
       subtitle = "Weak instrument threshold = 10 (red line)",
       x = "F-statistic", y = "Count") +
  theme_minimal()

## ----sensitivity-garch, cache=TRUE--------------------------------------------
# Test different GARCH parameters
alpha_values <- c(0.05, 0.1, 0.2, 0.3)
results_list <- list()

for (i in seq_along(alpha_values)) {
  config_temp <- config
  config_temp$garch_params$alpha <- alpha_values[i]
  config_temp$garch_params$beta <- 0.9 - alpha_values[i]  # Keep sum < 1
  
  mc_temp <- run_prono_monte_carlo(
    config_temp, 
    n_sims = 200,
    parallel = FALSE,
    progress = FALSE
  )
  
  results_list[[i]] <- data.frame(
    alpha = alpha_values[i],
    mean_bias_ols = mean(mc_temp$bias_ols),
    mean_bias_iv = mean(mc_temp$bias_iv),
    mean_f_stat = mean(mc_temp$f_stat)
  )
}

sensitivity_results <- do.call(rbind, results_list)

# Plot results
par(mfrow = c(1, 2))
plot(sensitivity_results$alpha, abs(sensitivity_results$mean_bias_iv),
     type = "b", col = "blue", pch = 19,
     xlab = "GARCH Alpha Parameter",
     ylab = "Absolute Bias",
     main = "Bias vs GARCH Parameter")
lines(sensitivity_results$alpha, abs(sensitivity_results$mean_bias_ols),
      type = "b", col = "red", pch = 17)
legend("topright", c("Prono IV", "OLS"), 
       col = c("blue", "red"), pch = c(19, 17), lty = 1)

plot(sensitivity_results$alpha, sensitivity_results$mean_f_stat,
     type = "b", col = "darkgreen", pch = 19,
     xlab = "GARCH Alpha Parameter",
     ylab = "Mean F-statistic",
     main = "Instrument Strength vs GARCH Parameter")
abline(h = 10, lty = 2, col = "red")

## ----sensitivity-n, cache=TRUE------------------------------------------------
# Test different sample sizes
n_values <- c(100, 200, 500, 1000)
n_results <- list()

for (i in seq_along(n_values)) {
  config_n <- config
  config_n$n <- n_values[i]
  
  mc_n <- run_prono_monte_carlo(
    config_n, 
    n_sims = 200,
    parallel = FALSE,
    progress = FALSE
  )
  
  n_results[[i]] <- data.frame(
    n = n_values[i],
    bias_ols = mean(mc_n$bias_ols),
    bias_iv = mean(mc_n$bias_iv),
    sd_iv = sd(mc_n$gamma1_iv)
  )
}

n_sensitivity <- do.call(rbind, n_results)

# Plot
ggplot(n_sensitivity, aes(x = n)) +
  geom_line(aes(y = abs(bias_ols), color = "OLS"), size = 1.2) +
  geom_line(aes(y = abs(bias_iv), color = "Prono IV"), size = 1.2) +
  geom_point(aes(y = abs(bias_ols), color = "OLS"), size = 3) +
  geom_point(aes(y = abs(bias_iv), color = "Prono IV"), size = 3) +
  scale_x_log10() +
  labs(title = "Bias vs Sample Size",
       x = "Sample Size (log scale)",
       y = "Absolute Bias",
       color = "Method") +
  theme_minimal() +
  scale_color_manual(values = c("OLS" = "#E74C3C", "Prono IV" = "#3498DB"))

## ----eval=FALSE---------------------------------------------------------------
# install.packages("rugarch")

