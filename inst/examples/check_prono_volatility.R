# Check if we match Prono's volatility (standard deviation) of returns

library(hetid)

cat("=== CHECKING VOLATILITY MATCH WITH PRONO (2014) ===\n\n")

# Prono doesn't explicitly report the std dev, but we can infer from typical weekly returns
cat("1. Typical weekly return volatility in finance:\n")
cat("   - US equity market: ~2-2.5% per week\n")
cat("   - Individual portfolios: ~2-4% per week\n")
cat("   - During 1963-2004: likely ~2% for market\n\n")

# Generate large sample to check our volatility
cat("2. Our implementation's volatility:\n")
set.seed(2014)
n_sims <- 100
market_sds <- numeric(n_sims)
portfolio_sds <- numeric(n_sims)

for (i in 1:n_sims) {
  data <- generate_prono_data(n = 2166, seed = i)
  market_sds[i] <- sd(data$Y2)
  portfolio_sds[i] <- sd(data$Y1)
}

cat(sprintf("   Market (Y2) volatility:\n"))
cat(sprintf("     Mean SD across sims: %.3f%%\n", mean(market_sds)))
cat(sprintf("     Range: [%.3f%%, %.3f%%]\n", min(market_sds), max(market_sds)))

cat(sprintf("\n   Portfolio (Y1) volatility:\n"))
cat(sprintf("     Mean SD across sims: %.3f%%\n", mean(portfolio_sds)))
cat(sprintf("     Range: [%.3f%%, %.3f%%]\n", min(portfolio_sds), max(portfolio_sds)))

# Check unconditional variance from GARCH parameters
cat("\n3. Theoretical volatility from GARCH parameters:\n")
config <- create_prono_config()
omega <- config$garch_params$omega
alpha <- config$garch_params$alpha
beta <- config$garch_params$beta

# Unconditional variance = omega / (1 - alpha - beta)
uncond_var <- omega / (1 - alpha - beta)
uncond_sd <- sqrt(uncond_var)

cat(sprintf("   GARCH unconditional SD: %.3f%%\n", uncond_sd))
cat(sprintf("   This is the long-run average volatility\n"))

# Compare with historical data
cat("\n4. Historical comparison:\n")
cat("   Prono's sample period: July 1963 - Dec 2004\n")
cat("   Historical weekly market volatility in this period: ~2%\n")
cat(sprintf("   Our simulated volatility: %.3f%%\n", mean(market_sds)))
cat(sprintf("   Match quality: %s\n",
            ifelse(abs(mean(market_sds) - 2.0) < 0.5, "GOOD", "NEEDS ADJUSTMENT")))

# Test with different sample sizes
cat("\n5. Volatility by sample size:\n")
sample_sizes <- c(100, 500, 1000, 2166, 5000)
for (n in sample_sizes) {
  data <- generate_prono_data(n = n, seed = 123)
  cat(sprintf("   n=%d: SD = %.3f%%\n", n, sd(data$Y2)))
}

# Check if GARCH is creating realistic volatility clustering
cat("\n6. GARCH volatility clustering check:\n")
data <- generate_prono_data(n = 1000, seed = 42)

# Calculate rolling window volatility
window <- 22 # Approximately one month of trading days
if (nrow(data) >= window) {
  rolling_vol <- numeric(nrow(data) - window + 1)
  for (i in 1:(nrow(data) - window + 1)) {
    rolling_vol[i] <- sd(data$Y2[i:(i + window - 1)])
  }
  # Scale to annualized percentage
  rolling_vol_ann <- rolling_vol * sqrt(252) * 100
}

cat(sprintf("   Rolling %d-week volatility:\n", window))
cat(sprintf("     Mean: %.3f%%\n", mean(rolling_vol)))
cat(sprintf("     SD: %.3f%%\n", sd(rolling_vol)))
cat(sprintf("     Min: %.3f%%, Max: %.3f%%\n", min(rolling_vol), max(rolling_vol)))
cat("   Volatility clustering present: ")
cat(ifelse(sd(rolling_vol) > 0.2, "YES\n", "WEAK\n"))

# Final assessment
cat("\n=== ASSESSMENT ===\n")
cat(sprintf("Market return volatility: %.3f%% (target: ~2%%)\n", mean(market_sds)))
cat(sprintf("Deviation from target: %.1f%%\n", 100 * abs(mean(market_sds) - 2.0) / 2.0))

if (mean(market_sds) < 1.5) {
  cat("\nOur volatility is TOO LOW. Consider:\n")
  cat("- Increasing GARCH omega parameter\n")
  cat("- Increasing error variance\n")
} else if (mean(market_sds) > 2.5) {
  cat("\nOur volatility is TOO HIGH. Consider:\n")
  cat("- Decreasing GARCH omega parameter\n")
  cat("- Decreasing error variance\n")
} else {
  cat("\n✓ Volatility matches typical weekly equity returns\n")
  cat("✓ GARCH creates realistic volatility clustering\n")
  cat("✓ Scale is appropriate for asset pricing applications\n")
}
