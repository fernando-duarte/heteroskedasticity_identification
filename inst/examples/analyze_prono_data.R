# Analyze Prono's actual data to compute summary statistics

cat("=== ANALYZING PRONO'S ACTUAL DATA ===\n\n")

# Define the path to the data file
# This is an example path and might need to be adjusted based on your setup
base_path <- "/Users/fernandoduarte/Dropbox (Personal)/Research/vfci/V-FCI-Overleaf/heteroskedasticity/"
data_file <- paste0(base_path, "lewbel2012/tp-files/ff_sze_bm_ind_Prono2013.txt")

# Read the data
prono_data <- read.table(data_file, header = FALSE)

# Assign column names based on readme file
col_names <- c("DATE", "rm", "smb", "hml", "RF")

# 25 Size-B/M portfolios (5x5)
for (i in 1:5) {
  for (j in 1:5) {
    col_names <- c(col_names, paste0("R", i, j))
  }
}

# 30 Industry portfolios
for (k in 1:30) {
  col_names <- c(col_names, paste0("RI", k))
}

colnames(prono_data) <- col_names

# Check dimensions
cat("Data dimensions:\n")
cat("  Rows (observations):", nrow(prono_data), "\n")
cat("  Columns:", ncol(prono_data), "\n\n")

# Convert date
prono_data$DATE <- as.Date(as.character(prono_data$DATE), format = "%Y%m%d")

cat("Date range:\n")
cat("  Start:", format(min(prono_data$DATE)), "\n")
cat("  End:", format(max(prono_data$DATE)), "\n\n")

# Compute summary statistics for key variables
cat("=== SUMMARY STATISTICS (Weekly Returns in %) ===\n\n")

# 1. Market excess return (rm - RF)
market_excess <- prono_data$rm - prono_data$RF
cat("1. Market Excess Return (rm - RF):\n")
cat(sprintf("   Mean: %.3f%%\n", mean(market_excess)))
cat(sprintf("   Standard Deviation: %.3f%%\n", sd(market_excess) * 100))

# Skewness and Kurtosis (manual calculation for clarity)
# Kurtosis = E[(X - mu)^4] / sigma^4 (Excess kurtosis would subtract 3)
n_market <- length(market_excess)
mean_market <- mean(market_excess)
sd_market <- sd(market_excess)

skewness_market <- sum((market_excess - mean_market)^3) / n_market / (sd_market^3)
cat(sprintf("   Skewness: %.3f\n", skewness_market))

kurtosis_market <- sum((market_excess - mean_market)^4) / n_market / (sd_market^4)
cat(sprintf("   Kurtosis: %.3f\n\n", kurtosis_market))

# 2. Fama-French factors
cat("2. Fama-French Factors:\n")
cat(sprintf("   SMB - Mean: %.3f%%, SD: %.3f%%\n", mean(prono_data$smb), sd(prono_data$smb)))
cat(sprintf("   HML - Mean: %.3f%%, SD: %.3f%%\n", mean(prono_data$hml), sd(prono_data$hml)))
cat(sprintf("   RF  - Mean: %.3f%%, SD: %.3f%%\n\n", mean(prono_data$RF), sd(prono_data$RF)))

# 3. Size-B/M portfolios (corner portfolios)
cat("3. 25 Size-B/M Portfolios (Corner Portfolios):\n")
corners <- list(
  "Small-Low" = "R11",
  "Small-High" = "R15",
  "Big-Low" = "R51",
  "Big-High" = "R55"
)

for (name in names(corners)) {
  col <- corners[[name]]
  cat(sprintf("   %s (%s) - Mean: %.3f%%, SD: %.3f%%\n",
              name, col, mean(prono_data[[col]]), sd(prono_data[[col]])))
}

# Compute excess returns
for (name in names(corners)) {
  col <- corners[[name]]
  excess <- prono_data[[col]] - prono_data$RF
  cat(sprintf("   %s Excess - Mean: %.3f%%, SD: %.3f%%\n",
              name, mean(excess), sd(excess)))
}

# Portfolio returns are from R11 to R55 (25 portfolios)
# These need to be reshaped or analyzed column by column
# Example: Summary for the first portfolio (R11)
portfolio_names <- paste0("V", 5:29) # Assuming V5 to V29 are portfolio returns
# A more robust way would be to identify these columns if names are consistent, e.g., Rxy
# For this example, let's assume portfolio columns are V5 through V29.

cat("\n4. Portfolio Returns (Sample - R11 or V5):\n")
if ("V5" %in% names(prono_data)) {
  portfolio_cols <- paste0("R", rep(1:5, each = 5), rep(1:5, 5))
  portfolio_means <- sapply(portfolio_cols, function(x) mean(prono_data[[x]]))
  portfolio_sds <- sapply(portfolio_cols, function(x) sd(prono_data[[x]]))

  cat(sprintf("   Mean returns range: [%.3f%%, %.3f%%]\n",
              min(portfolio_means), max(portfolio_means)))
  cat(sprintf("   SD range: [%.3f%%, %.3f%%]\n",
              min(portfolio_sds), max(portfolio_sds)))
}

# 5. Industry portfolios
cat("\n5. 30 Industry Portfolios:\n")
industry_cols <- paste0("RI", 1:30)
industry_means <- sapply(industry_cols, function(x) mean(prono_data[[x]]))
industry_sds <- sapply(industry_cols, function(x) sd(prono_data[[x]]))

cat(sprintf("   Mean returns range: [%.3f%%, %.3f%%]\n",
            min(industry_means), max(industry_means)))
cat(sprintf("   SD range: [%.3f%%, %.3f%%]\n",
            min(industry_sds), max(industry_sds)))

# Find specific industries (guessing based on position)
cat("\n   Selected Industries:\n")
# RI10 might be Tech, RI23 might be Utilities
cat(sprintf("   RI10 - Mean: %.3f%%, SD: %.3f%%\n",
            mean(prono_data$RI10), sd(prono_data$RI10)))
cat(sprintf("   RI23 - Mean: %.3f%%, SD: %.3f%%\n",
            mean(prono_data$RI23), sd(prono_data$RI23)))

# 6. Correlations
cat("\n6. Key Correlations:\n")
cat(sprintf("   Cor(rm, smb): %.3f\n", cor(prono_data$rm, prono_data$smb)))
cat(sprintf("   Cor(rm, hml): %.3f\n", cor(prono_data$rm, prono_data$hml)))
cat(sprintf("   Cor(smb, hml): %.3f\n", cor(prono_data$smb, prono_data$hml)))

# 7. GARCH effects check
cat("\n7. Testing for GARCH Effects (Market Returns):\n")
market_resid <- market_excess - mean(market_excess)
squared_resid <- market_resid^2

# Ljung-Box test for squared residuals
lb_test <- Box.test(squared_resid, lag = 10, type = "Ljung-Box")
cat(sprintf("   Ljung-Box test on squared residuals (lag 10):\n"))
cat(sprintf("   Statistic: %.3f, p-value: %.4f\n",
            lb_test$statistic, lb_test$p.value))

# ACF of squared residuals
acf_sq <- acf(squared_resid, lag.max = 10, plot = FALSE)
cat("   ACF of squared residuals:\n")
for (i in 1:5) {
  cat(sprintf("     Lag %d: %.3f\n", i, acf_sq$acf[i + 1]))
}

cat("\n=== COMPARISON WITH OUR IMPLEMENTATION ===\n")
cat("Prono's actual data:\n")
cat(sprintf("  Market excess return mean: %.3f%%\n", mean(market_excess)))
cat(sprintf("  Market excess return SD: %.3f%%\n", sd(market_excess)))
cat("\nOur simulated data:\n")
cat("  Target mean: 0.097%\n")
cat("  Target SD: ~2%\n")
cat(sprintf("\nDifference in mean: %.3f%%\n", mean(market_excess) - 0.097))
cat(sprintf("SD ratio: %.2f\n", sd(market_excess) / 2.0))

cat("\nAutocorrelation of Squared Market Returns (Ljung-Box test p-value):\n")
# Test for ARCH effects using Ljung-Box on squared residuals/returns
lb_test_sq <- Box.test(squared_resid, lag = 10, type = "Ljung-Box")
cat(sprintf("   Ljung-Box Q(10) on squared returns: %.3f (p-value: %.4f)\n",
            lb_test_sq$statistic, lb_test_sq$p.value))
if (lb_test_sq$p.value < 0.05) {
  cat("   Evidence of ARCH effects (GARCH likely present).\n")
} else {
  cat("   No strong evidence of ARCH effects at 10 lags.\n")
}
acf_sq <- acf(squared_resid, plot = FALSE, lag.max = 5)
cat("   ACF of squared returns (lags 1-5):\n")
for (i in 1:5) {
  cat(sprintf("     Lag %d: %.3f\n", i, acf_sq$acf[i + 1]))
}

cat("\n--- Analysis Summary ---")
