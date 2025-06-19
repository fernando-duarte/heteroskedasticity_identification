# Replication of Prono (2014) Empirical Results
# This script attempts to replicate the asset pricing results from
# "The Role of Conditional Heteroskedasticity in Identifying and
# Estimating Linear Triangular Systems" by Todd Prono (JAE, 2014)

# Load required packages
library(hetid)
library(tidyverse)
library(lubridate)

# Note: This script uses publicly available data. Prono uses CRSP data
# which requires a subscription. We use Fama-French data as a proxy.

# ========================================================================
# STEP 1: DOWNLOAD DATA
# ========================================================================

# Check if frenchdata is installed
if (!requireNamespace("frenchdata", quietly = TRUE)) {
  message("Installing frenchdata package...")
  install.packages("frenchdata")
}

library(frenchdata)

# Download Fama-French 3 factors (includes market return)
message("Downloading Fama-French 3 factors...")
ff3_factors <- download_french_data("Fama/French 3 Factors")

# Extract weekly data
ff3_weekly <- ff3_factors$subsets$weekly$data

# Download 25 Size-B/M portfolios
message("Downloading 25 Size-B/M portfolios...")
ff25_portfolios <- download_french_data("25 Portfolios Formed on Size and Book-to-Market")
ff25_weekly <- ff25_portfolios$subsets$weekly$data

# Download 30 Industry portfolios
message("Downloading 30 Industry portfolios...")
ff30_industries <- download_french_data("30 Industry Portfolios")
ff30_weekly <- ff30_industries$subsets$weekly$data

# ========================================================================
# STEP 2: PREPARE DATA
# ========================================================================

# Convert dates and filter to Prono's period (July 5, 1963 to Dec 31, 2004)
start_date <- as.Date("1963-07-05")
end_date <- as.Date("2004-12-31")

# Function to prepare weekly data
prepare_weekly_data <- function(data, start_date, end_date) {
  data %>%
    mutate(date = as.Date(row.names(.))) %>%
    filter(date >= start_date & date <= end_date) %>%
    arrange(date)
}

# Prepare factor data
factors_data <- prepare_weekly_data(ff3_weekly, start_date, end_date)

# Prepare portfolio data
portfolios_25 <- prepare_weekly_data(ff25_weekly, start_date, end_date)
portfolios_30 <- prepare_weekly_data(ff30_weekly, start_date, end_date)

# Check data availability
cat("\nData Summary:\n")
cat("Factors data: ", nrow(factors_data), "weeks\n")
cat("25 portfolios: ", nrow(portfolios_25), "weeks\n")
cat("30 industry portfolios: ", nrow(portfolios_30), "weeks\n")
cat("Expected (from paper): 2166 weeks\n")

# ========================================================================
# STEP 3: REPLICATE FIRST-PASS TIME SERIES REGRESSIONS
# ========================================================================

# Function to estimate market beta using Prono method for one portfolio
estimate_prono_beta <- function(portfolio_returns, market_returns, rf_rate) {

  # Create excess returns
  Y1 <- portfolio_returns - rf_rate  # Portfolio excess return
  Y2 <- market_returns - rf_rate     # Market excess return

  n <- length(Y1)

  # Skip if too few observations
  if (n < 100) return(NULL)

  # OLS estimation (for comparison)
  ols_fit <- lm(Y1 ~ Y2)
  beta_ols <- coef(ols_fit)[2]
  alpha_ols <- coef(ols_fit)[1]

  # Prono estimation
  tryCatch({
    # Get residuals from market model (second equation)
    e2 <- residuals(lm(Y2 ~ 1))

    # Check if rugarch is available for GARCH estimation
    if (requireNamespace("rugarch", quietly = TRUE)) {
      # Fit GARCH(1,1) to market residuals
      spec <- rugarch::ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
        mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
        distribution.model = "norm"
      )

      garch_fit <- rugarch::ugarchfit(spec = spec, data = e2, solver = "hybrid")

      # Extract conditional variance
      sigma2_sq <- as.numeric(rugarch::sigma(garch_fit))^2

      # Construct Prono instrument: Z * e2
      Z <- sigma2_sq - mean(sigma2_sq)
      instrument <- Z * e2

      # 2SLS with Prono instrument
      # First stage: Regress Y2 on instrument
      first_stage <- lm(Y2 ~ instrument)
      Y2_fitted <- fitted(first_stage)
      f_stat <- summary(first_stage)$fstatistic[1]

      # Second stage: Regress Y1 on fitted Y2
      second_stage <- lm(Y1 ~ Y2_fitted)
      beta_prono <- coef(second_stage)[2]
      alpha_prono <- coef(second_stage)[1]

      # Get GARCH parameters for reporting
      garch_params <- coef(garch_fit)

      return(list(
        alpha_ols = alpha_ols,
        beta_ols = beta_ols,
        alpha_prono = alpha_prono,
        beta_prono = beta_prono,
        f_stat = f_stat,
        garch_omega = garch_params["omega"],
        garch_alpha = garch_params["alpha1"],
        garch_beta = garch_params["beta1"],
        n_obs = n
      ))

    } else {
      # Fallback: use squared residuals as proxy
      Z <- e2^2 - mean(e2^2)
      instrument <- Z * e2

      first_stage <- lm(Y2 ~ instrument)
      Y2_fitted <- fitted(first_stage)
      f_stat <- summary(first_stage)$fstatistic[1]

      second_stage <- lm(Y1 ~ Y2_fitted)
      beta_prono <- coef(second_stage)[2]
      alpha_prono <- coef(second_stage)[1]

      return(list(
        alpha_ols = alpha_ols,
        beta_ols = beta_ols,
        alpha_prono = alpha_prono,
        beta_prono = beta_prono,
        f_stat = f_stat,
        n_obs = n,
        note = "GARCH not available, used squared residuals"
      ))
    }

  }, error = function(e) {
    return(list(
      alpha_ols = alpha_ols,
      beta_ols = beta_ols,
      error = e$message
    ))
  })
}

# ========================================================================
# STEP 4: RUN FIRST-PASS REGRESSIONS FOR SELECT PORTFOLIOS
# ========================================================================

# Merge data
analysis_data <- factors_data %>%
  select(date, mkt_excess = `Mkt-RF`, rf = RF, smb = SMB, hml = HML)

# Example: Replicate results for specific portfolios mentioned in paper
# Table VI shows results for Size 1, B/M 1 portfolio and Industry 21 (Machinery)

# Get Small-Low (Size 1, B/M 1) portfolio
small_low <- portfolios_25 %>%
  select(date, portfolio_return = `SMALL LoBM`) %>%
  inner_join(analysis_data, by = "date")

cat("\n\nEstimating Prono betas for Size 1, B/M 1 portfolio...\n")
sl_results <- estimate_prono_beta(
  small_low$portfolio_return,
  small_low$mkt_excess + small_low$rf,  # Add back rf to get total market return
  small_low$rf
)

if (!is.null(sl_results)) {
  cat("\nSize 1, B/M 1 Portfolio Results:\n")
  cat(sprintf("Alpha (OLS): %.4f, Beta (OLS): %.4f\n",
              sl_results$alpha_ols, sl_results$beta_ols))
  cat(sprintf("Alpha (Prono): %.4f, Beta (Prono): %.4f\n",
              sl_results$alpha_prono, sl_results$beta_prono))
  cat(sprintf("First-stage F-stat: %.2f\n", sl_results$f_stat))
  if (!is.null(sl_results$garch_omega)) {
    cat(sprintf("GARCH params: omega=%.4f, alpha=%.4f, beta=%.4f\n",
                sl_results$garch_omega, sl_results$garch_alpha, sl_results$garch_beta))
  }
}

# ========================================================================
# STEP 5: ESTIMATE FOR ALL 25 PORTFOLIOS (CAPM)
# ========================================================================

cat("\n\nEstimating betas for all 25 Size-B/M portfolios...\n")

# Get all portfolio names (excluding date column)
portfolio_names <- setdiff(names(portfolios_25), "date")

# Store results
all_results <- list()

# Progress counter
pb <- txtProgressBar(min = 0, max = length(portfolio_names), style = 3)

for (i in seq_along(portfolio_names)) {
  port_name <- portfolio_names[i]

  # Get portfolio data
  port_data <- portfolios_25 %>%
    select(date, portfolio_return = all_of(port_name)) %>%
    inner_join(analysis_data, by = "date")

  # Estimate betas
  results <- estimate_prono_beta(
    port_data$portfolio_return,
    port_data$mkt_excess + port_data$rf,
    port_data$rf
  )

  if (!is.null(results)) {
    results$portfolio <- port_name
    all_results[[port_name]] <- results
  }

  setTxtProgressBar(pb, i)
}
close(pb)

# Convert to data frame
results_df <- bind_rows(all_results)

# ========================================================================
# STEP 6: SECOND-PASS CROSS-SECTIONAL REGRESSION
# ========================================================================

cat("\n\nRunning second-pass cross-sectional regressions...\n")

# Calculate average excess returns for each portfolio
avg_returns <- portfolios_25 %>%
  select(-date) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "portfolio", values_to = "avg_return")

# Merge with beta estimates
cross_section_data <- results_df %>%
  select(portfolio, beta_ols, beta_prono) %>%
  inner_join(avg_returns, by = "portfolio")

# Cross-sectional regression: OLS betas
cs_ols <- lm(avg_return ~ beta_ols, data = cross_section_data)
cat("\nCross-sectional regression with OLS betas:\n")
print(summary(cs_ols))

# Cross-sectional regression: Prono betas
cs_prono <- lm(avg_return ~ beta_prono, data = cross_section_data)
cat("\nCross-sectional regression with Prono betas:\n")
print(summary(cs_prono))

# ========================================================================
# STEP 7: COMPARE WITH PRONO'S RESULTS
# ========================================================================

cat("\n\n=== COMPARISON WITH PRONO (2014) ===\n")
cat("\nProno reports in Table VII (CAPM, FF25 portfolios):\n")
cat("OLS: Constant = 0.100 (0.042), Market premium = 0.016 (0.059)\n")
cat("CUE: Constant = 0.087 (0.040), Market premium = 0.033 (0.057)\n")

cat("\nOur replication:\n")
cat(sprintf("OLS: Constant = %.3f (%.3f), Market premium = %.3f (%.3f)\n",
            coef(cs_ols)[1], summary(cs_ols)$coefficients[1,2],
            coef(cs_ols)[2], summary(cs_ols)$coefficients[2,2]))
cat(sprintf("Prono: Constant = %.3f (%.3f), Market premium = %.3f (%.3f)\n",
            coef(cs_prono)[1], summary(cs_prono)$coefficients[1,2],
            coef(cs_prono)[2], summary(cs_prono)$coefficients[2,2]))

# Calculate average market excess return
avg_mkt_excess <- mean(analysis_data$mkt_excess, na.rm = TRUE)
cat(sprintf("\nAverage weekly market excess return: %.3f%%\n", avg_mkt_excess))
cat("(The market premium should be close to this value)\n")

# ========================================================================
# STEP 8: DIAGNOSTICS
# ========================================================================

# Plot beta comparison
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  p <- ggplot(cross_section_data, aes(x = beta_ols, y = beta_prono)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(title = "Comparison of Beta Estimates",
         subtitle = "25 Size-B/M Portfolios",
         x = "OLS Beta",
         y = "Prono Beta") +
    theme_minimal()

  print(p)

  # Plot cross-sectional fit
  p2 <- ggplot(cross_section_data, aes(x = beta_prono, y = avg_return)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    labs(title = "Cross-Sectional Regression: Prono Betas",
         x = "Prono Beta",
         y = "Average Excess Return (%)") +
    theme_minimal()

  print(p2)
}

# Summary statistics
cat("\n\nSummary Statistics:\n")
cat("Beta estimates:\n")
cat("OLS betas: mean =", mean(results_df$beta_ols), ", sd =", sd(results_df$beta_ols), "\n")
cat("Prono betas: mean =", mean(results_df$beta_prono), ", sd =", sd(results_df$beta_prono), "\n")
cat("Correlation between OLS and Prono betas:", cor(results_df$beta_ols, results_df$beta_prono), "\n")

cat("\nFirst-stage F-statistics:\n")
cat("Mean:", mean(results_df$f_stat, na.rm = TRUE), "\n")
cat("Median:", median(results_df$f_stat, na.rm = TRUE), "\n")
cat("% with F > 10:", mean(results_df$f_stat > 10, na.rm = TRUE) * 100, "%\n")

# ========================================================================
# LIMITATIONS OF THIS REPLICATION
# ========================================================================

cat("\n\n=== LIMITATIONS ===\n")
cat("1. Data source: We use Fama-French data instead of CRSP\n")
cat("2. Frequency: The frenchdata package may not have exact weekly alignment\n")
cat("3. Implementation: Our Prono method is simplified (standard GARCH vs diagonal)\n")
cat("4. Standard errors: We use basic SEs, not Shanken (1992) corrected\n")
cat("5. GMM: We use 2SLS instead of full GMM implementation\n")

cat("\nDespite these limitations, this replication demonstrates:\n")
cat("- The basic mechanics of the Prono method\n")
cat("- How to apply it to real asset pricing data\n")
cat("- The potential for bias reduction in beta estimation\n")
