# Test Prono implementation on actual data
# This script tests our functions on Prono's actual data

# Check for required packages
if (!requireNamespace("rugarch", quietly = TRUE)) {
  stop("Please install rugarch package: install.packages('rugarch')")
}

# Source the functions we need
source("R/utils.R")
source("R/prono.R")
source("R/lewbel-gmm.R")
source("R/prono-diagonal-garch.R")

cat("=== TESTING PRONO IMPLEMENTATION ON ACTUAL DATA ===\n\n")

# Load Prono's data
cat("Loading Prono's data...\n")
data_file <- "lewbel2012/tp-files/ff_sze_bm_ind_Prono2013.txt"
prono_data <- read.table(data_file, header = FALSE)

# Assign column names
col_names <- c("DATE", "rm", "smb", "hml", "RF")
for (i in 1:5) {
  for (j in 1:5) {
    col_names <- c(col_names, paste0("R", i, j))
  }
}
for (k in 1:30) {
  col_names <- c(col_names, paste0("RI", k))
}
colnames(prono_data) <- col_names

# Convert date and create excess returns
prono_data$DATE <- as.Date(as.character(prono_data$DATE), format = "%Y%m%d")
prono_data$mkt_rf <- prono_data$rm - prono_data$RF
prono_data$port_rf <- prono_data$R15 - prono_data$RF  # Small-value portfolio

cat("Data loaded. Sample size:", nrow(prono_data), "\n")
cat("Date range:", format(min(prono_data$DATE)), "to", format(max(prono_data$DATE)), "\n\n")

# Prepare data in our format
analysis_data <- data.frame(
  Y1 = prono_data$port_rf,
  Y2 = prono_data$mkt_rf,
  X1 = prono_data$smb,
  X2 = prono_data$hml
)
analysis_data <- na.omit(analysis_data)

# Display summary statistics
cat("Summary Statistics:\n")
cat("Y1 (Portfolio excess return): Mean =", sprintf("%.3f%%", mean(analysis_data$Y1)),
    "SD =", sprintf("%.3f%%", sd(analysis_data$Y1)), "\n")
cat("Y2 (Market excess return): Mean =", sprintf("%.3f%%", mean(analysis_data$Y2)),
    "SD =", sprintf("%.3f%%", sd(analysis_data$Y2)), "\n\n")

# Test 1: OLS (biased)
cat("TEST 1: OLS ESTIMATION\n")
cat("=====================\n")
ols_fit <- lm(Y1 ~ Y2 + X1 + X2, data = analysis_data)
ols_coef <- coef(ols_fit)["Y2"]
ols_se <- summary(ols_fit)$coefficients["Y2", "Std. Error"]
cat("Coefficient on Y2:", sprintf("%.4f", ols_coef), "\n")
cat("Standard error:", sprintf("%.4f", ols_se), "\n")
cat("t-statistic:", sprintf("%.2f", ols_coef/ols_se), "\n\n")

# Test 2: Prono method with univariate GARCH
cat("TEST 2: PRONO METHOD WITH UNIVARIATE GARCH\n")
cat("==========================================\n")

tryCatch({
  # Run Prono's method
  prono_result <- run_single_prono_simulation(
    n = nrow(analysis_data),
    data = analysis_data,
    return_details = TRUE
  )

  cat("Coefficient on Y2:", sprintf("%.4f", prono_result$gamma1_iv), "\n")
  cat("Standard error:", sprintf("%.4f", prono_result$se_iv), "\n")
  cat("t-statistic:", sprintf("%.2f", prono_result$gamma1_iv / prono_result$se_iv), "\n")
  cat("First-stage F-stat:", sprintf("%.2f", prono_result$f_stat), "\n")
  cat("Bias reduction:", sprintf("%.1f%%",
      100 * (1 - abs(prono_result$bias_iv) / abs(prono_result$bias_ols))), "\n\n")

}, error = function(e) {
  cat("Error:", e$message, "\n\n")
})

# Test 3: GMM estimation
cat("TEST 3: PRONO GMM ESTIMATION\n")
cat("============================\n")

tryCatch({
  # First fit GARCH to get conditional variance
  y2_resid <- lm(Y2 ~ X1 + X2, data = analysis_data)$residuals

  garch_spec <- rugarch::ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE)
  )

  garch_fit <- rugarch::ugarchfit(garch_spec, y2_resid)
  analysis_data$sigma2_sq_hat <- as.numeric(rugarch::sigma(garch_fit))^2

  # Run GMM
  gmm_result <- prono_gmm(
    analysis_data,
    gmm_type = "twoStep",
    fit_garch = FALSE,
    verbose = FALSE
  )

  gmm_coef <- coef(gmm_result)["gamma1"]
  gmm_se <- sqrt(vcov(gmm_result)["gamma1", "gamma1"])

  cat("GMM coefficient on Y2:", sprintf("%.4f", gmm_coef), "\n")
  cat("GMM standard error:", sprintf("%.4f", gmm_se), "\n")
  cat("GMM t-statistic:", sprintf("%.2f", gmm_coef/gmm_se), "\n")

  if (!is.null(gmm_result$J_test)) {
    cat("J-test statistic:", sprintf("%.2f", gmm_result$J_test$J_stat), "\n")
    cat("J-test p-value:", sprintf("%.4f", gmm_result$J_test$p_value), "\n")
  }

}, error = function(e) {
  cat("Error in GMM:", e$message, "\n\n")
})

# Summary comparison
cat("\n=== SUMMARY COMPARISON ===\n")
cat("Method              Coefficient    Std Error    t-stat\n")
cat("-------------------------------------------------------\n")
cat(sprintf("OLS                 %8.4f      %8.4f    %6.2f\n",
    ols_coef, ols_se, ols_coef/ols_se))

if (exists("prono_result")) {
  cat(sprintf("Prono IV            %8.4f      %8.4f    %6.2f\n",
      prono_result$gamma1_iv, prono_result$se_iv,
      prono_result$gamma1_iv / prono_result$se_iv))
}

if (exists("gmm_coef")) {
  cat(sprintf("Prono GMM           %8.4f      %8.4f    %6.2f\n",
      gmm_coef, gmm_se, gmm_coef/gmm_se))
}

cat("\nNote: Y1 = Small-value portfolio excess return\n")
cat("      Y2 = Market excess return\n")
cat("      True market excess return mean = -0.022% (not 0.097% as reported)\n")
