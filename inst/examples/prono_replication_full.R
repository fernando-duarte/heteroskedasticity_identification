# Full Replication of Prono (2014) Results
# This script uses our package with diagonal GARCH to replicate Prono's results

# Load required packages
required_packages <- c("rugarch", "ivreg", "AER", "gmm")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Package", pkg, "is required but not installed.\n")
    cat("Please install it with: install.packages('", pkg, "')\n", sep = "")
  } else {
    library(pkg, character.only = TRUE)
  }
}

# Source the package functions
source("R/lewbel-gmm.R")
source("R/prono.R")
source("R/prono-diagonal-garch.R")
source("R/utils.R")

cat("=== FULL REPLICATION OF PRONO (2014) ===\n\n")

# Part 1: Load and prepare Prono's actual data
cat("PART 1: LOADING PRONO'S ACTUAL DATA\n")
cat("=====================================\n\n")

# Read Prono's data file - use direct path
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

# Convert date
prono_data$DATE <- as.Date(as.character(prono_data$DATE), format = "%Y%m%d")

# Create excess returns
prono_data$mkt_rf <- prono_data$rm - prono_data$RF

# Create example portfolio excess return (small-value portfolio)
prono_data$port_rf <- prono_data$R15 - prono_data$RF

cat("Data loaded successfully\n")
cat("Date range:", format(min(prono_data$DATE)), "to", format(max(prono_data$DATE)), "\n")
cat("Number of observations:", nrow(prono_data), "\n\n")

# Part 2: Prepare data for our functions
cat("PART 2: PREPARING DATA FOR ANALYSIS\n")
cat("===================================\n\n")

# Create data frame in our format
analysis_data <- data.frame(
  Y1 = prono_data$port_rf,  # Portfolio excess return
  Y2 = prono_data$mkt_rf,    # Market excess return
  X1 = prono_data$smb,       # Size factor
  X2 = prono_data$hml        # Value factor
)

# Remove any missing values
analysis_data <- na.omit(analysis_data)
n_obs <- nrow(analysis_data)

cat("Prepared data:\n")
cat("  Y1 (Portfolio excess): Mean =", sprintf("%.3f%%", mean(analysis_data$Y1)),
    "SD =", sprintf("%.3f%%", sd(analysis_data$Y1)), "\n")
cat("  Y2 (Market excess): Mean =", sprintf("%.3f%%", mean(analysis_data$Y2)),
    "SD =", sprintf("%.3f%%", sd(analysis_data$Y2)), "\n")
cat("  Number of observations:", n_obs, "\n\n")

# Part 3: Standard 2SLS estimation (for comparison)
cat("PART 3: STANDARD 2SLS ESTIMATION\n")
cat("================================\n\n")

# OLS (biased)
ols_fit <- lm(Y1 ~ Y2 + X1 + X2, data = analysis_data)
cat("OLS Results:\n")
print(summary(ols_fit)$coefficients["Y2", ])

# Standard IV using Fama-French factors as instruments
if (requireNamespace("ivreg", quietly = TRUE)) {
  iv_std <- ivreg::ivreg(Y1 ~ Y2 + X1 + X2 | X1 + X2, data = analysis_data)
  cat("\nStandard IV Results (using only X as instruments):\n")
  print(summary(iv_std)$coefficients["Y2", ])
}

# Part 4: Prono method with univariate GARCH
cat("\n\nPART 4: PRONO METHOD WITH UNIVARIATE GARCH\n")
cat("==========================================\n\n")

# Run single simulation on actual data
prono_univ <- run_single_prono_simulation(
  n = n_obs,
  data = analysis_data,
  return_details = TRUE
)

cat("Prono IV with Univariate GARCH:\n")
cat("  Coefficient:", sprintf("%.4f", prono_univ$gamma1_iv), "\n")
cat("  Std Error:", sprintf("%.4f", prono_univ$se_iv), "\n")
cat("  t-statistic:", sprintf("%.4f", prono_univ$gamma1_iv / prono_univ$se_iv), "\n")
cat("  First-stage F-stat:", sprintf("%.2f", prono_univ$f_stat), "\n")

# Part 5: Prono method with diagonal GARCH
cat("\n\nPART 5: PRONO METHOD WITH DIAGONAL GARCH\n")
cat("========================================\n\n")

tryCatch({
  prono_diag <- prono_diagonal_garch(
    analysis_data,
    method = "2sls",
    garch_order = c(1, 1),
    verbose = TRUE
  )

  cat("\nProno IV with Diagonal GARCH:\n")
  cat("  Coefficient:", sprintf("%.4f", prono_diag$gamma1_iv), "\n")
  cat("  Std Error:", sprintf("%.4f", prono_diag$se_iv), "\n")
  cat("  t-statistic:", sprintf("%.4f", prono_diag$gamma1_iv / prono_diag$se_iv), "\n")
  cat("  First-stage F-stat:", sprintf("%.2f", prono_diag$f_stat), "\n")

}, error = function(e) {
  cat("Error with diagonal GARCH:", e$message, "\n")
  cat("This may require installing tsmarch package\n")
})

# Part 6: GMM estimation
cat("\n\nPART 6: GMM ESTIMATION\n")
cat("=====================\n\n")

# Add GARCH variance to data for GMM
if (requireNamespace("rugarch", quietly = TRUE)) {
  # Fit GARCH to market excess return
  garch_spec <- rugarch::ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = FALSE)
  )
  garch_fit <- rugarch::ugarchfit(garch_spec, analysis_data$Y2)
  analysis_data$sigma2_sq_hat <- as.numeric(rugarch::sigma(garch_fit))^2

  # GMM estimation
  gmm_result <- prono_gmm(
    analysis_data,
    gmm_type = "twoStep",
    fit_garch = FALSE,
    verbose = FALSE
  )

  cat("GMM Two-Step Results:\n")
  print(summary(gmm_result))

  # Try CUE
  cue_result <- prono_gmm(
    analysis_data,
    gmm_type = "cue",
    fit_garch = FALSE,
    verbose = FALSE
  )

  cat("\nGMM CUE Results:\n")
  print(summary(cue_result))
}

# Part 7: Comparison table
cat("\n\nPART 7: SUMMARY COMPARISON\n")
cat("==========================\n\n")

results_table <- data.frame(
  Method = c("OLS", "Standard IV", "Prono IV (Univariate)", "Prono IV (Diagonal)",
             "GMM Two-Step", "GMM CUE"),
  Coefficient = c(
    coef(ols_fit)["Y2"],
    ifelse(exists("iv_std"), coef(iv_std)["Y2"], NA),
    prono_univ$gamma1_iv,
    ifelse(exists("prono_diag"), prono_diag$gamma1_iv, NA),
    ifelse(exists("gmm_result"), coef(gmm_result)["gamma1"], NA),
    ifelse(exists("cue_result"), coef(cue_result)["gamma1"], NA)
  ),
  Std_Error = c(
    summary(ols_fit)$coefficients["Y2", "Std. Error"],
    ifelse(exists("iv_std"), summary(iv_std)$coefficients["Y2", "Std. Error"], NA),
    prono_univ$se_iv,
    ifelse(exists("prono_diag"), prono_diag$se_iv, NA),
    ifelse(exists("gmm_result"), sqrt(diag(vcov(gmm_result)))["gamma1"], NA),
    ifelse(exists("cue_result"), sqrt(diag(vcov(cue_result)))["gamma1"], NA)
  )
)

results_table$t_statistic <- results_table$Coefficient / results_table$Std_Error

print(results_table, digits = 4)

# Part 8: Monte Carlo replication
cat("\n\nPART 8: MONTE CARLO REPLICATION OF TABLE II\n")
cat("===========================================\n\n")

cat("Running Monte Carlo simulation (this may take a few minutes)...\n")

# Create configuration matching data characteristics
config <- create_prono_config(
  n = 500,
  beta2 = c(mean(analysis_data$Y2), 0),  # Match actual mean
  sigma1 = sd(analysis_data$Y1),
  garch_params = list(omega = 0.2, alpha = 0.1, beta = 0.85)
)

# Run smaller Monte Carlo for demonstration
mc_results <- replicate_prono_table2(
  n_sim = 100,  # Reduced for speed
  n_obs = 500,
  config = config,
  use_diagonal_garch = FALSE,  # Start with univariate
  verbose = TRUE
)

cat("\n\nMonte Carlo Results (100 simulations):\n")
print(mc_results$summary, digits = 4)

cat("\n\n=== REPLICATION COMPLETE ===\n")
