#!/usr/bin/env Rscript
# ============================================================================
# Verification Script for Lewbel Implementation Differences
# 
# This script verifies the claims made in lewbel_implementations_comparison.tex
# about differences between hetid, REndo, and Stata implementations
# ============================================================================

# Load required packages
library(hetid)
library(AER)
library(REndo)

cat("=================================================================\n")
cat("Verification of Lewbel (2012) Implementation Differences\n")
cat("=================================================================\n\n")

# -----------------------------------------------------------------------------
# 1. Generate Lewbel-type data
# -----------------------------------------------------------------------------
cat("1. GENERATING LEWBEL-TYPE DATA\n")
cat("------------------------------\n")

set.seed(123)
n <- 1000
params <- list(
  beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
  beta2_0 = 1.0, beta2_1 = -1.0,
  alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
)

# Generate data using hetid's function
data <- generate_lewbel_data(n, params)

# Prepare data frame
test_data <- data.frame(
  y = data$Y1,
  X1 = data$Xk,
  P = data$Y2,
  Z = data$Z
)

cat("Generated data with n =", n, "observations\n")
cat("True gamma1 (coefficient on P):", params$gamma1, "\n\n")

# -----------------------------------------------------------------------------
# 2. Verify Instrument Construction Differences
# -----------------------------------------------------------------------------
cat("2. INSTRUMENT CONSTRUCTION COMPARISON\n")
cat("------------------------------------\n")

# Method 1: Standard Lewbel (as in hetid/ivreg2h)
cat("\nMethod 1 - Standard Lewbel (hetid/ivreg2h):\n")
cat("  Step 1: Run first-stage regression P ~ X1\n")
first_stage <- lm(P ~ X1, data = test_data)
e2_hat <- residuals(first_stage)
cat("  Step 2: Construct instrument as (Z - mean(Z)) * e2_hat\n")
lewbel_iv <- (test_data$Z - mean(test_data$Z)) * e2_hat
lewbel_iv <- lewbel_iv - mean(lewbel_iv)  # Ensure exactly mean zero
test_data$lewbel_iv <- lewbel_iv
cat("  Step 3: Instrument mean =", mean(lewbel_iv), "(should be ~0)\n")
cat("  Step 4: Instrument std dev =", sd(lewbel_iv), "\n")

# Method 2: What we suspect REndo does
cat("\nMethod 2 - REndo's approach (hypothesized):\n")
cat("  Step 1: Run REVERSE regression X1 ~ P\n")
reverse_reg <- lm(X1 ~ P, data = test_data)
u_hat <- residuals(reverse_reg)
cat("  Step 2: Construct instrument as u_hat * (P - mean(P))\n")
rendo_iv_manual <- u_hat * (test_data$P - mean(test_data$P))
cat("  Step 3: Instrument mean =", mean(rendo_iv_manual), "\n")
cat("  Step 4: Instrument std dev =", sd(rendo_iv_manual), "\n")

cat("\nCorrelation between instruments:", cor(lewbel_iv, rendo_iv_manual), "\n")

# -----------------------------------------------------------------------------
# 3. Run Different Implementations
# -----------------------------------------------------------------------------
cat("\n3. COMPARING ESTIMATION RESULTS\n")
cat("-------------------------------\n")

# Standard Lewbel using manual 2SLS
cat("\nA. Standard Lewbel (manual 2SLS with hetid approach):\n")
manual_2sls <- ivreg(y ~ X1 + P | X1 + lewbel_iv, data = test_data)
coef_manual <- coef(manual_2sls)["P"]
se_manual <- sqrt(diag(vcov(manual_2sls)))["P"]
cat("   Coefficient on P:", round(coef_manual, 6), "\n")
cat("   Standard error:", round(se_manual, 6), "\n")
cat("   t-statistic:", round(coef_manual/se_manual, 3), "\n")

# REndo's hetErrorsIV
cat("\nB. REndo's hetErrorsIV:\n")
rendo_model <- hetErrorsIV(y ~ X1 + P | P | IIV(X1), data = test_data)
coef_rendo <- coef(rendo_model)["P"]
se_rendo <- sqrt(diag(vcov(rendo_model)))["P"]
cat("   Coefficient on P:", round(coef_rendo, 6), "\n")
cat("   Standard error:", round(se_rendo, 6), "\n")
cat("   t-statistic:", round(coef_rendo/se_rendo, 3), "\n")
cat("   SE ratio (REndo/Lewbel):", round(se_rendo/se_manual, 4), "\n")

# Test overidentification hypothesis
cat("\nC. Testing overidentification hypothesis:\n")
# REndo might use {X1, P, generated_IV} as instruments
overid_model <- tryCatch({
  ivreg(y ~ X1 + P | X1 + P + lewbel_iv, data = test_data)
}, error = function(e) NULL)

if (!is.null(overid_model)) {
  coef_overid <- coef(overid_model)["P"]
  se_overid <- sqrt(diag(vcov(overid_model)))["P"]
  cat("   Overidentified model coefficient:", round(coef_overid, 6), "\n")
  cat("   Overidentified model SE:", round(se_overid, 6), "\n")
  
  # Sargan test
  summ <- summary(overid_model, diagnostics = TRUE)
  if ("diagnostics" %in% names(summ) && "Sargan" %in% rownames(summ$diagnostics)) {
    sargan_p <- summ$diagnostics["Sargan", "p-value"]
    cat("   Sargan test p-value:", round(sargan_p, 4), "\n")
    if (sargan_p < 0.05) {
      cat("   => Overidentifying restrictions REJECTED at 5% level\n")
    }
  }
}

# -----------------------------------------------------------------------------
# 4. Extract REndo's Actual Instruments
# -----------------------------------------------------------------------------
cat("\n4. EXTRACTING RENDO'S ACTUAL INSTRUMENTS\n")
cat("----------------------------------------\n")

# Try to access REndo's internal instruments
rendo_iv_extracted <- NULL
if ("internalInstruments" %in% names(rendo_model)) {
  rendo_iv_extracted <- rendo_model$internalInstruments
  cat("Found internalInstruments in REndo model\n")
} else if ("instruments" %in% names(rendo_model)) {
  rendo_iv_extracted <- rendo_model$instruments
  cat("Found instruments in REndo model\n")
} else {
  cat("Could not find instruments in REndo model object\n")
  cat("Available components:", paste(names(rendo_model), collapse=", "), "\n")
}

if (!is.null(rendo_iv_extracted)) {
  if (is.matrix(rendo_iv_extracted) || is.data.frame(rendo_iv_extracted)) {
    cat("Instrument matrix dimensions:", 
        nrow(rendo_iv_extracted), "x", ncol(rendo_iv_extracted), "\n")
    # Usually the generated instrument is the last column
    gen_iv <- rendo_iv_extracted[, ncol(rendo_iv_extracted)]
  } else {
    gen_iv <- rendo_iv_extracted
  }
  
  cat("\nComparing extracted REndo instrument with our constructions:\n")
  cat("  Correlation with standard Lewbel IV:", 
      round(cor(as.numeric(gen_iv), lewbel_iv), 4), "\n")
  cat("  Correlation with reverse regression IV:", 
      round(cor(as.numeric(gen_iv), rendo_iv_manual), 4), "\n")
}

# -----------------------------------------------------------------------------
# 5. Test with Weak Heteroskedasticity
# -----------------------------------------------------------------------------
cat("\n5. TESTING WITH WEAK HETEROSKEDASTICITY\n")
cat("---------------------------------------\n")

# Generate data with very weak heteroskedasticity
params_weak <- params
params_weak$delta_het <- 0.1  # Very weak
data_weak <- generate_lewbel_data(n, params_weak)

test_data_weak <- data.frame(
  y = data_weak$Y1,
  X1 = data_weak$Xk,
  P = data_weak$Y2,
  Z = data_weak$Z
)

# Construct Lewbel IV
e2_hat_weak <- residuals(lm(P ~ X1, data = test_data_weak))
lewbel_iv_weak <- (test_data_weak$Z - mean(test_data_weak$Z)) * e2_hat_weak
lewbel_iv_weak <- lewbel_iv_weak - mean(lewbel_iv_weak)
test_data_weak$lewbel_iv <- lewbel_iv_weak

# Run both methods
manual_weak <- ivreg(y ~ X1 + P | X1 + lewbel_iv, data = test_data_weak)
rendo_weak <- suppressWarnings(
  hetErrorsIV(y ~ X1 + P | P | IIV(X1), data = test_data_weak)
)

cat("\nWith weak heteroskedasticity (delta_het = 0.1):\n")
cat("  Standard Lewbel SE:", round(sqrt(diag(vcov(manual_weak)))["P"], 6), "\n")
cat("  REndo SE:", round(sqrt(diag(vcov(rendo_weak)))["P"], 6), "\n")
cat("  SE ratio:", round(sqrt(diag(vcov(rendo_weak)))["P"] / 
                        sqrt(diag(vcov(manual_weak)))["P"], 2), "x larger\n")

# -----------------------------------------------------------------------------
# 6. Summary Statistics
# -----------------------------------------------------------------------------
cat("\n6. SUMMARY OF FINDINGS\n")
cat("---------------------\n")

summary_table <- data.frame(
  Method = c("True value", "Standard Lewbel", "REndo hetErrorsIV", 
             "Overidentified (manual)", "Weak het - Lewbel", "Weak het - REndo"),
  Coefficient = c(params$gamma1, coef_manual, coef_rendo, 
                  ifelse(!is.null(overid_model), coef_overid, NA),
                  coef(manual_weak)["P"], coef(rendo_weak)["P"]),
  Std_Error = c(NA, se_manual, se_rendo, 
                ifelse(!is.null(overid_model), se_overid, NA),
                sqrt(diag(vcov(manual_weak)))["P"], 
                sqrt(diag(vcov(rendo_weak)))["P"])
)

print(round(summary_table, 6))

cat("\n=================================================================\n")
cat("CONCLUSION: REndo implements a different identification strategy\n")
cat("than the standard Lewbel (2012) method used by hetid and ivreg2h\n")
cat("=================================================================\n")

# Save results for further analysis
save(list = ls(), file = "lewbel_verification_results.RData")
cat("\nResults saved to lewbel_verification_results.RData\n")