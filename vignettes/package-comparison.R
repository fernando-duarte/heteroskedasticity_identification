## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = TRUE
)

## ----setup--------------------------------------------------------------------
library(hetid)
library(AER)
if (requireNamespace("REndo", quietly = TRUE)) {
  library(REndo)
}

## ----quick-demo---------------------------------------------------------------
# Generate test data
set.seed(12345)
params <- list(
  beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
  beta2_0 = 1.0, beta2_1 = -1.0,
  alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
)
data <- generate_lewbel_data(1000, params)

# Standard Lewbel implementation (as in hetid/Stata)
e2_hat <- residuals(lm(Y2 ~ Xk, data = data))
lewbel_iv <- (data$Z - mean(data$Z)) * e2_hat
lewbel_iv <- lewbel_iv - mean(lewbel_iv)
model <- ivreg(Y1 ~ Xk + Y2 | Xk + lewbel_iv, data = data)

# Display results
cat("Coefficient on Y2:", round(coef(model)["Y2"], 6), "\n")
cat("Standard error:", round(sqrt(diag(vcov(model)))["Y2"], 6), "\n")
cat("True value:", params$gamma1, "\n")

## ----complete-verification----------------------------------------------------
# Comprehensive function to verify all three implementations
verify_lewbel_implementations <- function(n = 1000, seed = 12345, verbose = TRUE) {
  set.seed(seed)

  # Generate data
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.5
  )

  if (verbose) {
    cat("=================================================================\n")
    cat("Verification of Lewbel (2012) Implementation Differences\n")
    cat("=================================================================\n\n")
    cat("1. GENERATING LEWBEL-TYPE DATA\n")
    cat("------------------------------\n")
    cat("Generated data with n =", n, "observations\n")
    cat("True gamma1 (coefficient on P):", params$gamma1, "\n\n")
  }

  # Generate multiple datasets for comparison
  data_strong <- generate_lewbel_data(n, params)
  params_weak <- params
  params_weak$delta_het <- 0.1
  data_weak <- generate_lewbel_data(n, params_weak)

  # Prepare data frames
  test_data <- data.frame(
    y = data_strong$Y1,
    x = data_strong$Xk,
    p = data_strong$Y2,
    z = data_strong$Z
  )

  test_data_weak <- data.frame(
    y = data_weak$Y1,
    x = data_weak$Xk,
    p = data_weak$Y2,
    z = data_weak$Z
  )

  if (verbose) {
    cat("2. INSTRUMENT CONSTRUCTION COMPARISON\n")
    cat("------------------------------------\n")
  }

  # Standard Lewbel (hetid/Stata) - uses Z = X² - E[X²]
  e2_hat <- residuals(lm(p ~ x, data = test_data))
  iv_z <- (test_data$z - mean(test_data$z)) * e2_hat
  iv_z <- iv_z - mean(iv_z)
  model_z <- ivreg(y ~ x + p | x + iv_z, data = test_data)

  if (verbose) {
    cat("\nMethod 1 - Standard Lewbel (hetid/ivreg2h):\n")
    cat("  Uses Z = X² - E[X²] to maximize heteroskedasticity signal\n")
    cat("  Instrument std dev =", round(sd(iv_z), 4), "\n")
  }

  # Alternative with X (what we suspect REndo does)
  iv_x <- (test_data$x - mean(test_data$x)) * e2_hat
  iv_x <- iv_x - mean(iv_x)
  model_x <- ivreg(y ~ x + p | x + iv_x, data = test_data)

  if (verbose) {
    cat("\nMethod 2 - Using X directly (hypothesized REndo approach):\n")
    cat("  Uses Z = X for simplicity\n")
    cat("  Instrument std dev =", round(sd(iv_x), 4), "\n")
    cat("\nCorrelation between instruments:", round(cor(iv_z, iv_x), 4), "\n")
  }

  # Results collection
  results_list <- list()

  if (verbose) {
    cat("\n3. COMPARING ESTIMATION RESULTS\n")
    cat("-------------------------------\n")
  }

  # Store standard results
  results_list$standard <- list(
    coef = coef(model_z)["p"],
    se = sqrt(diag(vcov(model_z)))["p"],
    method = "hetid/Stata (Z=X²-E[X²])"
  )

  results_list$alternative <- list(
    coef = coef(model_x)["p"],
    se = sqrt(diag(vcov(model_x)))["p"],
    method = "Manual with Z=X"
  )

  if (verbose) {
    cat("\nA. Standard Lewbel (hetid/Stata approach):\n")
    cat("   Coefficient:", round(results_list$standard$coef, 6), "\n")
    cat("   Standard error:", round(results_list$standard$se, 6), "\n")
    cat("   t-statistic:", round(results_list$standard$coef / results_list$standard$se, 3), "\n")
  }

  # REndo (if available)
  if (requireNamespace("REndo", quietly = TRUE)) {
    rendo <- REndo::hetErrorsIV(y ~ x + p | p | IIV(x), data = test_data)

    results_list$rendo <- list(
      coef = coef(rendo)["p"],
      se = sqrt(diag(vcov(rendo)))["p"],
      method = "REndo hetErrorsIV"
    )

    if (verbose) {
      cat("\nB. REndo's hetErrorsIV:\n")
      cat("   Coefficient:", round(results_list$rendo$coef, 6), "\n")
      cat("   Standard error:", round(results_list$rendo$se, 6), "\n")
      cat("   t-statistic:", round(results_list$rendo$coef / results_list$rendo$se, 3), "\n")
      cat("   SE ratio (REndo/Standard):",
          round(results_list$rendo$se / results_list$standard$se, 4), "\n")

      # Check which manual method REndo matches
      diff_to_z <- abs(results_list$rendo$coef - results_list$standard$coef)
      diff_to_x <- abs(results_list$rendo$coef - results_list$alternative$coef)

      if (diff_to_x < diff_to_z) {
        cat("\n   => REndo appears to use X directly (not Z = X² - E[X²])\n")
      }
    }
  }

  # Test with weak heteroskedasticity
  if (verbose) {
    cat("\n4. TESTING WITH WEAK HETEROSKEDASTICITY\n")
    cat("---------------------------------------\n")
  }

  # Weak heteroskedasticity tests
  e2_hat_weak <- residuals(lm(p ~ x, data = test_data_weak))
  iv_z_weak <- (test_data_weak$z - mean(test_data_weak$z)) * e2_hat_weak
  iv_z_weak <- iv_z_weak - mean(iv_z_weak)
  model_z_weak <- ivreg(y ~ x + p | x + iv_z_weak, data = test_data_weak)

  results_list$weak_standard <- list(
    coef = coef(model_z_weak)["p"],
    se = sqrt(diag(vcov(model_z_weak)))["p"],
    method = "Standard Lewbel (weak het)"
  )

  if (requireNamespace("REndo", quietly = TRUE)) {
    rendo_weak <- suppressWarnings(
      REndo::hetErrorsIV(y ~ x + p | p | IIV(x), data = test_data_weak)
    )

    results_list$weak_rendo <- list(
      coef = coef(rendo_weak)["p"],
      se = sqrt(diag(vcov(rendo_weak)))["p"],
      method = "REndo (weak het)"
    )

    if (verbose) {
      cat("\nWith weak heteroskedasticity (delta_het = 0.1):\n")
      cat("  Standard Lewbel SE:", round(results_list$weak_standard$se, 6), "\n")
      cat("  REndo SE:", round(results_list$weak_rendo$se, 6), "\n")
      cat("  Ratio:", round(results_list$weak_rendo$se / results_list$weak_standard$se, 2), "\n")
    }
  }

  # Create summary table
  summary_df <- data.frame(
    Method = character(),
    Coefficient = numeric(),
    Std_Error = numeric(),
    stringsAsFactors = FALSE
  )

  # Add true value
  summary_df <- rbind(summary_df, data.frame(
    Method = "True value",
    Coefficient = params$gamma1,
    Std_Error = NA
  ))

  # Add all results
  for (res in results_list) {
    summary_df <- rbind(summary_df, data.frame(
      Method = res$method,
      Coefficient = res$coef,
      Std_Error = res$se
    ))
  }

  if (verbose) {
    cat("\n5. SUMMARY OF FINDINGS\n")
    cat("---------------------\n")
    print(summary_df, digits = 6)

    cat("\n=================================================================\n")
    cat("CONCLUSION: REndo implements a valid but different approach\n")
    cat("than the standard Lewbel (2012) method used by hetid and ivreg2h\n")
    cat("=================================================================\n")

    # Stata verification code
    cat("\n6. STATA VERIFICATION CODE\n")
    cat("-------------------------\n")
    cat("* After saving data with: write.dta(test_data, 'test_data.dta')\n")
    cat("quietly reg p x\n")
    cat("predict e2_hat, residuals\n")
    cat("quietly sum z\n")
    cat("gen iv = (z - r(mean)) * e2_hat\n")
    cat("quietly sum iv\n")
    cat("replace iv = iv - r(mean)\n")
    cat("ivregress 2sls y x (p = iv)\n")
    cat("\nExpected Stata coefficient:", round(results_list$standard$coef, 8), "\n")
    cat("Expected Stata SE:", round(results_list$standard$se, 8), "\n")
  }

  invisible(summary_df)
}

# Run the comprehensive verification
results <- verify_lewbel_implementations()

## ----df-adjustments-----------------------------------------------------------
# Note: run_single_lewbel_simulation is from the hetid package
# hetid default: asymptotic (matches Stata)
params <- list(
  sample_size = 1000,  # Required parameter
  beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
  beta2_0 = 1.0, beta2_1 = -1.0,
  alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
)

result_asymp <- run_single_lewbel_simulation(
  sim_id = 1, params = params,
  df_adjust = "asymptotic" # Default
)

# For finite sample SEs (matches base R)
result_finite <- run_single_lewbel_simulation(
  sim_id = 1, params = params,
  df_adjust = "finite"
)

cat("Asymptotic SE:", round(result_asymp$tsls_se, 6), "\n")
cat("Finite sample SE:", round(result_finite$tsls_se, 6), "\n")
cat("Ratio (finite/asymptotic):", round(result_finite$tsls_se / result_asymp$tsls_se, 4), "\n")

## ----matching-software--------------------------------------------------------
# To match Stata ivreg2h (asymptotic SEs):
params <- list(
  sample_size = 1000,  # Required parameter
  beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
  beta2_0 = 1.0, beta2_1 = -1.0,
  alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
)

result <- run_single_lewbel_simulation(
  sim_id = 1, params = params, df_adjust = "asymptotic"
)
cat("Result to match Stata (asymptotic):\n")
cat("  Coefficient:", round(result$tsls_gamma1, 6), "\n")
cat("  SE:", round(result$tsls_se, 6), "\n")

# To match R's ivreg default (finite sample SEs):
result <- run_single_lewbel_simulation(
  sim_id = 1, params = params, df_adjust = "finite"
)
cat("\nResult to match R's ivreg (finite sample):\n")
cat("  Coefficient:", round(result$tsls_gamma1, 6), "\n")
cat("  SE:", round(result$tsls_se, 6), "\n")

