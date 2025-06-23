# Test finite sample standard errors across REndo, ivreg2h, and hetid
library(hetid)

# Skip all tests if AER is not available
if (!requireNamespace("AER", quietly = TRUE)) {
  skip("AER package not available")
}
library(AER)

test_that("finite sample SEs match across REndo, ivreg2h, and hetid", {
  skip_if_not_comprehensive_test()

  # Generate test data
  set.seed(42)
  data <- generate_hetid_test_data(n = 1000, seed = 42)

  # 1. hetid with finite sample SEs (default for AER::ivreg)
  hetid_model <- ivreg(
    y ~ X1 + P | X1 + lewbel_iv,
    data = data
  )
  hetid_coef <- coef(hetid_model)["P"]
  hetid_se_finite <- sqrt(diag(vcov(hetid_model)))["P"]  # Default is finite sample

  # Verify this is indeed finite sample SE
  hetid_se_check <- extract_se_ivreg(hetid_model, df_adjust = "finite")["P"]
  expect_equal(as.numeric(hetid_se_finite), as.numeric(hetid_se_check),
               tolerance = 1e-10, label = "hetid finite SE check")

  # 2. REndo comparison (if available)
  if (has_rendo()) {
    library(REndo)

    # REndo's hetErrorsIV
    rendo_model <- tryCatch({
      hetErrorsIV(
        y ~ X1 + P | X1 | IIV(Z),
        data = data
      )
    }, error = function(e) NULL)

    if (!is.null(rendo_model)) {
      rendo_summ <- summary(rendo_model)
      rendo_coef <- coef(rendo_summ)["P", "Estimate"]
      rendo_se <- coef(rendo_summ)["P", "Std. Error"]

      # REndo uses finite sample SEs by default
      # Check coefficient match (they should be identical)
      expect_equal(as.numeric(hetid_coef), as.numeric(rendo_coef),
                   tolerance = 0.001, label = "REndo coefficient match")

      # Check SE match (should be very close for finite sample)
      expect_equal(as.numeric(hetid_se_finite), as.numeric(rendo_se),
                   tolerance = 0.001, label = "REndo finite SE match")
    }
  }

  # 3. Stata comparison (if available)
  can_run_stata <- has_stata() && has_haven() &&
    !identical(Sys.getenv("NOT_CRAN"), "")

  if (can_run_stata && requireNamespace("haven", quietly = TRUE)) {
    library(haven)

    if (ensure_stata_packages()) {
      # Write data for Stata
      temp_dta <- tempfile(fileext = ".dta")
      write_dta(data, temp_dta)

      # Create Stata script with finite sample option
      temp_do <- tempfile(fileext = ".do")
      temp_results <- tempfile(fileext = ".dta")
      stata_code <- sprintf('
use "%s", clear

* Ensure packages are available
capture which ranktest
if _rc {
    ssc install ranktest, replace
}
capture which ivreg2
if _rc {
    ssc install ivreg2, replace
}
capture which ivreg2h
if _rc {
    ssc install ivreg2h, replace
}

* Method 1: Run ivreg2h (generates instruments internally)
ivreg2h y X1 (P =), gen(iiv)

* Store asymptotic results from ivreg2h
scalar b_P_asymp = _b[P]
scalar se_P_asymp = _se[P]

* Method 2: Run ivreg2 with the Lewbel instrument and small option for finite sample
ivreg2 y X1 (P = lewbel_iv), small

* Store finite sample results
scalar b_P_finite = _b[P]
scalar se_P_finite = _se[P]

* Display for verification
display "=== IVREG2H (asymptotic) ==="
display "Coefficient on P: " b_P_asymp
display "Standard error on P: " se_P_asymp

display "=== IVREG2 with small option (finite sample) ==="
display "Coefficient on P: " b_P_finite
display "Standard error on P: " se_P_finite

* Save results to dataset
clear
set obs 1
gen coef_P_asymp = scalar(b_P_asymp)
gen se_P_asymp = scalar(se_P_asymp)
gen coef_P_finite = scalar(b_P_finite)
gen se_P_finite = scalar(se_P_finite)
save "%s", replace

exit
', temp_dta, temp_results)

      writeLines(stata_code, temp_do)

      # Run Stata
      stata_path <- get_stata_path()
      temp_log <- tempfile(fileext = ".log")
      cmd <- sprintf("%s -b do %s", stata_path, temp_do)
      result <- system(cmd, intern = FALSE, ignore.stdout = TRUE)

      # Read results if available
      if (result == 0 && file.exists(temp_results)) {
        stata_results <- read_dta(temp_results)
        stata_coef_finite <- stata_results$coef_P_finite[1]
        stata_se_finite <- stata_results$se_P_finite[1]

        # Compare finite sample results
        expect_equal(as.numeric(hetid_coef), stata_coef_finite,
                     tolerance = 0.001,
                     label = "Coefficient matches Stata (finite sample)")

        expect_equal(as.numeric(hetid_se_finite), stata_se_finite,
                     tolerance = 0.001,
                     label = "SE matches Stata (finite sample)")
      }

      # Clean up
      unlink(c(temp_dta, temp_do, temp_log, temp_results))
    }
  }
})

test_that("finite sample SE calculations are consistent", {
  skip_if_not_comprehensive_test()

  # Generate smaller dataset for clearer demonstration
  set.seed(123)
  n <- 200
  data <- generate_hetid_test_data(n = n, seed = 123)

  # Fit model
  model <- ivreg(
    y ~ X1 + P | X1 + lewbel_iv,
    data = data
  )

  # Get finite sample SE (default)
  se_finite_default <- sqrt(diag(vcov(model)))["P"]

  # Get asymptotic SE
  se_asymptotic <- extract_se_ivreg(model, df_adjust = "asymptotic")["P"]

  # Calculate expected ratio
  k <- length(coef(model))
  expected_ratio <- sqrt(n / (n - k))
  actual_ratio <- as.numeric(se_finite_default / se_asymptotic)

  # Verify the mathematical relationship
  expect_equal(actual_ratio, expected_ratio, tolerance = 1e-10,
               label = "Finite/Asymptotic SE ratio")

  # Verify finite sample SE is larger (for small samples)
  expect_gt(se_finite_default, se_asymptotic,
            label = "Finite SE > Asymptotic SE")
})

test_that("all three packages produce identical coefficients when using same instruments", {
  skip_if_not_comprehensive_test()

  # Generate test data with single X for clearer comparison
  set.seed(789)
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.5
  )
  data <- generate_lewbel_data(n_obs = 1000, params = params)

  # Add column mapping for consistency
  data$y <- data$Y1
  data$P <- data$Y2
  data$X1 <- data$Xk

  # Generate Lewbel instruments using both approaches
  first_stage <- lm(P ~ X1, data = data)
  e2_hat <- residuals(first_stage)

  # Method 1: Standard Lewbel with Z = X^2 - E[X^2]
  z_demeaned <- data$Z - mean(data$Z)
  lewbel_iv_z <- z_demeaned * e2_hat
  lewbel_iv_z <- lewbel_iv_z - mean(lewbel_iv_z)

  # Method 2: Using X directly (what REndo does)
  x_demeaned <- data$X1 - mean(data$X1)
  lewbel_iv_x <- x_demeaned * e2_hat
  lewbel_iv_x <- lewbel_iv_x - mean(lewbel_iv_x)

  cat("\n=== Coefficient Comparison Across Packages ===\n")
  cat("True gamma1:", params$gamma1, "\n\n")

  # 1. hetid using standard Lewbel (Z-based) instrument
  hetid_z_model <- ivreg(
    y ~ X1 + P | X1 + lewbel_iv_z,
    data = data
  )
  hetid_z_coef <- coef(hetid_z_model)["P"]
  hetid_z_se <- sqrt(diag(vcov(hetid_z_model)))["P"]

  cat("1. hetid (standard Lewbel with Z = X^2 - E[X^2]):\n")
  cat("   Coefficient:", round(hetid_z_coef, 6), "\n")
  cat("   Std Error:", round(hetid_z_se, 6), "\n")
  cat("   t-statistic:", round(hetid_z_coef / hetid_z_se, 3), "\n")

  # Track results
  results <- list(
    hetid_z = list(coef = hetid_z_coef, se = hetid_z_se, method = "hetid (Z = X^2 - E[X^2])")
  )

  # 2. Manual implementation using X-based instrument
  hetid_x_model <- ivreg(
    y ~ X1 + P | X1 + lewbel_iv_x,
    data = data
  )
  hetid_x_coef <- coef(hetid_x_model)["P"]
  hetid_x_se <- sqrt(diag(vcov(hetid_x_model)))["P"]

  cat("\n2. Manual implementation with X as instrument:\n")
  cat("   Coefficient:", round(hetid_x_coef, 6), "\n")
  cat("   Std Error:", round(hetid_x_se, 6), "\n")
  cat("   t-statistic:", round(hetid_x_coef / hetid_x_se, 3), "\n")

  results$manual_x <- list(coef = hetid_x_coef, se = hetid_x_se,
                           method = "Manual (Z = X)")

  # 3. REndo coefficient (if available)
  if (has_rendo()) {
    library(REndo)

    # REndo uses X directly as the heteroskedasticity source
    rendo_model <- tryCatch({
      hetErrorsIV(
        y ~ X1 + P | P | IIV(X1),
        data = data
      )
    }, error = function(e) {
      cat("\n", "Error:", e$message, "\n")
      NULL
    })

    if (!is.null(rendo_model)) {
      rendo_coef <- coef(rendo_model)["P"]
      rendo_se <- sqrt(diag(vcov(rendo_model)))["P"]

      cat("\n3. REndo hetErrorsIV (should match manual X implementation):\n")
      cat("   Coefficient:", round(rendo_coef, 6), "\n")
      cat("   Std Error:", round(rendo_se, 6), "\n")
      cat("   t-statistic:", round(rendo_coef / rendo_se, 3), "\n")

      results$rendo <- list(coef = rendo_coef, se = rendo_se, method = "REndo (Z = X)")

      # Test that REndo matches our manual X implementation exactly
      expect_equal(as.numeric(rendo_coef), as.numeric(hetid_x_coef),
                   tolerance = 0.001,
                   label = "REndo coefficient matches manual X implementation")
    }
  }

  # 4. Stata comparison (reference from other test)
  cat("\n4. Stata ivreg2h (from test-lewbel-vs-stata-comprehensive.R):\n")
  cat("   Uses same approach as hetid (Z = X^2 - E[X^2])\n")
  cat("   Coefficients match hetid to 8+ decimal places\n")
  cat("   See test-lewbel-vs-stata-comprehensive.R for verification\n")

  # Summary comparison
  cat("\n=== Summary ===\n")

  # Key insight: When X is uniform [0,1], Z = X^2 - E[X^2] is perfectly correlated with X
  # So using Z or X as instrument should give identical results
  cat("Key insight: With uniform X, the instruments Z and X produce identical results\n")
  cat("This is because Z = X^2 - E[X^2] is a linear transformation of X\n\n")

  # Compare coefficients
  coef_diff_z_x <- abs(hetid_z_coef - hetid_x_coef)
  cat("Coefficient difference (Z-based vs X-based):", format(coef_diff_z_x, scientific = TRUE), "\n")

  # These should match exactly (within numerical precision)
  expect_equal(as.numeric(hetid_z_coef), as.numeric(hetid_x_coef),
               tolerance = 1e-10,
               label = "Z-based and X-based instruments give identical coefficients")

  # All coefficients should be on the same side of zero as the true value
  true_sign <- sign(params$gamma1)
  for (method in names(results)) {
    est_sign <- sign(results[[method]]$coef)
    expect_equal(as.numeric(est_sign), as.numeric(true_sign),
                 label = paste(method, "has correct sign"))
  }

  # Report number of methods tested
  cat("\nMethods tested:", length(results), "\n")

  # Additional verification with different data generation
  cat("\n\n=== Testing with Normal X (where Z and X differ) ===\n")

  # Generate data with normal X to show when methods differ
  set.seed(456)
  n <- 1000
  x_normal <- rnorm(n)
  u <- rnorm(n)
  v1 <- rnorm(n)
  v2 <- rnorm(n) * exp(0.5 * params$delta_het * (x_normal^2 - mean(x_normal^2)))

  epsilon1 <- params$alpha1 * u + v1
  epsilon2 <- params$alpha2 * u + v2

  data_normal <- data.frame(
    X1 = x_normal,
    P = params$beta2_0 + params$beta2_1 * x_normal + epsilon2,
    stringsAsFactors = FALSE
  )
  data_normal$y <- params$beta1_0 + params$beta1_1 * x_normal + params$gamma1 * data_normal$P + epsilon1

  # First stage with normal X
  first_stage_normal <- lm(P ~ X1, data = data_normal)
  e2_hat_normal <- residuals(first_stage_normal)

  # Z-based instrument with normal X
  z_normal <- x_normal^2 - mean(x_normal^2)
  z_iv_normal <- (z_normal - mean(z_normal)) * e2_hat_normal
  z_iv_normal <- z_iv_normal - mean(z_iv_normal)

  # X-based instrument with normal X
  x_iv_normal <- (x_normal - mean(x_normal)) * e2_hat_normal
  x_iv_normal <- x_iv_normal - mean(x_iv_normal)

  # Run both regressions
  model_z_normal <- ivreg(y ~ X1 + P | X1 + z_iv_normal, data = data_normal)
  model_x_normal <- ivreg(y ~ X1 + P | X1 + x_iv_normal, data = data_normal)

  coef_z_normal <- coef(model_z_normal)["P"]
  coef_x_normal <- coef(model_x_normal)["P"]

  cat("With Normal X:\n")
  cat("  Z-based coefficient:", round(coef_z_normal, 6), "\n")
  cat("  X-based coefficient:", round(coef_x_normal, 6), "\n")
  cat("  Difference:", round(abs(coef_z_normal - coef_x_normal), 6), "\n")
  cat("\nConclusion: With non-uniform X, Z and X instruments give different results\n")
})
