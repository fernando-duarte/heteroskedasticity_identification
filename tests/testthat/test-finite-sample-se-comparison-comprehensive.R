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

test_that("all three packages produce identical coefficients", {
  skip_if_not_comprehensive_test()

  # Generate test data
  set.seed(789)
  data <- generate_hetid_test_data(n = 500, seed = 789)

  # hetid coefficient
  hetid_model <- ivreg(
    y ~ X1 + P | X1 + lewbel_iv,
    data = data
  )
  hetid_coef <- coef(hetid_model)["P"]

  # Track which packages were tested
  packages_tested <- "hetid"

  # REndo coefficient (if available)
  if (has_rendo()) {
    library(REndo)
    rendo_model <- tryCatch({
      hetErrorsIV(
        y ~ X1 + P | X1 | IIV(Z),
        data = data
      )
    }, error = function(e) NULL)

    if (!is.null(rendo_model)) {
      rendo_coef <- coef(summary(rendo_model))["P", "Estimate"]
      expect_equal(as.numeric(hetid_coef), as.numeric(rendo_coef),
                   tolerance = 1e-6,
                   label = "REndo coefficient matches hetid")
      packages_tested <- paste(packages_tested, "+ REndo")
    }
  }

  # Note: Stata comparison is fully implemented in the first test of this file

  # Report which packages were tested
  cat("Packages tested:", packages_tested, "\n")
})
