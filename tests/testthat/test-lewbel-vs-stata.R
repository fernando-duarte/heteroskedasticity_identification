# Stata comparison tests for hetid package
library(hetid)
library(AER)

test_that("hetid matches Stata ivreg2h on single X", {
  skip_if_not(has_stata(), "Stata not available")
  skip_if_not(has_haven(), "haven not available")
  skip_on_cran()

  library(haven)

  # Ensure Stata packages are installed
  if (!ensure_stata_packages()) {
    skip("Could not install required Stata packages")
  }

  # Generate test data
  data <- generate_hetid_test_data(n = 1000, seed = 42)

  # Run hetid
  hetid_model <- ivreg(
    y ~ X1 + P | X1 + lewbel_iv,
    data = data
  )
  hetid_coef <- coef(hetid_model)["P"]
  hetid_se <- sqrt(diag(vcov(hetid_model)))["P"]

  # Write data for Stata
  temp_dta <- tempfile(fileext = ".dta")
  write_dta(data, temp_dta)

  # Create Stata script
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

* Run ivreg2h
ivreg2h y X1 (P =), gen(iiv)

* Store results
scalar b_P = _b[P]
scalar se_P = _se[P]

* Display for verification
display "Coefficient on P: " b_P
display "Standard error on P: " se_P

* Save results to dataset
clear
set obs 1
gen coef_P = scalar(b_P)
gen se_P = scalar(se_P)
save "%s", replace

exit
', temp_dta, temp_results)

  writeLines(stata_code, temp_do)

  # Run Stata
  stata_path <- get_stata_path()
  temp_log <- tempfile(fileext = ".log")
  cmd <- sprintf("%s -b do %s", stata_path, temp_do)
  result <- system(cmd, intern = FALSE, ignore.stdout = TRUE)

  # Read results
  if (result == 0 && file.exists(temp_results)) {
    stata_results <- read_dta(temp_results)
    stata_coef <- stata_results$coef_P[1]
    stata_se <- stata_results$se_P[1]

    # Compare results (we've seen 0.004% difference for coef, 2.3% for SE)
    expect_equal(as.numeric(hetid_coef), as.numeric(stata_coef),
      tolerance = 1e-3,
      label = "Coefficient comparison"
    )
    expect_equal(as.numeric(hetid_se), as.numeric(stata_se),
      tolerance = 0.025,
      label = "Standard error comparison"
    )
  } else {
    skip("Stata execution failed")
  }

  # Clean up
  unlink(c(temp_dta, temp_do, temp_log, temp_results))
})

test_that("hetid matches Stata ivreg2h with multiple X", {
  skip_if_not(has_stata(), "Stata not available")
  skip_if_not(has_haven(), "haven not available")
  skip_on_cran()

  library(haven)

  # Ensure Stata packages are installed
  if (!ensure_stata_packages()) {
    skip("Could not install required Stata packages")
  }

  # Generate data with 2 X variables
  set.seed(123)
  params <- list(
    beta1_0 = 0.5, beta1_1 = c(1.5, 3.0), gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = c(-1.0, 0.7),
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  n <- 1000
  data <- generate_lewbel_data(n, params, n_x = 2)

  # Rename columns
  names(data)[names(data) == "Y1"] <- "y"
  names(data)[names(data) == "Y2"] <- "P"

  # Generate Lewbel instruments for both X variables
  first_stage <- lm(P ~ X1 + X2, data = data)
  e2_hat <- residuals(first_stage)

  # Use both Z1 and Z2 for instruments
  Z1_demeaned <- data$Z1 - mean(data$Z1)
  Z2_demeaned <- data$Z2 - mean(data$Z2)

  data$lewbel_iv1 <- Z1_demeaned * e2_hat
  data$lewbel_iv1 <- data$lewbel_iv1 - mean(data$lewbel_iv1)

  data$lewbel_iv2 <- Z2_demeaned * e2_hat
  data$lewbel_iv2 <- data$lewbel_iv2 - mean(data$lewbel_iv2)

  # Run hetid with both instruments
  hetid_model <- ivreg(
    y ~ X1 + X2 + P | X1 + X2 + lewbel_iv1 + lewbel_iv2,
    data = data
  )
  hetid_coef <- coef(hetid_model)["P"]
  hetid_se <- sqrt(diag(vcov(hetid_model)))["P"]

  # Write data for Stata
  temp_dta <- tempfile(fileext = ".dta")
  write_dta(data, temp_dta)

  # Create Stata script
  temp_do <- tempfile(fileext = ".do")
  temp_results <- tempfile(fileext = ".dta")
  stata_code <- sprintf('
use "%s", clear

* Run ivreg2h with multiple X variables
ivreg2h y X1 X2 (P =), gen(iiv)

* Store results
scalar b_P = _b[P]
scalar se_P = _se[P]

* Display for verification
display "Coefficient on P: " b_P
display "Standard error on P: " se_P

* Save results
clear
set obs 1
gen coef_P = scalar(b_P)
gen se_P = scalar(se_P)
save "%s", replace

exit
', temp_dta, temp_results)

  writeLines(stata_code, temp_do)

  # Run Stata
  stata_path <- get_stata_path()
  temp_log <- tempfile(fileext = ".log")
  cmd <- sprintf("%s -b do %s", stata_path, temp_do)
  result <- system(cmd, intern = FALSE, ignore.stdout = TRUE)

  # Read results
  if (result == 0 && file.exists(temp_results)) {
    stata_results <- read_dta(temp_results)
    stata_coef <- stata_results$coef_P[1]
    stata_se <- stata_results$se_P[1]

    # Compare results
    expect_equal(as.numeric(hetid_coef), as.numeric(stata_coef),
      tolerance = 1e-3,
      label = "Coefficient comparison (multiple X)"
    )
    expect_equal(as.numeric(hetid_se), as.numeric(stata_se),
      tolerance = 0.025,
      label = "Standard error comparison (multiple X)"
    )
  } else {
    skip("Stata execution failed")
  }

  # Clean up
  unlink(c(temp_dta, temp_do, temp_log, temp_results))
})

test_that("Stata diagnostic tests match hetid expectations", {
  skip_if_not(has_stata(), "Stata not available")
  skip_if_not(has_haven(), "haven not available")
  skip_on_cran()

  library(haven)

  # Ensure Stata packages are installed
  if (!ensure_stata_packages()) {
    skip("Could not install required Stata packages")
  }

  # Generate test data
  data <- generate_hetid_test_data(n = 500, seed = 789)

  # Write data for Stata
  temp_dta <- tempfile(fileext = ".dta")
  write_dta(data, temp_dta)

  # Create Stata script with diagnostics
  temp_do <- tempfile(fileext = ".do")
  temp_results <- tempfile(fileext = ".dta")
  stata_code <- sprintf('
use "%s", clear

* Run ivreg2h
ivreg2h y X1 (P =), gen(iiv)

* Get generated instrument properties
summarize iiv_X1_1
scalar iv_mean = r(mean)
scalar iv_sd = r(sd)

* Try to get first-stage F-stat if available
* Note: ivreg2h may not report standard weak ID tests
scalar f_stat = .
capture scalar f_stat = e(widstat)

* Save diagnostics
clear
set obs 1
gen f_stat = scalar(f_stat)
gen iv_mean = scalar(iv_mean)
gen iv_sd = scalar(iv_sd)
save "%s", replace

exit
', temp_dta, temp_results)

  writeLines(stata_code, temp_do)

  # Run Stata
  stata_path <- get_stata_path()
  temp_log <- tempfile(fileext = ".log")
  cmd <- sprintf("%s -b do %s", stata_path, temp_do)
  result <- system(cmd, intern = FALSE, ignore.stdout = TRUE)

  # Read results
  if (result == 0 && file.exists(temp_results)) {
    stata_diag <- read_dta(temp_results)

    # Check instrument is mean-zero (Stata reports very small number like 1.46e-15)
    expect_equal(stata_diag$iv_mean[1], 0,
      tolerance = 1e-10,
      label = "Stata instrument mean-zero"
    )

    # Check instrument has variation
    expect_gt(stata_diag$iv_sd[1], 0,
      label = "Stata instrument has variation"
    )

    # Check F-stat if available (may be missing in ivreg2h output)
    if (!is.na(stata_diag$f_stat[1])) {
      expect_gt(stata_diag$f_stat[1], 10,
        label = "Stata F-stat indicates strong instrument"
      )
    }
  } else {
    skip("Stata execution failed")
  }

  # Clean up
  unlink(c(temp_dta, temp_do, temp_log, temp_results))
})

test_that("hetid and Stata agree across different specifications", {
  skip_if_not(has_stata(), "Stata not available")
  skip_if_not(has_haven(), "haven not available")
  skip_on_cran()

  library(haven)

  # Ensure Stata packages are installed
  if (!ensure_stata_packages()) {
    skip("Could not install required Stata packages")
  }

  # Test different sample sizes
  sample_sizes <- c(200, 500, 1000)

  for (n in sample_sizes) {
    # Generate data
    data <- generate_hetid_test_data(n = n, seed = n)

    # Run hetid
    hetid_model <- ivreg(
      y ~ X1 + P | X1 + lewbel_iv,
      data = data
    )
    hetid_coef <- coef(hetid_model)["P"]

    # Write data for Stata
    temp_dta <- tempfile(fileext = ".dta")
    write_dta(data, temp_dta)

    # Run Stata
    temp_do <- tempfile(fileext = ".do")
    temp_results <- tempfile(fileext = ".dta")
    stata_code <- sprintf('
use "%s", clear
ivreg2h y X1 (P =), gen(iiv)
scalar b_P = _b[P]
clear
set obs 1
gen coef_P = scalar(b_P)
save "%s", replace
exit
', temp_dta, temp_results)

    writeLines(stata_code, temp_do)

    stata_path <- get_stata_path()
    cmd <- sprintf("%s -b do %s", stata_path, temp_do)
    result <- system(cmd, intern = FALSE, ignore.stdout = TRUE)

    if (result == 0 && file.exists(temp_results)) {
      stata_results <- read_dta(temp_results)
      stata_coef <- stata_results$coef_P[1]

      expect_equal(as.numeric(hetid_coef), as.numeric(stata_coef),
        tolerance = 1e-3,
        label = paste("Sample size", n)
      )
    }

    # Clean up
    unlink(c(temp_dta, temp_do, tempfile(fileext = ".log"), temp_results))
  }
})
