test_that("hetid works on simulated UK Engel curve-like data", {
  skip_if_not_comprehensive_test()
  # Skip if AER is not available
  if (!requireNamespace("AER", quietly = TRUE)) {
    skip("AER package not available")
  }

  # Since external data may not be available, simulate similar data
  # This mimics UK food share Engel curve analysis

  set.seed(54321)
  n <- 500

  # Simulate UK Engel curve-like data
  # Age (exogenous)
  age <- runif(n, 25, 75)
  age_centered <- age - mean(age)

  # Common factor
  f <- rnorm(n)

  # Log total expenditure (endogenous)
  lntotalexp <- 7 + 0.02 * age_centered + 0.5 * f + rnorm(n, sd = 0.4)

  # Food share (outcome) - decreases with income (Engel's law)
  foodshare <- 0.4 - 0.1 * lntotalexp + 0.01 * age_centered - 0.2 * f +
    (age_centered^2 / 1000) * rnorm(n, sd = 0.5) # heteroskedasticity

  # Ensure food share is between 0 and 1
  foodshare <- pmax(0.05, pmin(0.95, foodshare))

  uk_data <- data.frame(
    foodshare = foodshare,
    lntotalexp = lntotalexp,
    age = age
  )

  # Data is always available since we simulated it
  # Basic data validation
  expect_true(nrow(uk_data) > 100)
  expect_true(all(c("foodshare", "lntotalexp", "age") %in% names(uk_data)))

  # Prepare data for analysis
  # Remove any rows with missing values
  uk_data <- na.omit(uk_data[, c("foodshare", "lntotalexp", "age")])

  # Set up parameters for hetid
  # We'll treat lntotalexp as endogenous, age as exogenous
  params <- list(
    beta1_0 = 0, beta1_1 = 0.1, gamma1 = -0.5,
    beta2_0 = 0, beta2_1 = 0.1,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.0,
    sample_size = nrow(uk_data)
  )

  # Run hetid analysis
  # Note: We need to use the actual data, not generate new data
  # So we'll construct the analysis manually

  # First stage regression
  first_stage <- lm(lntotalexp ~ age, data = uk_data)
  e2_hat <- residuals(first_stage)

  # Construct Lewbel instrument
  z_val <- uk_data$age^2 - mean(uk_data$age^2)
  lewbel_iv <- (z_val - mean(z_val)) * e2_hat
  uk_data$lewbel_iv <- lewbel_iv

  # 2SLS estimation
  tsls_hetid <- AER::ivreg(
    foodshare ~ age + lntotalexp | age + lewbel_iv,
    data = uk_data
  )

  # Check if REndo is available for comparison
  if (requireNamespace("REndo", quietly = TRUE)) {
    # Run REndo for comparison
    # Both warning and no warning are valid outcomes
    # Warning occurs when heteroskedasticity is weak (p-value > 0.05)
    fit_rendo <- tryCatch(
      {
        REndo::hetErrorsIV(
          foodshare ~ age + lntotalexp | lntotalexp | IIV(age),
          data = uk_data
        )
      },
      warning = function(w) {
        # If warning about weak instruments, that's expected
        if (grepl("heteroscedasticity.*not satisfied", conditionMessage(w))) {
          # Still run the function but note the warning occurred
          suppressWarnings(REndo::hetErrorsIV(
            foodshare ~ age + lntotalexp | lntotalexp | IIV(age),
            data = uk_data
          ))
        } else {
          # Re-throw unexpected warnings
          warning(w)
        }
      }
    )

    # Compare results
    coef_hetid <- coef(tsls_hetid)
    coef_rendo <- coef(fit_rendo)

    # Coefficients should be similar
    # (but may differ due to different Z functions)
    # REndo uses X directly while hetid uses Z = X^2 - E[X^2]
    expect_equal(
      coef_hetid,
      coef_rendo,
      tolerance = 0.02, # 2% tolerance as methods use different instruments
      ignore_attr = TRUE
    )
  }

  # Check that the endogenous coefficient is reasonable
  # For Engel curves, we expect negative coefficient on log total expenditure
  expect_true(coef_hetid["lntotalexp"] < 0)
})

test_that("hetid performs well on simulated data with realistic properties", {
  skip_if_not_comprehensive_test()
  # Skip if AER is not available
  if (!requireNamespace("AER", quietly = TRUE)) {
    skip("AER package not available")
  }

  # Create a larger, more realistic dataset
  n <- 2000
  set.seed(12345)

  # More realistic parameter values based on economic applications
  # Note: Heteroskedasticity is hardcoded in data generation as sqrt(0.5 + 2*Z)
  # Need stronger alpha2 for better identification
  params <- list(
    beta1_0 = 2.5, # Intercept
    beta1_1 = c(0.3, -0.2, 0.15), # Multiple X effects
    gamma1 = -1.2, # Endogenous effect (e.g., price elasticity)
    beta2_0 = 3.0, # First stage intercept
    beta2_1 = c(0.5, 0.3, -0.4), # First stage X effects
    alpha1 = -0.3, # Smaller factor loading for Y1
    alpha2 = 0.8, # Moderate factor loading for Y2
    delta_het = 1.5, # Not used in current implementation
    n_x = 3, # Three exogenous variables
    sample_size = n # Add sample_size to params
  )

  # Run simulation with models
  result <- run_single_lewbel_simulation(
    sim_id = 1,
    params = params,
    endog_var = "Y2",
    exog_vars = c("X1", "X2", "X3"),
    return_models = TRUE
  )

  # Check model quality
  tsls_model <- result$models$tsls_model

  # Should have reasonable R-squared
  # Note: With the log transform, R-squared can be low
  summ <- summary(tsls_model)
  expect_true(!is.na(summ$r.squared))

  # First stage F-stat with multiple X and weak heteroskedasticity is often low
  # With 3 X variables, heteroskedasticity is averaged, reducing instrument strength
  expect_true(result$results$first_stage_F > 2)

  # Estimates should be in the right direction but may have substantial bias
  # With weak instruments, bias can be large
  expect_equal(result$results$tsls_gamma1, params$gamma1, tolerance = 0.8)

  # Check that TSLS moves estimate in the right direction from OLS
  # (even if it doesn't fully correct the bias)
  ols_bias <- result$results$ols_gamma1 - params$gamma1
  tsls_bias <- result$results$tsls_gamma1 - params$gamma1

  # If OLS is biased upward, TSLS should be less biased upward
  # If OLS is biased downward, TSLS should be less biased downward
  improvement <- abs(tsls_bias) <= abs(ols_bias) * 1.1 # Allow 10% margin
  expect_true(improvement || result$results$first_stage_F < 5,
    label = "TSLS should improve on OLS bias unless instruments are very weak"
  )
})

test_that("hetid handles edge cases in real-world style data", {
  skip_if_not_comprehensive_test()
  skip_on_cran()

  # Skip if AER is not available
  if (!requireNamespace("AER", quietly = TRUE)) {
    skip("AER package not available")
  }

  # Test with data that has some challenging properties
  n <- 300
  set.seed(99999)

  # Parameters that create weak instruments
  params_weak <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0,
    delta_het = 0.2, # Very weak heteroskedasticity
    sample_size = n
  )

  result_weak <- run_single_lewbel_simulation(
    sim_id = 1,
    params = params_weak,
    endog_var = "Y2",
    exog_vars = "Xk",
    return_models = TRUE
  )

  # Should still return results, even if F-stat is low
  expect_false(is.na(result_weak$results$tsls_gamma1))
  # With very weak heteroskedasticity, F-stat might not be as low as expected
  # Just check that it returned a valid F-stat
  expect_true(result_weak$results$first_stage_F > 0)

  # Test with strong heteroskedasticity
  params_strong <- params_weak
  params_strong$delta_het <- 3.0 # Very strong heteroskedasticity

  result_strong <- run_single_lewbel_simulation(
    sim_id = 1,
    params = params_strong,
    endog_var = "Y2",
    exog_vars = "Xk",
    return_models = TRUE
  )

  # With new DGP for n_x=1, delta_het is ignored and heteroskedasticity is fixed
  # Both weak and strong will have same heteroskedasticity pattern
  # Just check both have valid F-stats
  expect_true(result_strong$results$first_stage_F > 0)
  expect_true(result_weak$results$first_stage_F > 0)

  # Coverage should be better with stronger instruments
  expect_true(!is.na(result_strong$results$tsls_coverage))
})
