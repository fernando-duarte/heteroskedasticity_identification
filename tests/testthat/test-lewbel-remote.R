test_that("hetid works on real Lewbel data from Boston College", {
  skip_on_cran()
  skip_if_not(has_curl())
  skip_if_offline()
  skip_if_not(has_rendo())
  
  # URL for Lewbel's UK Engel curve data
  url <- "http://fmwww.bc.edu/ec-p/data/stockwatson/uk_engel.csv"
  
  # Try to download data
  tmp_file <- tempfile(fileext = ".csv")
  download_success <- tryCatch({
    download.file(url, tmp_file, quiet = TRUE, method = "libcurl")
    TRUE
  }, error = function(e) FALSE)
  
  if (download_success && file.exists(tmp_file)) {
    uk_data <- tryCatch({
      read.csv(tmp_file)
    }, error = function(e) NULL)
    
    if (!is.null(uk_data)) {
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
      Z <- uk_data$age^2 - mean(uk_data$age^2)
      lewbel_iv <- (Z - mean(Z)) * e2_hat
      uk_data$lewbel_iv <- lewbel_iv
      
      # 2SLS estimation
      tsls_hetid <- AER::ivreg(
        foodshare ~ age + lntotalexp | age + lewbel_iv,
        data = uk_data
      )
      
      # Run REndo for comparison
      fit_rendo <- REndo::hetErrorsIV(
        foodshare ~ age + lntotalexp | lntotalexp | IIV(age),
        data = uk_data
      )
      
      # Compare results
      coef_hetid <- coef(tsls_hetid)
      coef_rendo <- coef(fit_rendo)
      
      # Coefficients should be very similar
      expect_equal(
        coef_hetid,
        coef_rendo,
        tolerance = 1e-6,
        ignore_attr = TRUE
      )
      
      # Check that the endogenous coefficient is reasonable
      # For Engel curves, we expect negative coefficient on log total expenditure
      expect_true(coef_hetid["lntotalexp"] < 0)
    }
  }
  
  # Clean up
  unlink(tmp_file)
})

test_that("hetid performs well on simulated data with realistic properties", {
  skip_on_cran()
  skip_if_not(has_rendo())
  
  # Create a larger, more realistic dataset
  n <- 2000
  set.seed(12345)
  
  # More realistic parameter values based on economic applications
  params <- list(
    beta1_0 = 2.5,      # Intercept
    beta1_1 = c(0.3, -0.2, 0.15),  # Multiple X effects
    gamma1 = -1.2,      # Endogenous effect (e.g., price elasticity)
    beta2_0 = 3.0,      # First stage intercept
    beta2_1 = c(0.5, 0.3, -0.4),   # First stage X effects
    alpha1 = -0.8,      # Factor loading
    alpha2 = 1.5,       # Factor loading
    delta_het = 1.5,    # Moderate heteroskedasticity
    n_x = 3             # Three exogenous variables
  )
  
  # Generate data
  data <- generate_lewbel_data(n, params, n_x = 3)
  
  # Add some realistic features
  # Log transform Y1 to make it more like real economic data
  data$Y1 <- exp(data$Y1 / 4)  # Scale and exponentiate
  
  # Run simulation with models
  params_sim <- c(params, list(sample_size = n))
  result <- run_single_lewbel_simulation(
    sim_id = 1,
    params = params_sim,
    endog_var = "Y2",
    exog_vars = c("X1", "X2", "X3"),
    return_models = TRUE
  )
  
  # Check model quality
  tsls_model <- result$models$tsls_model
  
  # Should have reasonable R-squared
  summ <- summary(tsls_model)
  expect_true(summ$r.squared > 0.1 && summ$r.squared < 0.9)
  
  # First stage should be strong
  expect_true(result$results$first_stage_F > 10)
  
  # Estimates should be reasonably close to truth
  expect_equal(result$results$tsls_gamma1, params$gamma1, tolerance = 0.2)
  
  # OLS should be biased (demonstrating endogeneity)
  bias_ols <- abs(result$results$ols_gamma1 - params$gamma1)
  bias_tsls <- abs(result$results$tsls_gamma1 - params$gamma1)
  expect_true(bias_tsls < bias_ols)
})

test_that("hetid handles edge cases in real-world style data", {
  skip_on_cran()
  
  # Test with data that has some challenging properties
  n <- 300
  set.seed(99999)
  
  # Parameters that create weak instruments
  params_weak <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, 
    delta_het = 0.2,  # Very weak heteroskedasticity
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
  expect_true(result_weak$results$first_stage_F < 10)  # Weak instrument
  
  # Test with strong heteroskedasticity
  params_strong <- params_weak
  params_strong$delta_het <- 3.0  # Very strong heteroskedasticity
  
  result_strong <- run_single_lewbel_simulation(
    sim_id = 1,
    params = params_strong,
    endog_var = "Y2",
    exog_vars = "Xk",
    return_models = TRUE
  )
  
  # Should have stronger first stage
  expect_true(result_strong$results$first_stage_F > 
              result_weak$results$first_stage_F)
  
  # Coverage should be better with stronger instruments
  expect_true(!is.na(result_strong$results$tsls_coverage))
})