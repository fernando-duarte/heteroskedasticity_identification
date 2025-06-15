test_that("hetid matches REndo::hetErrorsIV() on identical data", {
  skip_if_not(has_rendo())
  
  # Generate data using hetid's method
  params <- list(
    beta1_0 = 0.5, beta1_1 = c(1.5, 3.0), gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = c(-1.0, 0.7),
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2,
    n_x = 2
  )
  
  set.seed(123)
  n <- 500  # Larger sample for skip_on_cran tests
  data <- generate_lewbel_data(n, params, n_x = 2)
  
  # Rename columns to match standard formula interface
  colnames(data)[colnames(data) == "Y1"] <- "y"
  colnames(data)[colnames(data) == "Y2"] <- "P"
  
  # Run hetid simulation
  params_sim <- c(params, list(sample_size = n))
  result_hetid <- run_single_lewbel_simulation(
    sim_id = 1,
    params = params_sim,
    endog_var = "P",
    exog_vars = c("X1", "X2"),
    return_models = TRUE
  )
  
  # Extract hetid coefficients
  coef_hetid <- coef(result_hetid$models$tsls_model)
  
  # Run REndo - note REndo can only handle one endogenous variable
  # REndo expects formula: y ~ X1 + X2 + P | P | IIV(X2)
  # where IIV(X2) tells it to use X2 for heteroskedasticity-based instruments
  fit_rendo <- REndo::hetErrorsIV(
    y ~ X1 + X2 + P | P | IIV(X2),
    data = data
  )
  
  # Extract REndo coefficients
  coef_rendo <- coef(fit_rendo)
  
  # Compare coefficients
  expect_equal(
    coef_hetid,
    coef_rendo,
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
  
  # Compare variance-covariance matrices
  expect_equal(
    vcov(result_hetid$models$tsls_model),
    vcov(fit_rendo),
    tolerance = 1e-6,
    ignore_attr = TRUE
  )
  
  # Extract and compare instruments if possible
  # REndo stores internal instruments that we can access
  if ("internalInstruments" %in% names(fit_rendo)) {
    rendo_iv <- fit_rendo$internalInstruments[, 1]
    
    # Get hetid's instrument from model matrix
    hetid_iv <- model.matrix(result_hetid$models$tsls_model)[, "lewbel_iv"]
    
    # Both should be mean-zero
    expect_equal(mean(rendo_iv), 0, tolerance = 1e-10)
    expect_equal(mean(hetid_iv), 0, tolerance = 1e-10)
    
    # Check correlation (should be very high if using same method)
    cor_iv <- cor(rendo_iv, hetid_iv)
    expect_true(abs(cor_iv) > 0.99)
  }
})

test_that("hetid and REndo agree on diagnostic tests", {
  skip_if_not(has_rendo())
  
  # Use larger sample for better diagnostic test stability
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )
  
  set.seed(456)
  n <- 1000
  data <- generate_lewbel_data(n, params)
  
  # Rename for consistency
  colnames(data)[colnames(data) == "Y1"] <- "y"
  colnames(data)[colnames(data) == "Y2"] <- "P"
  colnames(data)[colnames(data) == "Xk"] <- "X1"
  colnames(data)[colnames(data) == "Z"] <- "Z1"
  
  # Run both methods
  params_sim <- c(params, list(sample_size = n))
  result_hetid <- run_single_lewbel_simulation(
    sim_id = 1,
    params = params_sim,
    endog_var = "P",
    exog_vars = "X1",
    return_models = TRUE
  )
  
  fit_rendo <- REndo::hetErrorsIV(
    y ~ X1 + P | P | IIV(X1),
    data = data
  )
  
  # Compare first-stage F-statistics
  f_stat_hetid <- result_hetid$results$first_stage_F
  
  # REndo reports diagnostics in summary
  summ_rendo <- summary(fit_rendo)
  if ("diagnostics" %in% names(summ_rendo)) {
    diag_rendo <- summ_rendo$diagnostics
    
    # Look for F-statistic or similar weak instrument test
    if (is.matrix(diag_rendo) && "statistic" %in% colnames(diag_rendo)) {
      # Find weak instrument test row (may vary by REndo version)
      weak_rows <- grep("weak|Weak", rownames(diag_rendo), ignore.case = TRUE)
      if (length(weak_rows) > 0) {
        f_stat_rendo <- diag_rendo[weak_rows[1], "statistic"]
        
        # F-statistics should be similar but may not be identical due to
        # implementation differences
        expect_equal(f_stat_hetid, f_stat_rendo, tolerance = 0.1)
      }
    }
  }
})

test_that("hetid and REndo produce similar results across parameter space", {
  skip_if_not(has_rendo())
  
  # Test multiple parameter configurations
  test_configs <- list(
    list(delta_het = 0.5, desc = "weak heteroskedasticity"),
    list(delta_het = 2.0, desc = "strong heteroskedasticity"),
    list(gamma1 = -2.0, desc = "large endogenous effect"),
    list(gamma1 = -0.2, desc = "small endogenous effect")
  )
  
  base_params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )
  
  for (config in test_configs) {
    # Update parameters
    test_params <- modifyList(base_params, config[names(config) != "desc"])
    
    set.seed(789)
    n <- 500
    data <- generate_lewbel_data(n, test_params)
    
    # Rename columns
    colnames(data)[colnames(data) == "Y1"] <- "y"
    colnames(data)[colnames(data) == "Y2"] <- "P"
    colnames(data)[colnames(data) == "Xk"] <- "X1"
    
    # Run both methods
    params_sim <- c(test_params, list(sample_size = n))
    result_hetid <- run_single_lewbel_simulation(
      sim_id = 1,
      params = params_sim,
      endog_var = "P",
      exog_vars = "X1",
      return_models = TRUE
    )
    
    fit_rendo <- REndo::hetErrorsIV(
      y ~ X1 + P | P | IIV(X1),
      data = data
    )
    
    # Compare endogenous variable coefficient
    coef_P_hetid <- coef(result_hetid$models$tsls_model)["P"]
    coef_P_rendo <- coef(fit_rendo)["P"]
    
    expect_equal(
      coef_P_hetid,
      coef_P_rendo,
      tolerance = 1e-6,
      ignore_attr = TRUE,
      label = config$desc
    )
  }
})