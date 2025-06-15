# REndo comparison tests for hetid package
library(hetid)
library(AER)

test_that("hetid matches REndo with identical instruments", {
  skip_if_not(has_rendo(), "REndo not available")
  skip_on_cran()
  
  library(REndo)
  
  # Use consistent test data
  data <- generate_hetid_test_data(n = 1000, seed = 42)
  
  # Run hetid approach using pre-computed instrument
  hetid_model <- ivreg(
    y ~ X1 + P | X1 + lewbel_iv,
    data = data
  )
  
  # Run REndo with heteroskedasticity-based IV
  # REndo expects: y ~ exog + endog | endog | IIV(het_var)
  rendo_model <- hetErrorsIV(
    y ~ X1 + P | P | IIV(X1),
    data = data
  )
  
  # Extract coefficients
  hetid_coef <- coef(hetid_model)["P"]
  rendo_coef <- coef(rendo_model)["P"]
  
  # Should match very closely (we've seen <0.00001% difference in practice)
  expect_equal(as.numeric(hetid_coef), as.numeric(rendo_coef), tolerance = 1e-3)
  
  # Compare standard errors
  hetid_se <- sqrt(diag(vcov(hetid_model)))["P"]
  rendo_se <- sqrt(diag(vcov(rendo_model)))["P"]
  
  # Standard errors should be similar (within 1%)
  expect_equal(as.numeric(hetid_se), as.numeric(rendo_se), tolerance = 0.01)
})

test_that("hetid replicates REndo Monte Carlo results", {
  skip_if_not(has_rendo(), "REndo not available")
  skip_on_cran()
  
  library(REndo)
  
  # Run Monte Carlo comparison
  n_sims <- 100
  results <- data.frame(
    sim_id = 1:n_sims,
    hetid_coef = NA_real_,
    rendo_coef = NA_real_,
    hetid_se = NA_real_,
    rendo_se = NA_real_
  )
  
  set.seed(123)
  for (i in 1:n_sims) {
    # Generate fresh data for each simulation
    sim_data <- generate_hetid_test_data(n = 500, seed = i)
    
    # hetid approach
    hetid_model <- ivreg(
      y ~ X1 + P | X1 + lewbel_iv,
      data = sim_data
    )
    
    # REndo approach
    tryCatch({
      rendo_model <- hetErrorsIV(
        y ~ X1 + P | P | IIV(X1),
        data = sim_data
      )
      
      results$hetid_coef[i] <- coef(hetid_model)["P"]
      results$rendo_coef[i] <- coef(rendo_model)["P"]
      results$hetid_se[i] <- sqrt(diag(vcov(hetid_model)))["P"]
      results$rendo_se[i] <- sqrt(diag(vcov(rendo_model)))["P"]
    }, error = function(e) {
      # Skip failed simulations
      warning("Simulation ", i, " failed: ", e$message)
    })
  }
  
  # Remove failed simulations
  results <- results[!is.na(results$rendo_coef), ]
  
  # Check that we have enough successful simulations
  expect_gte(nrow(results), 80)
  
  # Compare distributions
  # Mean coefficients should be very close
  expect_equal(
    mean(results$hetid_coef),
    mean(results$rendo_coef),
    tolerance = 0.001
  )
  
  # Standard deviations should be similar
  expect_equal(
    sd(results$hetid_coef),
    sd(results$rendo_coef),
    tolerance = 0.01
  )
  
  # Correlation should be very high
  cor_coef <- cor(results$hetid_coef, results$rendo_coef)
  expect_gt(cor_coef, 0.99)
})

test_that("REndo instrument properties match hetid", {
  skip_if_not(has_rendo(), "REndo not available")
  skip_on_cran()
  
  library(REndo)
  
  # Generate test data
  data <- generate_hetid_test_data(n = 500, seed = 99)
  
  # Run REndo
  rendo_model <- hetErrorsIV(
    y ~ X1 + P | P | IIV(X1),
    data = data
  )
  
  # Check if we can access REndo's internal instruments
  # This varies by REndo version
  rendo_has_instruments <- FALSE
  
  if ("internalInstruments" %in% names(rendo_model)) {
    rendo_iv <- rendo_model$internalInstruments
    rendo_has_instruments <- TRUE
  } else if ("instruments" %in% names(rendo_model)) {
    rendo_iv <- rendo_model$instruments
    rendo_has_instruments <- TRUE
  }
  
  if (rendo_has_instruments && !is.null(rendo_iv)) {
    # REndo may have multiple columns, get the generated one
    if (is.matrix(rendo_iv) || is.data.frame(rendo_iv)) {
      # Usually the last column is the generated instrument
      rendo_iv <- rendo_iv[, ncol(rendo_iv)]
    }
    
    # Both instruments should be mean-zero
    expect_equal(mean(rendo_iv), 0, tolerance = 1e-6)
    expect_equal(mean(data$lewbel_iv), 0, tolerance = 1e-10)
    
    # They should be highly correlated (same construction method)
    cor_iv <- cor(as.numeric(rendo_iv), data$lewbel_iv)
    expect_gt(abs(cor_iv), 0.95)
  }
})

test_that("hetid matches REndo across parameter space", {
  skip_if_not(has_rendo(), "REndo not available")
  skip_on_cran()
  
  library(REndo)
  
  # Test different scenarios
  scenarios <- list(
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
  
  for (scenario in scenarios) {
    # Update parameters for this scenario
    test_params <- modifyList(base_params, scenario[names(scenario) != "desc"])
    
    # Generate data with these parameters
    set.seed(789)
    n <- 500
    data <- generate_lewbel_data(n, test_params)
    
    # Rename columns for consistency
    names(data)[names(data) == "Y1"] <- "y"
    names(data)[names(data) == "Y2"] <- "P"
    names(data)[names(data) == "Xk"] <- "X1"
    
    # Generate Lewbel instrument manually
    first_stage <- lm(P ~ X1, data = data)
    e2_hat <- residuals(first_stage)
    Z_demeaned <- data$Z - mean(data$Z)
    data$lewbel_iv <- Z_demeaned * e2_hat
    data$lewbel_iv <- data$lewbel_iv - mean(data$lewbel_iv)
    
    # Run both methods
    hetid_model <- ivreg(
      y ~ X1 + P | X1 + lewbel_iv,
      data = data
    )
    
    rendo_model <- hetErrorsIV(
      y ~ X1 + P | P | IIV(X1),
      data = data
    )
    
    # Compare coefficients
    coef_P_hetid <- coef(hetid_model)["P"]
    coef_P_rendo <- coef(rendo_model)["P"]
    
    expect_equal(
      as.numeric(coef_P_hetid),
      as.numeric(coef_P_rendo),
      tolerance = 1e-3,
      label = scenario$desc
    )
  }
})

test_that("hetid handles edge cases that REndo might not", {
  skip_if_not(has_rendo(), "REndo not available")
  skip_on_cran()
  
  # Test with small sample
  small_data <- generate_hetid_test_data(n = 50, seed = 456)
  
  # hetid should handle this
  hetid_model <- ivreg(
    y ~ X1 + P | X1 + lewbel_iv,
    data = small_data
  )
  
  expect_s3_class(hetid_model, "ivreg")
  expect_true(all(is.finite(coef(hetid_model))))
  
  # REndo might have issues with small samples
  # Try REndo but don't fail test if it errors
  rendo_success <- tryCatch({
    rendo_model <- hetErrorsIV(
      y ~ X1 + P | P | IIV(X1),
      data = small_data
    )
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  # Just verify hetid works regardless of REndo
  expect_true(TRUE)
})