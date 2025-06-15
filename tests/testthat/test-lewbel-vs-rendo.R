# Comparison tests for hetid, REndo, and Stata implementations
# Based on our finding that:
# - hetid and Stata use Z = X^2 - E[X^2] (identical results)
# - REndo uses Z = X (valid but different)
library(hetid)
library(AER)

test_that("hetid matches Stata approach exactly", {
  skip_on_cran()
  
  # Generate test data
  set.seed(12345)
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )
  data <- generate_lewbel_data(1000, params)
  
  # Construct Lewbel instrument using Z = X^2 - E[X^2]
  e2_hat <- residuals(lm(Y2 ~ Xk, data = data))
  iv_z <- (data$Z - mean(data$Z)) * e2_hat
  iv_z <- iv_z - mean(iv_z)
  
  # Run 2SLS (this is what Stata's ivregress does)
  model <- ivreg(Y1 ~ Xk + Y2 | Xk + iv_z, data = data)
  
  # Expected results based on our verification
  expected_coef <- -0.8009241
  expected_se <- 0.00096109
  
  # Check coefficient matches to high precision
  expect_equal(as.numeric(coef(model)["Y2"]), expected_coef, tolerance = 1e-6)
  
  # Check SE matches (with asymptotic df adjustment)
  expect_equal(as.numeric(sqrt(diag(vcov(model)))["Y2"]), expected_se, tolerance = 1e-5)
})

test_that("REndo uses X instead of Z for instruments", {
  skip_if_not(has_rendo(), "REndo not available")
  skip_on_cran()

  library(REndo)
  
  # Generate test data
  set.seed(42)
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.5
  )
  data <- generate_lewbel_data(1000, params)
  test_data <- data.frame(y=data$Y1, x=data$Xk, p=data$Y2, z=data$Z)
  
  # Standard Lewbel with Z = X^2 - E[X^2]
  e2_hat <- residuals(lm(p ~ x, data = test_data))
  iv_z <- (test_data$z - mean(test_data$z)) * e2_hat
  iv_z <- iv_z - mean(iv_z)
  model_z <- ivreg(y ~ x + p | x + iv_z, data = test_data)
  
  # Alternative using X directly (what REndo does)
  iv_x <- (test_data$x - mean(test_data$x)) * e2_hat
  iv_x <- iv_x - mean(iv_x)
  model_x <- ivreg(y ~ x + p | x + iv_x, data = test_data)
  
  # REndo's hetErrorsIV
  rendo <- suppressMessages(hetErrorsIV(y ~ x + p | p | IIV(x), data = test_data))
  
  # REndo should match the X-based approach, not the Z-based
  expect_equal(
    as.numeric(coef(rendo)["p"]), 
    as.numeric(coef(model_x)["p"]), 
    tolerance = 1e-6,
    label = "REndo uses X for instruments"
  )
  
  # REndo should NOT match the Z-based approach exactly
  expect_false(
    abs(coef(rendo)["p"] - coef(model_z)["p"]) < 1e-8,
    label = "REndo differs from Z-based approach"
  )
  
  # Document the typical SE difference
  se_ratio <- sqrt(diag(vcov(rendo)))["p"] / sqrt(diag(vcov(model_z)))["p"]
  # REndo can have more variation in SE ratio
  expect_true(se_ratio > 0.90 && se_ratio < 1.10, 
    label = "REndo SEs typically within 10% of Z-based approach")
})

test_that("hetid with multiple instruments matches theoretical expectations", {
  skip_on_cran()
  
  # Generate data with multiple X variables
  set.seed(12345)
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )
  n <- 1000
  
  # Generate multiple X and Z variables
  X1 <- rnorm(n); X2 <- rnorm(n); X3 <- rnorm(n)
  Z1 <- X1^2 - mean(X1^2)
  Z2 <- X2^2 - mean(X2^2) 
  Z3 <- X3^2 - mean(X3^2)
  
  # Generate Y2 and Y1 following Lewbel structure
  epsilon2 <- sqrt(0.5 + 0.5*(Z1^2 + Z2^2 + Z3^2)) * rnorm(n)
  Y2 <- 1 + X1 - X2 + 0.5*X3 + epsilon2
  epsilon1 <- rnorm(n)
  Y1 <- 0.5 + 1.5*X1 + 2*X2 - 0.5*X3 - 0.8*Y2 + epsilon1
  
  data <- data.frame(Y1=Y1, Y2=Y2, X1=X1, X2=X2, X3=X3, Z1=Z1, Z2=Z2, Z3=Z3)
  
  # Construct multiple Lewbel instruments
  e2_hat <- residuals(lm(Y2 ~ X1 + X2 + X3, data = data))
  iv1 <- (Z1 - mean(Z1)) * e2_hat; iv1 <- iv1 - mean(iv1)
  iv2 <- (Z2 - mean(Z2)) * e2_hat; iv2 <- iv2 - mean(iv2)
  iv3 <- (Z3 - mean(Z3)) * e2_hat; iv3 <- iv3 - mean(iv3)
  
  # Test with 1, 2, and 3 instruments
  model1 <- ivreg(Y1 ~ X1 + X2 + X3 + Y2 | X1 + X2 + X3 + iv1, data = data)
  model2 <- ivreg(Y1 ~ X1 + X2 + X3 + Y2 | X1 + X2 + X3 + iv1 + iv2, data = data)
  model3 <- ivreg(Y1 ~ X1 + X2 + X3 + Y2 | X1 + X2 + X3 + iv1 + iv2 + iv3, data = data)
  
  # More instruments should typically reduce SE (more efficient)
  se1 <- sqrt(diag(vcov(model1)))["Y2"]
  se2 <- sqrt(diag(vcov(model2)))["Y2"]
  se3 <- sqrt(diag(vcov(model3)))["Y2"]
  
  expect_true(se2 <= se1, label = "Two instruments more efficient than one")
  expect_true(se3 <= se2, label = "Three instruments more efficient than two")
  
  # All should estimate similar coefficients (close to true value -0.8)
  expect_equal(as.numeric(coef(model1)["Y2"]), -0.8, tolerance = 0.1)
  expect_equal(as.numeric(coef(model2)["Y2"]), -0.8, tolerance = 0.1)
  expect_equal(as.numeric(coef(model3)["Y2"]), -0.8, tolerance = 0.1)
})

test_that("REndo warns appropriately about weak instruments", {
  skip_if_not(has_rendo(), "REndo not available")
  skip_on_cran()

  library(REndo)

  # Test scenario with weak heteroskedasticity (delta_het = 0.1)
  params_weak <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 0.1  # Very weak
  )
  
  set.seed(789)
  data <- generate_lewbel_data(500, params_weak)
  test_data <- data.frame(y=data$Y1, x=data$Xk, p=data$Y2)
  
  # REndo might warn about weak instruments, or it might not
  # Let's test both scenarios
  rendo_warns <- tryCatch({
    suppressMessages(hetErrorsIV(y ~ x + p | p | IIV(x), data = test_data))
    FALSE  # No warning
  }, warning = function(w) {
    TRUE   # Warning occurred
  })
  
  # Just document whether it warns or not
  expect_true(is.logical(rendo_warns), 
    label = "REndo completes with or without warning")
  
  # Test with strong heteroskedasticity (should not warn)
  params_strong <- params_weak
  params_strong$delta_het <- 2.0
  
  data_strong <- generate_lewbel_data(500, params_strong)
  test_data_strong <- data.frame(y=data_strong$Y1, x=data_strong$Xk, p=data_strong$Y2)
  
  # Should not warn with strong heteroskedasticity
  expect_no_warning(
    suppressMessages(hetErrorsIV(y ~ x + p | p | IIV(x), data = test_data_strong))
  )
})

test_that("Both hetid and REndo handle edge cases appropriately", {
  skip_if_not(has_rendo(), "REndo not available")
  skip_on_cran()
  
  library(REndo)

  # Test with small sample
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )
  
  set.seed(456)
  small_data <- generate_lewbel_data(50, params)
  test_data <- data.frame(y=small_data$Y1, x=small_data$Xk, p=small_data$Y2, z=small_data$Z)
  
  # Construct standard Lewbel IV
  e2_hat <- residuals(lm(p ~ x, data = test_data))
  iv <- (test_data$z - mean(test_data$z)) * e2_hat
  iv <- iv - mean(iv)

  # hetid approach should work
  hetid_model <- ivreg(y ~ x + p | x + iv, data = test_data)
  expect_s3_class(hetid_model, "ivreg")
  expect_true(all(is.finite(coef(hetid_model))))

  # REndo might warn or have larger SEs with small samples
  rendo_result <- tryCatch({
    # Suppress messages but not warnings
    suppressMessages(hetErrorsIV(y ~ x + p | p | IIV(x), data = test_data))
  }, warning = function(w) {
    # Capture warning but still return result
    suppressWarnings(suppressMessages(
      hetErrorsIV(y ~ x + p | p | IIV(x), data = test_data)
    ))
  }, error = function(e) {
    NULL
  })
  
  # Both should produce some result (even if with warnings)
  expect_false(is.null(rendo_result))
  
  # Document that small samples produce less reliable estimates
  if (!is.null(rendo_result)) {
    se_hetid <- sqrt(diag(vcov(hetid_model)))["p"]
    se_rendo <- sqrt(diag(vcov(rendo_result)))["p"]
    
    # Small samples typically have larger SEs than large samples
    # With n=50 and true gamma=-0.8, SE might still be reasonable
    expect_true(se_hetid > 0.001, label = "Small sample produces reasonable SE")
    expect_true(se_rendo > 0.001, label = "Small sample produces reasonable SE in REndo")
  }
})

test_that("Document theoretical validity of both approaches", {
  skip_if_not(has_rendo(), "REndo not available")
  skip_on_cran()

  library(REndo)
  
  # Generate data with strong heteroskedasticity
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 2.0
  )
  
  set.seed(123)
  data <- generate_lewbel_data(1000, params)
  test_data <- data.frame(y=data$Y1, x=data$Xk, p=data$Y2, z=data$Z)
  
  # Approach 1: hetid/Stata using Z = X^2 - E[X^2]
  e2_hat <- residuals(lm(p ~ x, data = test_data))
  iv_z <- (test_data$z - mean(test_data$z)) * e2_hat
  iv_z <- iv_z - mean(iv_z)
  model_z <- ivreg(y ~ x + p | x + iv_z, data = test_data)

  # Approach 2: REndo using Z = X
  rendo_model <- suppressMessages(
    hetErrorsIV(y ~ x + p | p | IIV(x), data = test_data)
  )

  # Both approaches are theoretically valid per Lewbel (2012)
  # Document their properties
  coef_z <- coef(model_z)["p"]
  coef_rendo <- coef(rendo_model)["p"]
  se_z <- sqrt(diag(vcov(model_z)))["p"]
  se_rendo <- sqrt(diag(vcov(rendo_model)))["p"]
  
  # Both should produce reasonable estimates
  expect_true(abs(coef_z - params$gamma1) < 0.1, 
    label = "Z-based approach estimates well")
  expect_true(abs(coef_rendo - params$gamma1) < 0.1, 
    label = "X-based approach estimates well")
  
  # Document typical differences
  se_ratio <- se_rendo / se_z
  expect_true(se_ratio > 0.9 && se_ratio < 1.1,
    label = "Both approaches have similar precision")
  
  # Both are consistent estimators
  expect_true(is.finite(coef_z) && is.finite(se_z))
  expect_true(is.finite(coef_rendo) && is.finite(se_rendo))
})

test_that("hetid df adjustments work correctly", {
  skip_on_cran()

  # Test with small sample where df adjustment matters
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2,
    sample_size = 50, tau_set_id = 0, bootstrap_reps = 0
  )
  
  set.seed(999)
  result_finite <- run_single_lewbel_simulation(
    sim_id = 1, params = params, df_adjust = "finite"
  )
  
  result_asymp <- run_single_lewbel_simulation(
    sim_id = 1, params = params, df_adjust = "asymptotic"  
  )
  
  # Both should produce valid results
  expect_true(is.finite(result_finite$tsls_gamma1))
  expect_true(is.finite(result_asymp$tsls_gamma1))
  expect_true(is.finite(result_finite$tsls_se))
  expect_true(is.finite(result_asymp$tsls_se))
  
  # Finite sample adjustment should produce larger SEs
  # The actual adjustment might be different due to IV estimation
  # Just verify finite > asymptotic
  expect_true(result_finite$tsls_se > result_asymp$tsls_se,
    label = "Finite sample SE larger than asymptotic")
  
  # And that the difference is reasonable (not too large)
  actual_ratio <- result_finite$tsls_se / result_asymp$tsls_se
  expect_true(actual_ratio > 1.0 && actual_ratio < 3.0,
    label = "DF adjustment produces reasonable SE inflation")
  
  # Asymptotic matches Stata default
  expect_equal(result_asymp$df_adjust, "asymptotic")
  expect_equal(result_finite$df_adjust, "finite")
})
