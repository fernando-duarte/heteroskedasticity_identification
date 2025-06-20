# Test edge cases for data generation validation and assumptions

test_that("verify_lewbel_assumptions handles perfect correlations", {
  skip_if_not_comprehensive_test()
  # Create data with perfect correlation between Z and epsilon1*epsilon2
  # This violates the key assumption
  n_test <- 1000
  set.seed(123)

  # Manually create data that violates assumptions
  test_data <- data.frame(
    Xk = rnorm(n_test),
    Z = rnorm(n_test),
    epsilon1 = rnorm(n_test),
    epsilon2 = rnorm(n_test)
  )

  # Make epsilon1*epsilon2 perfectly correlated with Z
  test_data$epsilon1 <- test_data$Z
  test_data$epsilon2 <- 1

  # Add Y1 and Y2 to make it valid
  test_data$Y1 <- rnorm(n_test)
  test_data$Y2 <- rnorm(n_test)

  config <- create_default_config()

  # Should detect violation
  result <- verify_lewbel_assumptions(
    test_data, config,
    verbose = FALSE
  )

  expect_true(abs(result$cov_z_e1e2) > 0.9) # High correlation
  expect_true(result$p_value < 0.001) # Should reject null
})

test_that("verify_lewbel_assumptions handles zero variance", {
  skip_if_not_comprehensive_test()
  # Create data with zero variance in Z
  n_test <- 100
  test_data <- data.frame(
    Xk = rnorm(n_test),
    Z = rep(0, n_test), # Zero variance
    epsilon1 = rnorm(n_test),
    epsilon2 = rnorm(n_test),
    Y1 = rnorm(n_test),
    Y2 = rnorm(n_test)
  )

  config <- create_default_config()

  result <- verify_lewbel_assumptions(
    test_data, config,
    verbose = FALSE
  )

  # With zero variance in Z, covariances should be zero
  expect_equal(result$cov_z_e1e2, 0)
  expect_equal(result$cov_z_e2sq, 0)
})

test_that("verify_lewbel_assumptions handles NA values", {
  skip_if_not_comprehensive_test()
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  data_with_na <- generate_lewbel_data(100, params)

  # Introduce NA values
  data_with_na$Z[c(1, 5, 10)] <- NA
  data_with_na$epsilon1[c(2, 6)] <- NA
  data_with_na$epsilon2[c(3, 7)] <- NA

  config <- create_default_config()

  # Should handle NA values gracefully
  expect_no_error({
    result <- verify_lewbel_assumptions(
      data_with_na, config,
      verbose = FALSE
    )
  })
})

test_that("verify_lewbel_assumptions with extreme n_obs", {
  skip_if_not_comprehensive_test()
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  # Very small n_obs
  result_small <- verify_lewbel_assumptions(
    n_obs = 10, params = params, verbose = FALSE
  )
  expect_type(result_small$p_value, "double")

  # Test with n_obs as numeric (not integer)
  result_numeric <- verify_lewbel_assumptions(
    n_obs = 100.5, params = params, verbose = FALSE
  )
  expect_equal(nrow(result_numeric$data), 100) # Should truncate to 100
})

test_that("data generation maintains expected relationships", {
  skip_if_not_comprehensive_test()
  # Test that the single-factor structure is maintained
  params <- list(
    beta1_0 = 0, beta1_1 = 0, gamma1 = 0,
    beta2_0 = 0, beta2_1 = 0,
    alpha1 = 1, alpha2 = 1, # Same loading
    delta_het = 0 # No heteroskedasticity
  )

  set.seed(42)
  data_test <- generate_lewbel_data(1000, params)

  # With same alpha and no other effects, correlation should be positive
  # but the exact value depends on random generation
  cor_errors <- cor(data_test$epsilon1, data_test$epsilon2)
  expect_true(cor_errors > 0.3) # Should be positively correlated

  # Test independence of Z and epsilon1*epsilon2
  cor_z_e1e2 <- cor(data_test$Z, data_test$epsilon1 * data_test$epsilon2)
  expect_true(abs(cor_z_e1e2) < 0.1) # Should be close to zero
})

test_that("verify_lewbel_assumptions handles extreme cases", {
  skip_if_not_comprehensive_test()
  # Test with data that has known properties
  n_test <- 100
  config <- create_default_config()

  # Case 1: Normal data generated by the package
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  test_data <- generate_lewbel_data(n_test, params)

  # Should run without error and produce expected output
  output <- capture.output({
    result <- verify_lewbel_assumptions(test_data, config, verbose = TRUE)
  })

  # Basic checks
  expect_type(result, "list")
  expect_true("cov_z_e1e2" %in% names(result))
  expect_true("p_value" %in% names(result))
  expect_true(any(grepl("Cov\\(Z, e1\\*e2\\)", output)))

  # Case 2: Data with perfect identification (should pass)
  expect_true(result$p_value > 0.01) # Should not reject null
})
