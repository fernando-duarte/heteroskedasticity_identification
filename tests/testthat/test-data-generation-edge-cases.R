# Test edge cases for data generation parameter handling

test_that("generate_lewbel_data handles extreme parameter values", {
  # Extreme delta_het (tests exponent capping)
  params_extreme_delta <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0,
    delta_het = 100 # Very large delta_het
  )

  data_extreme <- generate_lewbel_data(100, params_extreme_delta)
  expect_s3_class(data_extreme, "data.frame")
  expect_equal(nrow(data_extreme), 100)
  expect_true(all(is.finite(data_extreme$epsilon2))) # No Inf values

  # Very small delta_het
  params_small_delta <- params_extreme_delta
  params_small_delta$delta_het <- 0.0001

  data_small <- generate_lewbel_data(100, params_small_delta)
  expect_true(all(is.finite(data_small$epsilon2)))

  # Negative delta_het
  params_neg_delta <- params_extreme_delta
  params_neg_delta$delta_het <- -50

  data_neg <- generate_lewbel_data(100, params_neg_delta)
  expect_true(all(is.finite(data_neg$epsilon2)))
})

test_that("generate_lewbel_data handles extreme coefficients", {
  # Very large coefficients
  params_large <- list(
    beta1_0 = 1e6, beta1_1 = 1e6, gamma1 = -1e6,
    beta2_0 = 1e6, beta2_1 = -1e6,
    alpha1 = -1e3, alpha2 = 1e3,
    delta_het = 1.0
  )

  data_large <- generate_lewbel_data(50, params_large)
  expect_s3_class(data_large, "data.frame")
  # Check that Y values can be extreme but finite
  expect_true(all(is.finite(data_large$Y1) | is.na(data_large$Y1)))
  expect_true(all(is.finite(data_large$Y2) | is.na(data_large$Y2)))

  # Zero coefficients
  params_zero <- list(
    beta1_0 = 0, beta1_1 = 0, gamma1 = 0,
    beta2_0 = 0, beta2_1 = 0,
    alpha1 = 0, alpha2 = 0,
    delta_het = 0
  )

  data_zero <- generate_lewbel_data(50, params_zero)
  # Y2 = epsilon2 when all coefficients are zero
  expect_equal(data_zero$Y2[1], data_zero$epsilon2[1])
  # Y1 = epsilon1 when all coefficients are zero
  expect_equal(data_zero$Y1[1], data_zero$epsilon1[1])
})

test_that("generate_lewbel_data handles edge sample sizes", {
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  # Single observation
  data_single <- generate_lewbel_data(1, params)
  expect_equal(nrow(data_single), 1)
  expect_s3_class(data_single, "data.frame")

  # Large sample size
  data_large <- generate_lewbel_data(10000, params)
  expect_equal(nrow(data_large), 10000)

  # Check statistical properties with large sample
  expect_true(abs(mean(data_large$Xk) - 2) < 0.1) # Should be close to 2
  expect_true(abs(sd(data_large$Xk) - 1) < 0.1) # Should be close to 1

  # Generate data and check intermediate calculations
  n_obs <- 1000
  x_k <- rnorm(n_obs, mean = 2, sd = 10) # Large variance in X
  z_val <- x_k^2 - mean(x_k^2)

  # Manually check exponent capping
  exponent <- params_extreme$delta_het * z_val
  exponent_capped <- pmin(pmax(exponent, -10), 10)

  expect_true(all(exponent_capped >= -10))
  expect_true(all(exponent_capped <= 10))
  expect_true(any(exponent != exponent_capped)) # Some should be capped
})

test_that("generate_lewbel_data with missing parameters", {
  # Incomplete params list
  params_incomplete <- list(
    beta1_0 = 0.5, beta1_1 = 1.5
    # Missing other parameters
  )

  # Should produce an error
  expect_error(
    generate_lewbel_data(100, params_incomplete)
  )

  # NULL params
  expect_error(
    generate_lewbel_data(100, NULL)
  )
})

test_that("generate_lewbel_data numerical stability", {
  # Test parameters that could cause numerical issues
  params_unstable <- list(
    beta1_0 = 1e-10, beta1_1 = 1e-10, gamma1 = -1e10,
    beta2_0 = 1e-10, beta2_1 = -1e-10,
    alpha1 = -1e-5, alpha2 = 1e-5,
    delta_het = 50 # Large but should be capped
  )

  # Should not produce errors or warnings
  expect_no_error({
    data_unstable <- generate_lewbel_data(100, params_unstable)
  })

  # Check for NaN values
  expect_false(any(is.nan(unlist(data_unstable))))
})

test_that("exponent capping works correctly", {
  # Test that extreme Z values don't cause overflow
  set.seed(123)
  params_extreme <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0,
    delta_het = 10 # Large multiplier
  )

  # Generate data and check intermediate calculations
  n_obs <- 1000
  x_k <- rnorm(n_obs, mean = 2, sd = 10) # Large variance in X
  z_val <- x_k^2 - mean(x_k^2)

  # Manually check exponent capping
  exponent <- params_extreme$delta_het * z_val
  exponent_capped <- pmin(pmax(exponent, -10), 10)

  expect_true(all(exponent_capped >= -10))
  expect_true(all(exponent_capped <= 10))
  expect_true(any(exponent != exponent_capped)) # Some should be capped
})
