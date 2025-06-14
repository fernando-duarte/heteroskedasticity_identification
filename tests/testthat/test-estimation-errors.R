# Test error handling and edge cases in estimation.R

test_that("calculate_lewbel_bounds handles weak identification", {
  # Create data where cov_z_w2sq is very small
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = 0, alpha2 = 0, delta_het = 0 # No heteroskedasticity
  )

  set.seed(123)
  data <- generate_lewbel_data(50, params)
  # Manually set Z to be constant to trigger weak identification
  data$Z <- rep(1, nrow(data))

  bounds <- calculate_lewbel_bounds(data, tau = 0.2)

  # Should return NA bounds due to weak identification
  expect_true(all(is.na(bounds$bounds)))
  expect_true(all(is.na(bounds$se)))
})

test_that("calculate_lewbel_bounds handles negative discriminant", {
  # Create a small dataset that might produce negative discriminant
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 0.1
  )

  # Use very small sample size and high tau
  set.seed(456)
  data <- generate_lewbel_data(10, params)

  bounds <- calculate_lewbel_bounds(data, tau = 0.99)

  # Check that it handles the case gracefully
  expect_type(bounds, "list")
  expect_true("bounds" %in% names(bounds))
  expect_true("se" %in% names(bounds))
})

test_that("calculate_lewbel_bounds handles bootstrap failures", {
  # Create data that causes bootstrap to fail
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  # Very small dataset
  set.seed(789)
  data <- generate_lewbel_data(5, params)

  # Try to compute bootstrap SE with very small data
  bounds <- calculate_lewbel_bounds(
    data,
    tau = 0.2,
    compute_se = TRUE,
    b_reps = 10
  )

  expect_type(bounds, "list")
  expect_true("bounds" %in% names(bounds))
  expect_true("se" %in% names(bounds))
})

test_that("run_single_lewbel_simulation handles OLS failures", {
  # Create problematic data
  params <- list(
    sample_size = 5, # Very small
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2,
    tau_set_id = 0.2, bootstrap_reps = 5
  )

  # Mock data generation to produce problematic data
  set.seed(111)
  result <- run_single_lewbel_simulation(1, params)

  # Should return a data frame even with failures
  expect_s3_class(result, "data.frame")
  expect_true("ols_gamma1" %in% names(result))
  expect_true("tsls_gamma1" %in% names(result))
})

test_that("run_single_lewbel_simulation handles weak instruments", {
  # Create params that lead to weak instruments
  params <- list(
    sample_size = 20, # Small sample size
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = 0.01, alpha2 = 0.01, delta_het = 0.01, # Very weak heteroskedasticity
    tau_set_id = 0.2, bootstrap_reps = 5
  )

  set.seed(222)
  result <- run_single_lewbel_simulation(1, params)

  expect_s3_class(result, "data.frame")
  # Should return a valid data frame with all required columns
  expect_true(all(
    c("ols_gamma1", "tsls_gamma1", "first_stage_F") %in% names(result)
  ))
})

test_that("run_single_lewbel_simulation handles ivreg failures", {
  # Test with data that might cause ivreg to fail
  params <- list(
    sample_size = 10, # Very small for IV estimation
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 0.5,
    tau_set_id = 0.2, bootstrap_reps = 5
  )

  set.seed(333)
  result <- run_single_lewbel_simulation(1, params)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  # Check that it returns proper structure even with potential failures
  expect_true(all(
    c("ols_gamma1", "tsls_gamma1", "first_stage_F") %in% names(result)
  ))
})

test_that("run_single_lewbel_simulation complete failure handling", {
  # Test the outer error handler by passing invalid params
  params <- list(
    sample_size = -10, # Invalid sample size
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2,
    tau_set_id = 0.2, bootstrap_reps = 5
  )

  # This should trigger the outer error handler
  result <- suppressWarnings(
    run_single_lewbel_simulation(1, params)
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  # Should return all NA values
  expect_true(is.na(result$ols_gamma1))
  expect_true(is.na(result$tsls_gamma1))
  expect_true(is.na(result$first_stage_F))
})

test_that("calculate_lewbel_bounds bootstrap SE computation", {
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  data <- generate_lewbel_data(100, params)

  # Test bootstrap SE computation
  bounds_with_se <- calculate_lewbel_bounds(
    data,
    tau = 0.2,
    compute_se = TRUE,
    b_reps = 50
  )

  expect_type(bounds_with_se, "list")
  expect_true("bounds" %in% names(bounds_with_se))
  expect_true("se" %in% names(bounds_with_se))
  expect_true(length(bounds_with_se$se) == 2)
  expect_true(all(!is.na(bounds_with_se$se)))
})
