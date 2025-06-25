# Test error handling and edge cases in estimation.R

test_that("calculate_lewbel_bounds handles weak identification", {
  # Create data where cov_z_w2sq is very small
  params <- create_test_params(delta_het = delta_het_none)
  params$alpha1 <- 0
  params$alpha2 <- 0

  data <- create_test_data(n = n_small, params = params)
  # Manually set Z to be constant to trigger weak identification
  data$Z <- rep(1, nrow(data))

  bounds <- calculate_lewbel_bounds(data, tau = test_tolerance_normal)

  # Should return NA bounds due to weak identification
  expect_true(all(is.na(bounds$bounds)))
  expect_true(all(is.na(bounds$se)))
})

test_that("calculate_lewbel_bounds handles negative discriminant", {
  # Create a small dataset that might produce negative discriminant
  params <- create_test_params(delta_het = delta_het_weak)

  # Use very small sample size and high tau
  set.seed(seed_series[2])
  data <- create_test_data(n = n_tiny, params = params)

  bounds <- calculate_lewbel_bounds(data, tau = 1 - alpha_conservative)

  # Check that it handles the case gracefully
  assert_list_structure(bounds, c("bounds", "se"))
})

test_that("calculate_lewbel_bounds handles bootstrap failures", {
  # Create data that causes bootstrap to fail
  params <- create_test_params()

  # Very small dataset
  set.seed(seed_series[3])
  data <- create_test_data(n = n_tiny / 2, params = params)

  # Try to compute bootstrap SE with very small data
  bounds <- calculate_lewbel_bounds(
    data,
    tau = test_tolerance_normal,
    compute_se = TRUE,
    b_reps = n_boot_tiny
  )

  assert_list_structure(bounds, c("bounds", "se"))
})

test_that("run_single_lewbel_simulation handles OLS failures", {
  # Create problematic data
  params <- list(
    sample_size = n_tiny / 2, # Very small
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = test_gamma1_true,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = delta_het_moderate,
    tau_set_id = test_tolerance_normal, bootstrap_reps = n_boot_tiny
  )

  # Mock data generation to produce problematic data
  set.seed(seed_series[4])
  result <- run_single_lewbel_simulation(1, params)

  # Should return a data frame even with failures
  assert_valid_dataframe(result)
  assert_list_structure(result, c("ols_gamma1", "tsls_gamma1"))
})

test_that("run_single_lewbel_simulation handles weak instruments", {
  # Create params that lead to weak instruments
  base_params <- create_test_params(delta_het = delta_het_weak / 10)
  params <- c(
    list(sample_size = n_small / 2.5, tau_set_id = test_tolerance_normal, bootstrap_reps = n_boot_tiny),
    base_params
  )
  params$alpha1 <- delta_het_weak / 10
  params$alpha2 <- delta_het_weak / 10

  set.seed(seed_series[5])
  result <- run_single_lewbel_simulation(1, params)

  assert_valid_dataframe(result)
  # Should return a valid data frame with all required columns
  assert_list_structure(result, c("ols_gamma1", "tsls_gamma1", "first_stage_F"))
})

test_that("run_single_lewbel_simulation handles ivreg failures", {
  # Test with data that might cause ivreg to fail
  params <- list(
    sample_size = n_tiny, # Very small for IV estimation
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = test_gamma1_true,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = test_tolerance_loose,
    tau_set_id = test_tolerance_normal, bootstrap_reps = n_boot_tiny
  )

  set.seed(seed_series[1] * 3)
  result <- run_single_lewbel_simulation(1, params)

  assert_valid_dataframe(result, expected_rows = 1)
  # Check that it returns proper structure even with potential failures
  assert_list_structure(result, c("ols_gamma1", "tsls_gamma1", "first_stage_F"))
})

test_that("run_single_lewbel_simulation complete failure handling", {
  # Test the outer error handler by passing invalid params
  params <- list(
    sample_size = -n_tiny, # Invalid sample size
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = test_gamma1_true,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = delta_het_moderate,
    tau_set_id = test_tolerance_normal, bootstrap_reps = n_boot_tiny
  )

  # This should trigger the outer error handler
  result <- suppressWarnings(
    run_single_lewbel_simulation(1, params)
  )

  assert_valid_dataframe(result, expected_rows = 1)
  # Should return all NA values
  expect_true(is.na(result$ols_gamma1))
  expect_true(is.na(result$tsls_gamma1))
  expect_true(is.na(result$first_stage_F))
})

test_that("calculate_lewbel_bounds bootstrap SE computation", {
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = test_gamma1_true,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = delta_het_moderate
  )

  data <- generate_lewbel_data(n_medium, params)

  # Test bootstrap SE computation
  bounds_with_se <- calculate_lewbel_bounds(
    data,
    tau = test_tolerance_normal,
    compute_se = TRUE,
    b_reps = n_boot_medium * 2.5
  )

  assert_list_structure(bounds_with_se, c("bounds", "se"))
  expect_true(length(bounds_with_se$se) == 2)
  expect_true(all(!is.na(bounds_with_se$se)))
})
