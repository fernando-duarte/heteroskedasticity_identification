# Test data generation functions

test_that("generate_lewbel_data works", {
  skip_if_not_fast_test()
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = test_gamma1_true,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = delta_het_moderate
  )
  data <- generate_lewbel_data(n_medium, params)

  assert_valid_dataframe(data, expected_rows = n_medium,
                        required_cols = lewbel_data_cols)
  assert_valid_numeric(data$Y1)
  assert_valid_numeric(data$Y2)
  assert_valid_numeric(data$Xk)
})

test_that("generate_lewbel_data with config works", {
  skip_if_not_fast_test()
  config <- create_default_config()
  data <- generate_lewbel_data(n_small, config)

  assert_valid_dataframe(data, expected_rows = n_small,
                        required_cols = lewbel_data_cols)
})

test_that("verify_lewbel_assumptions with data and config works", {
  skip_if_not_fast_test()
  config <- create_default_config()
  data <- generate_lewbel_data(n_medium, config)

  # Test with verbose = FALSE to avoid output during testing
  verification <- verify_lewbel_assumptions(data, config, verbose = FALSE)

  assert_list_structure(verification, c(
    "cov_z_e1e2", "cov_z_e2sq", "cov_e1_e2", "test_stat",
    "p_value", "data"
  ))
  assert_valid_numeric(verification$cov_z_e1e2)
  assert_valid_numeric(verification$p_value)
  assert_valid_dataframe(verification$data)
})

test_that("verify_lewbel_assumptions with params works", {
  skip_if_not_fast_test()
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = test_gamma1_true,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = delta_het_moderate
  )

  verification <- verify_lewbel_assumptions(
    n_obs = n_medium, params = params, verbose = FALSE
  )

  assert_list_structure(verification, c("cov_z_e1e2", "cov_z_e2sq", "cov_e1_e2"))
  assert_valid_dataframe(verification$data, expected_rows = n_medium)
})

test_that("calculate_lewbel_bounds works", {
  skip_if_not_fast_test()
  config <- create_default_config()
  data <- generate_lewbel_data(n_medium, config)

  bounds <- calculate_lewbel_bounds(data, tau = 0.0)
  assert_list_structure(bounds, "bounds")
  expect_equal(length(bounds$bounds), 2)
  # Check that lower bound is less than or equal to upper bound
  expect_true(bounds$bounds[1] <= bounds$bounds[2])
})

test_that("calculate_lewbel_bounds with different tau values works", {
  skip_if_not_fast_test()
  config <- create_default_config()
  data <- generate_lewbel_data(n_medium, config)

  bounds1 <- calculate_lewbel_bounds(data, tau = test_tolerance_tight)
  bounds2 <- calculate_lewbel_bounds(data, tau = test_tolerance_normal * 1.5)

  assert_valid_numeric(bounds1$bounds)
  assert_valid_numeric(bounds2$bounds)
  expect_equal(length(bounds1$bounds), 2)
  expect_equal(length(bounds2$bounds), 2)
})
