# Test data generation functions

test_that("generate_lewbel_data works", {
  skip_if_not_fast_test()
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )
  data <- generate_lewbel_data(100, params)

  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 100)
  # Correct expected column names based on actual function implementation
  expect_true(all(
    c("Y1", "Y2", "Xk", "Z", "epsilon1", "epsilon2") %in% names(data)
  ))
  expect_type(data$Y1, "double")
  expect_type(data$Y2, "double")
  expect_type(data$Xk, "double")
})

test_that("generate_lewbel_data with config works", {
  skip_if_not_fast_test()
  config <- create_default_config()
  data <- generate_lewbel_data(50, config)

  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 50)
  expect_true(all(
    c("Y1", "Y2", "Xk", "Z", "epsilon1", "epsilon2") %in% names(data)
  ))
})

test_that("verify_lewbel_assumptions with data and config works", {
  skip_if_not_fast_test()
  config <- create_default_config()
  data <- generate_lewbel_data(100, config)

  # Test with verbose = FALSE to avoid output during testing
  verification <- verify_lewbel_assumptions(data, config, verbose = FALSE)

  expect_type(verification, "list")
  expect_true(all(c(
    "cov_z_e1e2", "cov_z_e2sq", "cov_e1_e2", "test_stat",
    "p_value", "data"
  ) %in% names(verification)))
  expect_type(verification$cov_z_e1e2, "double")
  expect_type(verification$p_value, "double")
  expect_s3_class(verification$data, "data.frame")
})

test_that("verify_lewbel_assumptions with params works", {
  skip_if_not_fast_test()
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  verification <- verify_lewbel_assumptions(
    n_obs = 100, params = params, verbose = FALSE
  )

  expect_type(verification, "list")
  expect_true(all(
    c("cov_z_e1e2", "cov_z_e2sq", "cov_e1_e2") %in% names(verification)
  ))
  expect_equal(nrow(verification$data), 100)
})

test_that("calculate_lewbel_bounds works", {
  skip_if_not_fast_test()
  config <- create_default_config()
  data <- generate_lewbel_data(100, config)

  bounds <- calculate_lewbel_bounds(data, tau = 0.0)
  expect_type(bounds, "list")
  expect_equal(length(bounds$bounds), 2)
  # Check that lower bound is less than or equal to upper bound
  expect_true(bounds$bounds[1] <= bounds$bounds[2])
})

test_that("calculate_lewbel_bounds with different tau values works", {
  skip_if_not_fast_test()
  config <- create_default_config()
  data <- generate_lewbel_data(100, config)

  bounds1 <- calculate_lewbel_bounds(data, tau = 0.1)
  bounds2 <- calculate_lewbel_bounds(data, tau = 0.3)

  expect_type(bounds1$bounds, "double")
  expect_type(bounds2$bounds, "double")
  expect_equal(length(bounds1$bounds), 2)
  expect_equal(length(bounds2$bounds), 2)
})
