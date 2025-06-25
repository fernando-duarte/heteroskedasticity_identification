# Test edge cases and error handling

test_that("verify_lewbel_assumptions error handling works", {
  # Test with missing parameters
  expect_error(
    verify_lewbel_assumptions(),
    "Must provide either \\(data, config\\) or \\(n_obs, params\\)"
  )

  # Test with incomplete parameters
  expect_error(
    verify_lewbel_assumptions(n_obs = 100),
    "Must provide either \\(data, config\\) or \\(n_obs, params\\)"
  )
})

test_that("generate_lewbel_data with small sample size works", {
  params <- list(
    beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
    beta2_0 = 1.0, beta2_1 = -1.0,
    alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
  )

  # Test with very small sample size
  data <- generate_lewbel_data(n_tiny / 2, params)
  assert_valid_dataframe(data, expected_rows = n_tiny / 2)
})

test_that("calculate_lewbel_bounds with extreme tau values works", {
  config <- create_default_config()
  data <- generate_lewbel_data(n_small, config)

  # Test with very small tau
  bounds_small <- calculate_lewbel_bounds(data, tau = alpha_conservative)
  assert_list_structure(bounds_small, "bounds")

  # Test with larger tau
  bounds_large <- calculate_lewbel_bounds(data, tau = test_tolerance_loose)
  assert_list_structure(bounds_large, "bounds")
})

test_that("create_default_config with extreme values works", {
  # Test with very small num_simulations
  config_small <- create_default_config(num_simulations = 1)
  expect_equal(config_small$num_simulations, 1)

  # Test with very small sample size
  config_tiny_n <- create_default_config(main_sample_size = n_tiny)
  expect_equal(config_tiny_n$main_sample_size, n_tiny)
})

test_that("generate_seed_matrix with edge cases works", {
  # Test with single row
  seeds_single <- generate_seed_matrix(seed_default, 1, n_sim_tiny)
  expect_equal(dim(seeds_single), c(1, n_sim_tiny))

  # Test with single column
  seeds_col <- generate_seed_matrix(seed_default, 3, 1)
  expect_equal(dim(seeds_col), c(3, 1))

  # Test with 1x1 matrix
  seeds_one <- generate_seed_matrix(seed_default, 1, 1)
  expect_equal(dim(seeds_one), c(1, 1))
})

test_that("simulation functions handle minimal configurations", {
  # Test with minimal simulation configuration
  config_minimal <- create_default_config(num_simulations = 1)

  # Test single simulation
  # The simulation may generate warnings about data length or matrix operations
  # These are expected in edge cases with small sample sizes
  suppressWarnings({
    single_result <- run_single_lewbel_simulation(1, config_minimal)
  })
  assert_list_structure(single_result, c("ols_gamma1", "tsls_gamma1"))

  # Test main simulation with 1 run
  seeds_minimal <- generate_all_seeds(config_minimal)
  suppressMessages({
    main_results_minimal <- run_main_simulation(config_minimal, seeds_minimal)
  })
  expect_equal(nrow(main_results_minimal), 1)
})

test_that("analysis functions handle minimal data", {
  config_minimal <- create_default_config(num_simulations = 1)
  seeds_minimal <- generate_all_seeds(config_minimal)

  suppressMessages({
    main_results_minimal <- run_main_simulation(config_minimal, seeds_minimal)
    analysis_minimal <- analyze_main_results(
      main_results_minimal, config_minimal,
      verbose = FALSE
    )
  })

  expect_type(analysis_minimal, "list")
  expect_s3_class(analysis_minimal$summary_table, "data.frame")
  expect_true(nrow(analysis_minimal$summary_table) >= 1)
})

test_that("print functions handle various input types", {
  # Test print_simulation_summary with different verbose types
  expect_output(
    print_simulation_summary(verbose = c(TRUE, FALSE)) # Vector input
  )
  expect_output(print_simulation_summary(verbose = list(TRUE))) # List input
  expect_silent(print_simulation_summary(verbose = 1)) # Numeric input
})
