# Test simulation functions

test_that("run_single_lewbel_simulation works", {
  skip_if_not_fast_test()
  config <- create_default_config()

  # The simulation may generate warnings about data length or matrix operations
  # These are expected in edge cases with small sample sizes
  suppressWarnings({
    single_sim <- run_single_lewbel_simulation(1, config)
  })

  expect_s3_class(single_sim, "data.frame")
  expect_true(all(c(
    "ols_gamma1", "tsls_gamma1", "first_stage_F", "bound_lower_tau_set",
    "bound_upper_tau_set"
  ) %in% names(single_sim)))
  expect_type(single_sim$ols_gamma1, "double")
  expect_type(single_sim$tsls_gamma1, "double")
  expect_type(single_sim$first_stage_F, "double")
  # Check that all values in first_stage_F are numeric
  expect_true(all(is.numeric(single_sim$first_stage_F)))
})

test_that("run_main_simulation works", {
  skip_if_not_fast_test()
  config_small <- create_default_config(num_simulations = 3)
  seeds_small <- generate_all_seeds(config_small)

  # Suppress output during testing
  suppressMessages({
    main_results <- run_main_simulation(config_small, seeds_small)
  })

  expect_s3_class(main_results, "data.frame")
  expect_equal(nrow(main_results), 3)
  expect_true(all(c(
    "ols_gamma1", "tsls_gamma1", "first_stage_F"
  ) %in% names(main_results)))
  expect_true(all(!is.na(main_results$ols_gamma1)))

  # 2SLS results depend on availability of AER or ivreg packages
  if (requireNamespace("AER", quietly = TRUE) ||
    requireNamespace("ivreg", quietly = TRUE)) {
    expect_true(all(!is.na(main_results$tsls_gamma1)))
  } else {
    # If neither package is available, tsls_gamma1 should be NA
    expect_true(all(is.na(main_results$tsls_gamma1)))
  }
})

test_that("run_sample_size_analysis works", {
  skip_if_not_fast_test()
  config_tiny <- create_default_config(
    n_reps_by_n = 2, sample_sizes = c(100, 200)
  )
  seeds_tiny <- generate_all_seeds(config_tiny)

  suppressMessages({
    sample_results <- run_sample_size_analysis(config_tiny, seeds_tiny)
  })

  expect_s3_class(sample_results, "data.frame")
  expect_true("sample_size" %in% names(sample_results))
  expect_true(all(sample_results$sample_size %in% c(100, 200)))
  expect_true(nrow(sample_results) >= 4) # At least 2 reps * 2 sample sizes
})

test_that("run_sensitivity_analysis works", {
  skip_if_not_fast_test()
  config_sens <- create_default_config(
    n_reps_by_delta = 2, delta_het_values = c(0.8, 1.2)
  )
  seeds_sens <- generate_all_seeds(config_sens)

  suppressMessages({
    sens_results <- run_sensitivity_analysis(config_sens, seeds_sens)
  })

  expect_s3_class(sens_results, "data.frame")
  expect_true("delta_het" %in% names(sens_results))
  expect_true(all(sens_results$delta_het %in% c(0.8, 1.2)))
  expect_true(nrow(sens_results) >= 4) # At least 2 reps * 2 delta values
})

test_that("run_bootstrap_demonstration works", {
  skip_if_not_fast_test()
  config_boot <- create_default_config(
    bootstrap_demo_size = 2, bootstrap_reps = 5
  )
  seeds_boot <- generate_all_seeds(config_boot)

  suppressMessages({
    bootstrap_demo <- run_bootstrap_demonstration(config_boot, seeds_boot)
  })

  expect_s3_class(bootstrap_demo, "data.frame")
  expect_true("sim_id" %in% names(bootstrap_demo))
  expect_true(nrow(bootstrap_demo) >= 2) # At least 2 demo_size
  expect_true(all(c(
    "bound_lower_tau_set", "bound_upper_tau_set"
  ) %in% names(bootstrap_demo)))
})

test_that("run_lewbel_demo works", {
  skip_if_not_fast_test()
  suppressMessages({
    demo_results <- run_lewbel_demo(num_simulations = 2, verbose = FALSE)
  })

  expect_type(demo_results, "list")
  expect_true(all(c(
    "config", "results_main", "analysis"
  ) %in% names(demo_results)))
  expect_s3_class(demo_results$results_main, "data.frame")
  expect_type(demo_results$analysis, "list")
})
