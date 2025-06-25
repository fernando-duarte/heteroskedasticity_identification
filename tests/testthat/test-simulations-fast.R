# Test simulation functions

test_that("run_single_lewbel_simulation works", {
  skip_if_not_fast_test()
  config <- create_default_config()

  # The simulation may generate warnings about data length or matrix operations
  # These are expected in edge cases with small sample sizes
  suppressWarnings({
    single_sim <- run_single_lewbel_simulation(1, config)
  })

  assert_valid_dataframe(single_sim)
  assert_list_structure(single_sim, c(
    "ols_gamma1", "tsls_gamma1", "first_stage_F", "bound_lower_tau_set",
    "bound_upper_tau_set"
  ))
  assert_valid_numeric(single_sim$ols_gamma1)
  assert_valid_numeric(single_sim$tsls_gamma1)
  assert_valid_numeric(single_sim$first_stage_F)
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

  assert_valid_dataframe(main_results, expected_rows = 3)
  assert_list_structure(main_results, c(
    "ols_gamma1", "tsls_gamma1", "first_stage_F"
  ))
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
    n_reps_by_n = 2, sample_sizes = c(n_medium, n_medium * 2)
  )
  seeds_tiny <- generate_all_seeds(config_tiny)

  suppressMessages({
    sample_results <- run_sample_size_analysis(config_tiny, seeds_tiny)
  })

  assert_valid_dataframe(sample_results)
  assert_list_structure(sample_results, "sample_size")
  expect_true(all(sample_results$sample_size %in% c(n_medium, n_medium * 2)))
  expect_true(nrow(sample_results) >= 4) # At least 2 reps * 2 sample sizes
})

test_that("run_sensitivity_analysis works", {
  skip_if_not_fast_test()
  config_sens <- create_default_config(
    n_reps_by_delta = 2, delta_het_values = c(test_gamma1_true, delta_het_moderate)
  )
  seeds_sens <- generate_all_seeds(config_sens)

  suppressMessages({
    sens_results <- run_sensitivity_analysis(config_sens, seeds_sens)
  })

  assert_valid_dataframe(sens_results)
  assert_list_structure(sens_results, "delta_het")
  expect_true(all(sens_results$delta_het %in% c(test_gamma1_true, delta_het_moderate)))
  expect_true(nrow(sens_results) >= 4) # At least 2 reps * 2 delta values
})

test_that("run_bootstrap_demonstration works", {
  skip_if_not_fast_test()
  config_boot <- create_default_config(
    bootstrap_demo_size = 2, bootstrap_reps = n_boot_tiny
  )
  seeds_boot <- generate_all_seeds(config_boot)

  suppressMessages({
    bootstrap_demo <- run_bootstrap_demonstration(config_boot, seeds_boot)
  })

  assert_valid_dataframe(bootstrap_demo)
  assert_list_structure(bootstrap_demo, "sim_id")
  expect_true(nrow(bootstrap_demo) >= 2) # At least 2 demo_size
  assert_list_structure(bootstrap_demo, c(
    "bound_lower_tau_set", "bound_upper_tau_set"
  ))
})

test_that("run_lewbel_demo works", {
  skip_if_not_fast_test()
  suppressMessages({
    demo_results <- run_lewbel_demo(num_simulations = 2, verbose = FALSE)
  })

  assert_list_structure(demo_results, c(
    "config", "results_main", "analysis"
  ))
  assert_valid_dataframe(demo_results$results_main)
  assert_list_structure(demo_results$analysis, names(demo_results$analysis))
})
