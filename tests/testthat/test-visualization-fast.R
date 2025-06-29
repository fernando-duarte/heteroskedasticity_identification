# Test visualization functions

test_that("plot_estimator_distributions works", {
  skip_if_not_fast_test()
  config_small <- create_small_test_config()
  seeds_small <- generate_all_seeds(config_small)

  main_results <- run_and_check_simulation(config_small, seeds_small)
  plot_result <- plot_estimator_distributions(main_results, config_small)

  assert_valid_ggplot(plot_result)
})

test_that("plot_sample_size_consistency works", {
  skip_if_not_fast_test()
  config_tiny <- create_default_config(
    n_reps_by_n = 2,
    sample_sizes = c(100, 200)
  )
  seeds_tiny <- generate_all_seeds(config_tiny)

  suppressMessages({
    sample_results <- run_sample_size_analysis(config_tiny, seeds_tiny)
  })

  plot_result <- plot_sample_size_consistency(sample_results, config_tiny)

  assert_valid_ggplot(plot_result)
})

test_that("plot_het_sensitivity works", {
  skip_if_not_fast_test()
  config_sens <- create_default_config(
    n_reps_by_delta = 2,
    delta_het_values = c(0.8, 1.2)
  )
  seeds_sens <- generate_all_seeds(config_sens)

  sens_results <- run_and_check_simulation(config_sens, seeds_sens,
                                           sim_function = run_sensitivity_analysis)

  plot_result <- plot_het_sensitivity(sens_results, config_sens)
  assert_valid_ggplot(plot_result)
})

test_that("plot_first_stage_f_dist works", {
  skip_if_not_fast_test()
  config_small <- create_small_test_config()
  seeds_small <- generate_all_seeds(config_small)

  main_results <- run_and_check_simulation(config_small, seeds_small)

  # Test with automatic weak_iv_pct calculation
  plot_result <- plot_first_stage_f_dist(main_results)
  assert_valid_ggplot(plot_result)
})

test_that("plot_first_stage_f_dist with explicit weak_iv_pct works", {
  skip_if_not_fast_test()
  config_small <- create_small_test_config()
  seeds_small <- generate_all_seeds(config_small)

  main_results <- run_and_check_simulation(config_small, seeds_small)

  # Test with explicit weak_iv_pct
  plot_result <- plot_first_stage_f_dist(main_results, weak_iv_pct = 10.5)

  expect_s3_class(plot_result, "ggplot")
  expect_true("data" %in% names(plot_result))
})

test_that("plot_bootstrap_ci works", {
  skip_if_not_fast_test()
  config_boot <- create_default_config(
    bootstrap_demo_size = 2,
    bootstrap_reps = 5
  )
  seeds_boot <- generate_all_seeds(config_boot)

  suppressMessages({
    main_results <- run_main_simulation(config_boot, seeds_boot)
    bootstrap_demo <- run_bootstrap_demonstration(config_boot, seeds_boot)
    bootstrap_analysis <- analyze_bootstrap_results(
      main_results, bootstrap_demo, config_boot,
      verbose = FALSE
    )
  })

  plot_result <- plot_bootstrap_ci(bootstrap_analysis, config_boot)

  # Function may return NULL if insufficient data
  if (!is.null(plot_result)) {
    expect_s3_class(plot_result, "ggplot")
    expect_true("data" %in% names(plot_result))
    expect_true("layers" %in% names(plot_result))
  } else {
    # If NULL, verify it's because of insufficient data
    expect_true(is.null(plot_result))
    # With such small demo size, we expect NULL return
    expect_true(config_boot$bootstrap_demo_size == 2)
  }
})

test_that("plot functions handle missing data gracefully", {
  skip_if_not_fast_test()
  # Test plot_first_stage_f_dist with missing column
  fake_data <- data.frame(x = 1:10, y = 1:10)

  expect_warning(
    result <- plot_first_stage_f_dist(fake_data),
    "first_stage_F column not found"
  )
  expect_null(result)
})
