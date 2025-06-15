# Comprehensive tests for bootstrap analysis functions

test_that("analyze_bootstrap_results handles various data combinations", {
  config <- create_default_config(
    bootstrap_demo_size = 3,
    bootstrap_subset_size = 2,
    bootstrap_reps = 5
  )
  seeds <- generate_all_seeds(config)

  suppressMessages({
    main_results <- run_main_simulation(config, seeds, verbose = FALSE)
    bootstrap_demo <- run_bootstrap_demonstration(config, seeds, verbose = FALSE)
  })

  # Test with verbose output
  output <- capture.output({
    bootstrap_analysis <- analyze_bootstrap_results(
      main_results, bootstrap_demo, config,
      verbose = TRUE
    )
  })

  expect_s3_class(bootstrap_analysis, "data.frame")
  expect_true(length(output) > 0)
  expect_true(any(grepl("Bootstrap Standard Errors", output)))

  # Test with verbose = FALSE
  bootstrap_analysis_quiet <- analyze_bootstrap_results(
    main_results, bootstrap_demo, config,
    verbose = FALSE
  )
  expect_s3_class(bootstrap_analysis_quiet, "data.frame")
})

test_that("analyze_bootstrap_results handles edge cases in data filtering", {
  config <- create_default_config(bootstrap_subset_size = 10)

  # Create main results with some NA values in bound_se_lower
  main_results <- data.frame(
    sim_id = 1:5,
    bound_lower_tau_set = c(-0.5, -0.6, -0.7, -0.8, -0.9),
    bound_upper_tau_set = c(0.5, 0.6, 0.7, 0.8, 0.9),
    bound_se_lower = c(0.1, NA, 0.2, NA, 0.3),
    bound_se_upper = c(0.1, NA, 0.2, NA, 0.3)
  )

  # Create bootstrap demo
  bootstrap_demo <- data.frame(
    sim_id = 6:7,
    bound_lower_tau_set = c(-1.0, -1.1),
    bound_upper_tau_set = c(1.0, 1.1),
    bound_se_lower = c(0.4, 0.5),
    bound_se_upper = c(0.4, 0.5)
  )

  result <- analyze_bootstrap_results(
    main_results, bootstrap_demo, config,
    verbose = FALSE
  )

  # Should filter out NA values and combine with bootstrap_demo
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 2) # At least the bootstrap_demo rows
})

test_that("print_simulation_summary handles all parameter combinations", {
  # Test with NULL analysis and config
  expect_silent(print_simulation_summary(NULL, NULL, verbose = FALSE))

  # Test with verbose = TRUE and NULL inputs
  output1 <- capture.output(
    print_simulation_summary(NULL, NULL, verbose = TRUE)
  )
  expect_true(any(grepl("SIMULATION COMPLETE", output1)))
  expect_true(any(grepl("Key findings:", output1)))

  # Test with analysis object containing weak_iv_pct
  analysis_with_weak <- list(weak_iv_pct = 25.7)
  output2 <- capture.output(
    print_simulation_summary(analysis_with_weak, NULL, verbose = TRUE)
  )
  expect_true(any(grepl("25.7%", output2)))
  expect_true(any(grepl("Weak instrument rate", output2)))

  # Test with analysis object without weak_iv_pct
  analysis_without_weak <- list(other_field = "value")
  output3 <- capture.output(
    print_simulation_summary(analysis_without_weak, NULL, verbose = TRUE)
  )
  expect_true(any(grepl("SIMULATION COMPLETE", output3)))
  expect_false(any(grepl("Weak instrument rate", output3)))
})

test_that("print_simulation_summary handles edge cases in verbose parameter", {
  # Test with list as verbose parameter - should produce output (defaults to TRUE)
  output1 <- capture.output(
    print_simulation_summary(verbose = list(a = 1, b = 2))
  )
  expect_true(length(output1) > 0)

  # Test with vector as verbose parameter - should produce output (defaults to TRUE)
  output2 <- capture.output(
    print_simulation_summary(verbose = c(TRUE, FALSE, TRUE))
  )
  expect_true(length(output2) > 0)

  # Test with numeric verbose parameter
  expect_silent(
    print_simulation_summary(verbose = 1)
  )

  # Test with character verbose parameter
  expect_silent(
    print_simulation_summary(verbose = "true")
  )
})

test_that("all analysis functions handle missing columns gracefully", {
  config <- create_default_config()

  # Test with minimal data frame missing some expected columns
  minimal_results <- data.frame(
    sim_id = 1:2,
    ols_gamma1 = c(0.5, 0.6),
    tsls_gamma1 = c(0.3, 0.4)
  )

  # Should error due to missing required columns
  # When columns are missing, mean() will warn about non-numeric arguments
  # This is expected behavior when required columns are absent
  expect_error(
    suppressWarnings(
      analyze_main_results(minimal_results, config, verbose = FALSE)
    )
  )

  # Test sample size analysis with missing columns
  minimal_sample <- data.frame(
    sim_id = 1:2,
    sample_size = c(100, 200)
  )

  expect_error(
    analyze_sample_size_results(minimal_sample, config, verbose = FALSE)
  )

  # Test sensitivity analysis with missing columns
  minimal_sens <- data.frame(
    sim_id = 1:2,
    delta_het = c(0.8, 1.2)
  )

  expect_error(
    analyze_sensitivity_results(minimal_sens, config, verbose = FALSE)
  )
})

test_that("analyze_bootstrap_results handles empty filtered results", {
  config <- create_default_config(bootstrap_subset_size = 5)

  # Create main results where all bound_se_lower are NA
  all_na_main <- data.frame(
    sim_id = 1:3,
    bound_lower_tau_set = c(-0.5, -0.6, -0.7),
    bound_upper_tau_set = c(0.5, 0.6, 0.7),
    bound_se_lower = rep(NA_real_, 3),
    bound_se_upper = rep(NA_real_, 3)
  )

  # Empty bootstrap demo
  empty_bootstrap <- data.frame(
    sim_id = integer(0),
    bound_lower_tau_set = numeric(0),
    bound_upper_tau_set = numeric(0),
    bound_se_lower = numeric(0),
    bound_se_upper = numeric(0)
  )

  result <- analyze_bootstrap_results(
    all_na_main, empty_bootstrap, config,
    verbose = FALSE
  )

  # Should return empty data frame or handle gracefully
  expect_s3_class(result, "data.frame")
})
