# Tests for bootstrap analysis functionality - analyze_bootstrap_results()
# Covers bootstrap standard error calculations, data filtering, and edge cases
# Dependencies: testthat, hetid package functions

test_that("analyze_bootstrap_results handles various data combinations", {
  config <- create_default_config(
    bootstrap_demo_size = 3,
    bootstrap_subset_size = 2,
    bootstrap_reps = 5
  )
  seeds <- generate_all_seeds(config)

  suppressMessages({
    main_results <- run_main_simulation(config, seeds, verbose = FALSE)
    bootstrap_demo <- run_bootstrap_demonstration(
      config, seeds,
      verbose = FALSE
    )
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
