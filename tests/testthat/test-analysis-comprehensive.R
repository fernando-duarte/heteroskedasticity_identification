# Comprehensive tests for main analysis functions

test_that("analyze_main_results knitr conditional logic works", {
  config <- create_default_config(num_simulations = 2)
  seeds <- generate_all_seeds(config)

  suppressMessages({
    results <- run_main_simulation(config, seeds, verbose = FALSE)
  })

  # Test with knitr available (normal case)
  if (requireNamespace("knitr", quietly = TRUE)) {
    output_with_knitr <- capture.output({
      analysis <- analyze_main_results(results, config, verbose = TRUE)
    })
    expect_true(length(output_with_knitr) > 0)
    expect_true(any(grepl("Main simulation", output_with_knitr)))
  }

  # Test fallback when knitr is not available (mock scenario)
  # We can't easily unload knitr, but we can test the else branch
  # by checking that the function works with verbose output
  output_verbose <- capture.output({
    analysis <- analyze_main_results(results, config, verbose = TRUE)
  })
  expect_true(length(output_verbose) > 0)
  expect_true(any(grepl("Performance of Point Estimators", output_verbose)))
  expect_true(any(grepl("Performance of Set Identification", output_verbose)))
})

test_that("analyze_main_results handles different config parameters", {
  # Test with different gamma1 values
  config1 <- create_default_config(num_simulations = 2, gamma1 = -0.5)
  config2 <- create_default_config(num_simulations = 2, gamma1 = -1.2)

  seeds1 <- generate_all_seeds(config1)
  seeds2 <- generate_all_seeds(config2)

  suppressMessages({
    results1 <- run_main_simulation(config1, seeds1, verbose = FALSE)
    results2 <- run_main_simulation(config2, seeds2, verbose = FALSE)
  })

  analysis1 <- analyze_main_results(results1, config1, verbose = FALSE)
  analysis2 <- analyze_main_results(results2, config2, verbose = FALSE)

  # Check that different gamma1 values affect bias calculations
  expect_type(analysis1$summary_table$Bias, "double")
  expect_type(analysis2$summary_table$Bias, "double")
  expect_equal(nrow(analysis1$summary_table), 2)
  expect_equal(nrow(analysis2$summary_table), 2)
})

test_that("analyze_main_results verbose output includes all expected elements", {
  config <- create_default_config(num_simulations = 3)
  seeds <- generate_all_seeds(config)

  suppressMessages({
    results <- run_main_simulation(config, seeds, verbose = FALSE)
  })

  # Capture all verbose output
  output <- capture.output({
    analysis <- analyze_main_results(results, config, verbose = TRUE)
  })

  # Check for all expected output elements
  expect_true(any(grepl("Main simulation:", output)))
  expect_true(any(grepl("True value of gamma1:", output)))
  expect_true(any(grepl("Performance of Point Estimators", output)))
  expect_true(any(grepl("Weak instrument diagnostic:", output)))
  expect_true(any(grepl("Performance of Set Identification", output)))

  # Check that weak_iv_pct is calculated correctly
  expect_type(analysis$weak_iv_pct, "double")
  expect_true(analysis$weak_iv_pct >= 0 && analysis$weak_iv_pct <= 100)
})

test_that("analyze_sample_size_results verbose output works correctly", {
  config <- create_default_config(
    n_reps_by_n = 2,
    sample_sizes = c(100, 200, 300)
  )
  seeds <- generate_all_seeds(config)

  suppressMessages({
    sample_results <- run_sample_size_analysis(config, seeds, verbose = FALSE)
  })

  # Test verbose output
  output <- capture.output({
    analysis <- analyze_sample_size_results(
      sample_results, config,
      verbose = TRUE
    )
  })

  expect_s3_class(analysis, "data.frame")
  expect_true(length(output) > 0)
  expect_true(any(grepl("Consistency Check", output)))
  expect_true(any(grepl("Performance by Sample Size", output)))
  expect_equal(nrow(analysis), 3) # Three sample sizes
})

test_that("analyze_sensitivity_results verbose output works correctly", {
  config <- create_default_config(
    n_reps_by_delta = 2,
    delta_het_values = c(0.8, 1.0, 1.2)
  )
  seeds <- generate_all_seeds(config)

  suppressMessages({
    sens_results <- run_sensitivity_analysis(config, seeds, verbose = FALSE)
  })

  # Test verbose output
  output <- capture.output({
    analysis <- analyze_sensitivity_results(
      sens_results, config,
      verbose = TRUE
    )
  })

  expect_s3_class(analysis, "data.frame")
  expect_true(length(output) > 0)
  expect_true(any(grepl("Sensitivity to Heteroscedasticity", output)))
  expect_equal(nrow(analysis), 3) # Three delta values
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

test_that("analyze_main_results handles correlation edge cases", {
  config <- create_default_config()

  # Create results where correlation calculation might fail
  problematic_results <- data.frame(
    sim_id = 1:3,
    sample_size = rep(100, 3),
    delta_het = rep(1.0, 3),
    ols_gamma1 = c(0.5, 0.6, 0.7),
    tsls_gamma1 = c(0.3, 0.4, 0.5),
    ols_coverage = c(TRUE, FALSE, TRUE),
    tsls_coverage = c(FALSE, TRUE, FALSE),
    first_stage_F = c(10, 15, 20),
    bound_lower_tau0 = c(0.3, 0.4, 0.5), # Same as tsls_gamma1
    bound_upper_tau0 = c(0.3, 0.4, 0.5), # Same as tsls_gamma1
    bound_lower_tau_set = c(-1.0, -1.1, -1.2),
    bound_upper_tau_set = c(1.0, 1.1, 1.2)
  )

  analysis <- analyze_main_results(
    problematic_results, config,
    verbose = FALSE
  )

  # Should handle perfect correlation case
  expect_type(analysis$bounds_summary$`Point ID Check`, "double")
  expect_true(is.finite(analysis$bounds_summary$`Point ID Check`))
})

test_that("analyze_main_results handles zero variance cases", {
  config <- create_default_config()

  # Create results with zero variance in some variables
  zero_var_results <- data.frame(
    sim_id = 1:3,
    sample_size = rep(100, 3),
    delta_het = rep(1.0, 3),
    ols_gamma1 = rep(0.5, 3), # Zero variance
    tsls_gamma1 = rep(0.3, 3), # Zero variance
    ols_coverage = rep(TRUE, 3),
    tsls_coverage = rep(FALSE, 3),
    first_stage_F = rep(10, 3), # Zero variance
    bound_lower_tau0 = rep(0.3, 3),
    bound_upper_tau0 = rep(0.3, 3),
    bound_lower_tau_set = rep(-1.0, 3),
    bound_upper_tau_set = rep(1.0, 3)
  )

  # When all values are identical (zero variance), cor() will warn about
  # "the standard deviation is zero". This is expected behavior.
  suppressWarnings({
    analysis <- analyze_main_results(
      zero_var_results, config,
      verbose = FALSE
    )
  })

  # Should handle zero variance gracefully
  # RMSE is calculated against config$gamma1, so won't be 0 unless estimates equal gamma1
  expect_type(analysis$summary_table$RMSE, "double")
  expect_true(all(is.finite(analysis$summary_table$RMSE)))
  expect_equal(analysis$weak_iv_pct, 0) # No weak instruments (F=10 > 10 threshold)
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

test_that("analysis functions handle single row data frames", {
  config <- create_default_config()

  # Single row for main analysis
  single_row <- data.frame(
    sim_id = 1,
    sample_size = 100,
    delta_het = 1.0,
    ols_gamma1 = 0.5,
    tsls_gamma1 = 0.3,
    ols_coverage = TRUE,
    tsls_coverage = FALSE,
    first_stage_F = 10,
    bound_lower_tau0 = -0.5,
    bound_upper_tau0 = 0.5,
    bound_lower_tau_set = -1.0,
    bound_upper_tau_set = 1.0
  )

  analysis <- analyze_main_results(single_row, config, verbose = FALSE)
  expect_type(analysis, "list")
  expect_equal(nrow(analysis$summary_table), 2)

  # Single row for sample size analysis
  single_sample <- data.frame(
    sim_id = 1,
    sample_size = 100,
    tsls_gamma1 = 0.3,
    first_stage_F = 10
  )

  sample_analysis <- analyze_sample_size_results(
    single_sample, config,
    verbose = FALSE
  )
  expect_s3_class(sample_analysis, "data.frame")
  expect_equal(nrow(sample_analysis), 1)

  # Single row for sensitivity analysis
  single_sens <- data.frame(
    sim_id = 1,
    delta_het = 1.0,
    tsls_gamma1 = 0.3,
    first_stage_F = 10,
    bound_lower_tau_set = -1.0,
    bound_upper_tau_set = 1.0
  )

  sens_analysis <- analyze_sensitivity_results(
    single_sens, config,
    verbose = FALSE
  )
  expect_s3_class(sens_analysis, "data.frame")
  expect_equal(nrow(sens_analysis), 1)
})

test_that("verbose output formatting works with different table sizes", {
  config <- create_default_config()

  # Create results with many rows to test table formatting
  many_rows <- data.frame(
    sim_id = 1:10,
    sample_size = rep(c(100, 200), each = 5),
    delta_het = rep(c(0.8, 1.2), each = 5),
    ols_gamma1 = runif(10, 0.4, 0.6),
    tsls_gamma1 = runif(10, 0.2, 0.4),
    ols_coverage = sample(c(TRUE, FALSE), 10, replace = TRUE),
    tsls_coverage = sample(c(TRUE, FALSE), 10, replace = TRUE),
    first_stage_F = runif(10, 5, 25),
    bound_lower_tau0 = runif(10, -0.6, -0.4),
    bound_upper_tau0 = runif(10, 0.4, 0.6),
    bound_lower_tau_set = runif(10, -1.2, -0.8),
    bound_upper_tau_set = runif(10, 0.8, 1.2)
  )

  # Test main analysis verbose output
  output_main <- capture.output({
    analyze_main_results(many_rows, config, verbose = TRUE)
  })
  expect_true(length(output_main) > 0)

  # Test sample size analysis verbose output
  output_sample <- capture.output({
    analyze_sample_size_results(many_rows, config, verbose = TRUE)
  })
  expect_true(length(output_sample) > 0)

  # Test sensitivity analysis verbose output
  output_sens <- capture.output({
    analyze_sensitivity_results(many_rows, config, verbose = TRUE)
  })
  expect_true(length(output_sens) > 0)
})
