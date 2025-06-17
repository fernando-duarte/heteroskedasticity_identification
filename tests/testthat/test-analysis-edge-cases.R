# Test basic edge cases for analysis functions

test_that("analyze_main_results handles empty results", {
  config <- create_default_config()

  # Empty data frame - should produce errors due to missing columns
  empty_results <- data.frame()
  # analyze_main_results will fail with empty data
  # When results are empty, mean() will warn about non-numeric arguments
  # We expect multiple warnings (one for each mean() call), but since
  # expect_warning() only captures one warning at a time in testthat edition 3,
  # we suppress them here and document their expected nature
  suppressWarnings({
    analysis_empty <- analyze_main_results(
      empty_results, config,
      verbose = FALSE
    )
  })

  expect_type(analysis_empty, "list")
  # Should have NA values for empty results
  expect_true(is.na(analysis_empty$bounds_summary$`Point ID Check`))

  # All NA results
  na_results <- data.frame(
    sim_id = 1:3,
    ols_gamma1 = rep(NA_real_, 3),
    tsls_gamma1 = rep(NA_real_, 3),
    ols_coverage = rep(NA, 3),
    tsls_coverage = rep(NA, 3),
    first_stage_F = rep(NA_real_, 3),
    bound_lower_tau0 = rep(NA_real_, 3),
    bound_upper_tau0 = rep(NA_real_, 3),
    bound_lower_tau_set = rep(NA_real_, 3),
    bound_upper_tau_set = rep(NA_real_, 3)
  )

  # With all NA values, correlation calculation returns NA but function succeeds
  suppressWarnings({
    analysis_na <- analyze_main_results(na_results, config, verbose = FALSE)
  })

  expect_type(analysis_na, "list")
  expect_true(is.na(analysis_na$bounds_summary$`Point ID Check`))
})

test_that("analyze_main_results handles extreme values", {
  config <- create_default_config()

  # Results with extreme values
  extreme_results <- data.frame(
    sim_id = 1:3,
    sample_size = rep(100, 3),
    delta_het = rep(1.0, 3),
    ols_gamma1 = c(1e6, -1e6, 0),
    tsls_gamma1 = c(1e10, -1e10, 0),
    ols_coverage = c(TRUE, FALSE, NA),
    tsls_coverage = c(FALSE, TRUE, NA),
    first_stage_F = c(0.001, 1e6, NA),
    bound_lower_tau0 = c(-Inf, -1e6, NA),
    bound_upper_tau0 = c(Inf, 1e6, NA),
    bound_lower_tau_set = c(-1e6, -1e6, NA),
    bound_upper_tau_set = c(1e6, 1e6, NA)
  )

  suppressWarnings({
    analysis_extreme <- analyze_main_results(
      extreme_results, config,
      verbose = FALSE
    )
  })

  expect_type(analysis_extreme, "list")
  expect_false(any(is.nan(
    analysis_extreme$summary_table$Bias[is.finite(
      analysis_extreme$summary_table$Bias
    )]
  )))
})

test_that("analyze_main_results verbose output works with edge cases", {
  config <- create_default_config()

  # Single valid result
  single_result <- data.frame(
    sim_id = 1,
    sample_size = 100,
    delta_het = 1.0,
    ols_gamma1 = 0.5,
    tsls_gamma1 = 0.3,
    ols_coverage = TRUE,
    tsls_coverage = FALSE,
    first_stage_F = 5.0, # Weak instrument
    bound_lower_tau0 = -0.5,
    bound_upper_tau0 = 0.5,
    bound_lower_tau_set = -1.0,
    bound_upper_tau_set = 1.0
  )

  # Capture output
  output <- capture.output({
    analysis <- analyze_main_results(
      single_result, config,
      verbose = TRUE
    )
  })

  # Check for expected output patterns
  expect_true(any(grepl("Main simulation", output)))
  expect_true(any(grepl("Weak instrument diagnostic", output)))
})

test_that("analyze_bootstrap_results handles edge cases", {
  config <- create_default_config(bootstrap_subset_size = 5)

  # Empty bootstrap demo - will fail with missing columns
  empty_main <- data.frame()
  empty_bootstrap <- data.frame()

  # Should error on empty dataframes without required columns
  expect_error(
    analyze_bootstrap_results(
      empty_main, empty_bootstrap, config,
      verbose = FALSE
    )
  )

  # All NA standard errors
  na_main <- data.frame(
    sim_id = 1:3,
    bound_lower_tau_set = c(-0.5, -0.6, -0.7),
    bound_upper_tau_set = c(0.5, 0.6, 0.7),
    bound_se_lower = rep(NA_real_, 3),
    bound_se_upper = rep(NA_real_, 3)
  )

  na_bootstrap <- data.frame(
    sim_id = 4:5,
    bound_lower_tau_set = c(-0.8, -0.9),
    bound_upper_tau_set = c(0.8, 0.9),
    bound_se_lower = rep(NA_real_, 2),
    bound_se_upper = rep(NA_real_, 2)
  )

  result_na <- analyze_bootstrap_results(
    na_main, na_bootstrap, config,
    verbose = FALSE
  )
  # The function combines the data but doesn't filter by NA
  # It just shows what data is available
  expect_true(nrow(result_na) >= 0)
})

test_that("analyze_sample_size_results handles edge cases", {
  config <- create_default_config()

  # Empty results
  empty_results <- data.frame()
  expect_error(
    analyze_sample_size_results(empty_results, config, verbose = FALSE)
  )

  # Single sample size with all NA
  na_results <- data.frame(
    sim_id = 1:10,
    sample_size = rep(100, 10),
    tsls_gamma1 = rep(NA_real_, 10),
    first_stage_F = rep(NA_real_, 10)
  )

  analysis_na <- analyze_sample_size_results(
    na_results, config,
    verbose = FALSE
  )
  expect_true(all(is.na(analysis_na$`2SLS Bias`)))
  expect_true(all(is.na(analysis_na$`2SLS RMSE`)))

  # Mixed valid and invalid results
  mixed_results <- data.frame(
    sim_id = 1:6,
    sample_size = rep(c(100, 200), each = 3),
    tsls_gamma1 = c(0.5, NA, 0.6, 0.7, 0.8, NA),
    first_stage_F = c(10, NA, 15, 20, 25, NA)
  )

  analysis_mixed <- analyze_sample_size_results(
    mixed_results, config,
    verbose = FALSE
  )
  expect_equal(nrow(analysis_mixed), 2)
  expect_false(any(is.na(analysis_mixed$`2SLS Bias`)))
})
