# Helper functions for coverage tests

# Create mock results for analysis tests
create_mock_main_results <- function(n_sim = 5) {
  data.frame(
    sim_id = seq_len(n_sim),
    ols_gamma1 = rnorm(n_sim, 0.5, 0.1),
    tsls_gamma1 = rnorm(n_sim, 0.3, 0.05),
    ols_coverage = sample(c(TRUE, FALSE), n_sim, replace = TRUE),
    tsls_coverage = sample(c(TRUE, FALSE), n_sim, replace = TRUE),
    first_stage_F = runif(n_sim, 5, 20),
    bound_lower_tau_set = rnorm(n_sim, 0.2, 0.02),
    bound_upper_tau_set = rnorm(n_sim, 0.4, 0.02),
    bound_lower_tau0 = rnorm(n_sim, 0.25, 0.02),
    bound_upper_tau0 = rnorm(n_sim, 0.35, 0.02)
  )
}

# Create mock bootstrap results
create_mock_bootstrap_results <- function(n_sim = 3) {
  data.frame(
    sim_id = seq_len(n_sim),
    bound_lower_tau_set = rnorm(n_sim, 0.2, 0.02),
    bound_upper_tau_set = rnorm(n_sim, 0.4, 0.02),
    bound_se_lower = runif(n_sim, 0.01, 0.05),
    bound_se_upper = runif(n_sim, 0.01, 0.05)
  )
}

# Create mock sample size results
create_mock_sample_size_data <- function(sample_sizes = c(500, 1000), n_per_size = 3) {
  n_total <- length(sample_sizes) * n_per_size
  data.frame(
    sample_size = rep(sample_sizes, each = n_per_size),
    tsls_gamma1 = rnorm(n_total, -0.8, 0.05),
    first_stage_F = runif(n_total, 5, 20)
  )
}

# Create mock sensitivity results
create_mock_sensitivity_data <- function(delta_values = c(0.8, 1.2), n_per_delta = 3) {
  n_total <- length(delta_values) * n_per_delta
  data.frame(
    delta_het = rep(delta_values, each = n_per_delta),
    tsls_gamma1 = rnorm(n_total, -0.8, 0.05),
    first_stage_F = runif(n_total, 5, 20)
  )
}

# Test verbose output for analysis functions
test_verbose_output <- function(analysis_function, ..., expected_output) {
  output <- capture.output(
    result <- analysis_function(..., verbose = TRUE)
  )

  # Check that expected output appears
  for (pattern in expected_output) {
    expect_true(
      any(grepl(pattern, output)),
      info = sprintf("Expected output pattern '%s' not found", pattern)
    )
  }

  result
}

# Test print methods
test_print_method <- function(object, expected_patterns) {
  output <- capture.output(print(object))

  for (pattern in expected_patterns) {
    expect_true(
      any(grepl(pattern, output)),
      info = sprintf("Expected pattern '%s' not found in print output", pattern)
    )
  }
}

# Test summary methods
test_summary_method <- function(object, expected_patterns) {
  output <- capture.output(summary(object))

  for (pattern in expected_patterns) {
    expect_true(
      any(grepl(pattern, output)),
      info = sprintf("Expected pattern '%s' not found in summary output", pattern)
    )
  }
}

# Test error conditions systematically
test_error_conditions <- function(test_function, error_cases) {
  for (case in error_cases) {
    expect_error(
      do.call(test_function, case$args),
      case$pattern,
      info = case$description
    )
  }
}

# Test edge cases systematically
test_edge_cases <- function(test_function, edge_cases) {
  for (case in edge_cases) {
    result <- do.call(test_function, case$args)
    case$check(result)
  }
}

# Create consistent test scenarios
create_test_scenario <- function(scenario_type = c("minimal", "standard", "comprehensive")) {
  scenario_type <- match.arg(scenario_type)

  switch(scenario_type,
    minimal = list(
      config = create_test_config(num_simulations = 2, bootstrap_demo_size = 1),
      n_obs = 100,
      n_reps = 2
    ),
    standard = list(
      config = create_test_config(num_simulations = 10, bootstrap_demo_size = 5),
      n_obs = 500,
      n_reps = 10
    ),
    comprehensive = list(
      config = create_test_config(num_simulations = 100, bootstrap_demo_size = 20),
      n_obs = 1000,
      n_reps = 50
    )
  )
}
