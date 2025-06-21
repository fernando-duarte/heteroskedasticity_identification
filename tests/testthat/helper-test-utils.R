# Test Helper Functions for hetid Package
# This file contains common utilities used across multiple test files

#' Create standard test parameters for Lewbel data generation
#' @param gamma1 The endogenous parameter (default: -0.8)
#' @param delta_het Heteroskedasticity strength (default: 1.2)
#' @return List of parameters for generate_lewbel_data()
create_test_params <- function(gamma1 = -0.8, delta_het = 1.2) {
  list(
    beta1_0 = 0.5,
    beta1_1 = 1.5,
    gamma1 = gamma1,
    beta2_0 = 1.0,
    beta2_1 = -1.0,
    alpha1 = -0.5,
    alpha2 = 1.0,
    delta_het = delta_het
  )
}

#' Create test data using standard parameters
#' @param n Sample size (default: 100)
#' @param params Parameter list (default: create_test_params())
#' @return Data frame from generate_lewbel_data()
create_test_data <- function(n = 100, params = create_test_params()) {
  generate_lewbel_data(n, params)
}

#' Create standard test configuration
#' @param ... Additional parameters to pass to create_default_config()
#' @return Configuration list
create_test_config <- function(num_simulations = 10,
                                main_sample_size = 100,
                                bootstrap_reps = 20,
                                ...) {
  # Use smaller values for testing
  create_default_config(
    num_simulations = num_simulations,
    main_sample_size = main_sample_size,
    bootstrap_reps = bootstrap_reps,
    ...
  )
}

#' Assert that an object is a valid data frame with expected properties
#' @param df The data frame to check
#' @param expected_rows Expected number of rows
#' @param required_cols Required column names
assert_valid_dataframe <- function(df, expected_rows = NULL, required_cols = NULL) {
  expect_s3_class(df, "data.frame")

  if (!is.null(expected_rows)) {
    expect_equal(nrow(df), expected_rows)
  }

  if (!is.null(required_cols)) {
    expect_true(all(required_cols %in% names(df)))
  }
}

#' Assert that a numeric vector has expected properties
#' @param x The numeric vector to check
#' @param length Expected length
#' @param finite Whether all values should be finite
assert_valid_numeric <- function(x, length = NULL, finite = TRUE) {
  expect_type(x, "double")

  if (!is.null(length)) {
    expect_length(x, length)
  }

  if (finite) {
    expect_true(all(is.finite(x)))
  }
}

#' Suppress GARCH NaN warnings
#' @param expr Expression to evaluate
#' @return Result of expression with NaN warnings suppressed
suppress_garch_nan_warning <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl("NaNs produced", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

#' Test function behavior with mocked package unavailability
#' @param expr Expression to test
#' @param package Package name to mock as unavailable
test_without_package <- function(expr, package = "gmm") {
  with_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == package) FALSE else TRUE
    },
    expr,
    .package = "base"
  )
}

#' Test function with both verbose TRUE and FALSE
#' @param fn Function to test
#' @param ... Arguments to pass to function
#' @param check_output Function to check captured output (optional)
test_verbose_behavior <- function(fn, ..., check_output = NULL) {
  # Test with verbose = FALSE (should be silent)
  result_quiet <- fn(..., verbose = FALSE)
  expect_silent(fn(..., verbose = FALSE))

  # Test with verbose = TRUE (should produce output)
  output <- capture.output(result_verbose <- fn(..., verbose = TRUE))
  expect_true(length(output) > 0)

  # Results should be the same
  expect_equal(result_quiet, result_verbose)

  # Optional output checking
  if (!is.null(check_output)) {
    check_output(output)
  }

  invisible(result_quiet)
}

#' Skip test based on test level and package availability
#' @param level Test level ("cran", "fast", "integration", "comprehensive")
#' @param required_packages Character vector of required packages
skip_if_not_test_level <- function(level, required_packages = NULL) {
  # Skip based on test level
  switch(level,
    "cran" = skip_if_not_cran_test(),
    "fast" = skip_if_not_fast_test(),
    "integration" = skip_if_not_integration_test(),
    "comprehensive" = skip_if_not_comprehensive_test()
  )

  # Skip if required packages not available
  if (!is.null(required_packages)) {
    for (pkg in required_packages) {
      skip_if_not_installed(pkg)
    }
  }
}

#' Create a parameterized test for multiple cases
#' @param test_name Base test name
#' @param cases List of test cases
#' @param test_fn Function that takes a case and runs expectations
run_parameterized_test <- function(test_name, cases, test_fn) {
  for (case_name in names(cases)) {
    test_that(paste0(test_name, " - ", case_name), {
      test_fn(cases[[case_name]])
    })
  }
}

#' Common error messages used in tests
test_error_messages <- list(
  missing_package = "is required but not installed",
  invalid_input = "Must provide either",
  not_found = "not found in data",
  invalid_system = "must be one of"
)

#' Assert that an error message matches expected pattern
#' @param expr Expression expected to error
#' @param pattern Regex pattern or key from test_error_messages
expect_error_pattern <- function(expr, pattern) {
  # If pattern is a key in test_error_messages, use that
  if (pattern %in% names(test_error_messages)) {
    pattern <- test_error_messages[[pattern]]
  }
  expect_error(expr, pattern)
}
