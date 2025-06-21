# Test utility functions (stopper and messager)
# This consolidates tests from stopper-cran.R and messager-cran.R

# Stopper function tests -----------------------------------------------------

test_that("stopper stops with correct messages", {
  skip_if_not_test_level("cran")

  # Basic functionality
  expect_error(
    hetid:::stopper("Custom error message", v = TRUE),
    "Custom error message"
  )

  # Multiple arguments
  expect_error(
    hetid:::stopper("Error:", " something went wrong", v = TRUE),
    "Error:  something went wrong"
  )

  # Silent mode (v = FALSE)
  expect_error(
    hetid:::stopper("Custom error message", v = FALSE),
    "^$"
  )
})

test_that("stopper handles edge cases", {
  skip_if_not_test_level("cran")

  # Empty messages
  expect_error(hetid:::stopper("", v = TRUE), "^$")
  expect_error(hetid:::stopper("", v = FALSE), "^$")

  # NULL input
  expect_error(hetid:::stopper(NULL, v = TRUE), "")

  # Numeric input
  expect_error(hetid:::stopper(123, v = TRUE), "123")

  # Mixed types
  expect_error(
    hetid:::stopper("Error code: ", 404, v = TRUE),
    "Error code:  404"
  )
})

test_that("stopper handles special characters", {
  skip_if_not_test_level("cran")

  expect_error(
    hetid:::stopper("Error with\nnewline", v = TRUE),
    "Error with\nnewline"
  )

  expect_error(
    hetid:::stopper("Error with 'quotes'", v = TRUE),
    "Error with 'quotes'"
  )
})

# Messager function tests ----------------------------------------------------

test_that("messager prints messages based on verbose setting", {
  skip_if_not_test_level("cran")

  # Basic functionality
  expect_message(
    hetid:::messager("Test message", v = TRUE),
    "Test message"
  )

  expect_silent(
    hetid:::messager("Test message", v = FALSE)
  )

  # Multiple arguments
  expect_message(
    hetid:::messager("Part 1", " Part 2", v = TRUE),
    "Part 1  Part 2"
  )
})

test_that("messager handles parallel mode", {
  skip_if_not_test_level("cran")

  # In parallel mode, output goes to system echo
  # We can't capture it easily, but ensure no errors
  expect_silent(
    hetid:::messager("Parallel message", v = TRUE, parallel = TRUE)
  )

  expect_silent(
    hetid:::messager("Parallel message", v = FALSE, parallel = TRUE)
  )
})

test_that("messager respects VERBOSE environment variable", {
  skip_if_not_test_level("cran")

  # Save current value
  old_verbose <- Sys.getenv("VERBOSE", unset = NA)

  # Test with VERBOSE=FALSE
  Sys.setenv(VERBOSE = "FALSE")
  expect_silent(hetid:::messager("Test message"))

  # Test with VERBOSE=TRUE
  Sys.setenv(VERBOSE = "TRUE")
  expect_message(hetid:::messager("Test message"), "Test message")

  # Restore original value
  if (is.na(old_verbose)) {
    Sys.unsetenv("VERBOSE")
  } else {
    Sys.setenv(VERBOSE = old_verbose)
  }
})

test_that("messager handles special inputs", {
  skip_if_not_test_level("cran")

  # Empty string
  output <- capture.output(
    hetid:::messager("", v = TRUE),
    type = "message"
  )
  expect_equal(output, "")

  # NULL input
  output <- capture.output(
    hetid:::messager(NULL, v = TRUE),
    type = "message"
  )
  expect_equal(output, "")

  # List input (gets converted by paste())
  expect_message(
    hetid:::messager(list(a = 1), v = TRUE),
    "1"
  )
})

test_that("messager handles special characters", {
  skip_if_not_test_level("cran")

  expect_message(
    hetid:::messager("Message with\nnewline", v = TRUE),
    "Message with\nnewline"
  )

  expect_message(
    hetid:::messager("Message with 'quotes'", v = TRUE),
    "Message with 'quotes'"
  )
})
