# Test messager.R functions

test_that("messager prints messages when verbose is TRUE", {
  # Capture output
  expect_message(
    hetid:::messager("Test message", v = TRUE),
    "Test message"
  )

  # Multiple arguments
  expect_message(
    hetid:::messager("Part 1", " Part 2", v = TRUE),
    "Part 1  Part 2" # Note: extra space due to paste()
  )
})

test_that("messager suppresses messages when verbose is FALSE", {
  # Should not print when v = FALSE
  expect_silent(
    hetid:::messager("Test message", v = FALSE)
  )
})

test_that("messager handles parallel mode", {
  # Test parallel mode (output goes to system echo)
  # We can't easily capture system() output in tests, but we can ensure
  # it doesn't error
  expect_silent(
    hetid:::messager("Parallel message", v = TRUE, parallel = TRUE)
  )

  # Should be silent when v = FALSE even in parallel mode
  expect_silent(
    hetid:::messager("Parallel message", v = FALSE, parallel = TRUE)
  )
})

test_that("messager handles environment variable VERBOSE", {
  # Save current value
  old_verbose <- Sys.getenv("VERBOSE", unset = NA)

  # Test with VERBOSE=FALSE
  Sys.setenv(VERBOSE = "FALSE")
  expect_silent(
    hetid:::messager("Test message") # Should use env var default
  )

  # Test with VERBOSE=TRUE
  Sys.setenv(VERBOSE = "TRUE")
  expect_message(
    hetid:::messager("Test message"), # Should use env var default
    "Test message"
  )

  # Restore original value
  if (is.na(old_verbose)) {
    Sys.unsetenv("VERBOSE")
  } else {
    Sys.setenv(VERBOSE = old_verbose)
  }
})

test_that("messager handles empty messages", {
  # Empty string produces an empty message
  output <- capture.output(
    hetid:::messager("", v = TRUE),
    type = "message"
  )
  expect_equal(output, "")

  expect_silent(
    hetid:::messager("", v = FALSE)
  )
})

test_that("messager handles special characters", {
  # Test with special characters
  expect_message(
    hetid:::messager("Message with\nnewline", v = TRUE),
    "Message with\nnewline"
  )

  expect_message(
    hetid:::messager("Message with 'quotes'", v = TRUE),
    "Message with 'quotes'"
  )
})

test_that("messager handles errors gracefully", {
  # The function uses try() so errors should be caught
  # Test with various inputs that might cause issues
  # NULL produces an empty message
  output <- capture.output(
    hetid:::messager(NULL, v = TRUE),
    type = "message"
  )
  expect_equal(output, "")

  # list gets converted to string by paste()
  expect_message(
    hetid:::messager(list(a = 1), v = TRUE),
    "1"
  )
})
