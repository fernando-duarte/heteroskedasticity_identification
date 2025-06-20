# Test stopper.R functions

test_that("stopper stops with message when v is TRUE", {
  expect_error(
    hetid:::stopper("Custom error message", v = TRUE),
    "Custom error message"
  )

  # Multiple arguments
  expect_error(
    hetid:::stopper("Error:", " something went wrong", v = TRUE),
    "Error:  something went wrong" # Note: extra space due to paste()
  )
})

test_that("stopper stops without message when v is FALSE", {
  # Should still stop but without custom message
  expect_error(
    hetid:::stopper("Custom error message", v = FALSE),
    "^$" # Empty error message
  )
})

test_that("stopper handles empty messages", {
  expect_error(
    hetid:::stopper("", v = TRUE),
    "^$" # Empty message
  )

  expect_error(
    hetid:::stopper("", v = FALSE),
    "^$"
  )
})

test_that("stopper handles special characters", {
  expect_error(
    hetid:::stopper("Error with\nnewline", v = TRUE),
    "Error with\nnewline"
  )

  expect_error(
    hetid:::stopper("Error with 'quotes'", v = TRUE),
    "Error with 'quotes'"
  )
})

test_that("stopper handles various input types", {
  # NULL input
  expect_error(
    hetid:::stopper(NULL, v = TRUE),
    ""
  )

  # Numeric input
  expect_error(
    hetid:::stopper(123, v = TRUE),
    "123"
  )

  # Multiple types
  expect_error(
    hetid:::stopper("Error code: ", 404, v = TRUE),
    "Error code:  404" # Note: extra space due to paste()
  )
})
