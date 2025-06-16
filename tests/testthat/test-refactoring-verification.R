test_that("constants environment is locked and immutable", {
  # Test that constants environment exists and is locked
  expect_true(exists("constants_env"))
  expect_true(environmentIsLocked(constants_env))

  # Test that constants cannot be modified
  expect_error(
    {constants_env$WEAK_INSTRUMENT_F_THRESHOLD <- 999},
    "cannot change value of locked binding"
  )

  # Test that constants have expected values
  expect_equal(constants_env$WEAK_INSTRUMENT_F_THRESHOLD, 10)
  expect_equal(constants_env$ALPHA_LEVEL, 0.05)
  expect_equal(constants_env$Z_CRITICAL_95, 1.96)
})

test_that("options system works correctly", {
  # Test default values from constants
  expect_equal(hetid_opt("display_digits"), 4)
  expect_equal(hetid_opt("alpha_level"), 0.05)

  # Test user override capability
  old_option <- getOption("hetid.display_digits")
  options(hetid.display_digits = 6)
  expect_equal(hetid_opt("display_digits"), 6)

  # Restore original option
  if (is.null(old_option)) {
    options(hetid.display_digits = NULL)
  } else {
    options(hetid.display_digits = old_option)
  }
})

test_that("string constants function works", {
  strings <- .hetid_strings()
  expect_equal(strings$df_adjust$ASYMPTOTIC, "asymptotic")
  expect_equal(strings$df_adjust$FINITE, "finite")
  expect_equal(strings$columns$STD_ERROR, "Std. Error")
  expect_equal(strings$plot_labels$OLS_BIASED, "OLS (Biased)")
  expect_equal(strings$plot_labels$TSLS_LEWBEL, "2SLS (Lewbel)")
})

test_that("NSE variables work with .data pronoun", {
  # Test that .data is properly imported and works
  test_data <- data.frame(x = 1:5, y = 6:10)
  
  # This should work without errors
  result <- dplyr::filter(test_data, .data$x > 2)
  expect_equal(nrow(result), 3)
  
  # Test grouping with .data
  grouped_result <- dplyr::group_by(test_data, .data$x)
  expect_s3_class(grouped_result, "grouped_df")
})

test_that("constants are accessible in package functions", {
  # Test that constants can be accessed in the package context
  expect_true(exists("constants_env"))
  expect_equal(constants_env$DISPLAY_DIGITS, 4)
  expect_equal(constants_env$WEAK_INSTRUMENT_F_THRESHOLD, 10)
  expect_equal(constants_env$PLOT_BASE_FONT_SIZE, 14)
})
