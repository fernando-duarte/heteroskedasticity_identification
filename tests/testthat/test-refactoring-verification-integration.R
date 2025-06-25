test_that("constants access works via .hetid_const", {
  skip_if_not_integration_test()
  # Test that constants have expected values through .hetid_const
  expect_equal(.hetid_const("WEAK_INSTRUMENT_F_THRESHOLD"), 10)
  expect_equal(.hetid_const("ALPHA_LEVEL"), 0.05)
  expect_equal(.hetid_const("Z_CRITICAL_95"), 1.96)
  expect_equal(.hetid_const("DISPLAY_DIGITS"), 4)
  expect_equal(.hetid_const("PLOT_BASE_FONT_SIZE"), 14)
})

test_that("options system works correctly", {
  skip_if_not_integration_test()
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

test_that("nested constants access works via .hetid_const", {
  skip_if_not_integration_test()
  # Test nested constant access
  expect_equal(.hetid_const("df_adjust$ASYMPTOTIC"), "asymptotic")
  expect_equal(.hetid_const("df_adjust$FINITE"), "finite")
  expect_equal(.hetid_const("columns$STD_ERROR"), "Std. Error")
  expect_equal(.hetid_const("plot$labels$OLS_BIASED"), "OLS (Biased)")
  expect_equal(.hetid_const("plot$labels$TSLS_LEWBEL"), "2SLS (Lewbel)")
})

test_that("NSE variables work with .data pronoun", {
  skip_if_not_integration_test()
  # Test that .data is properly imported and works
  test_data <- data.frame(x = 1:5, y = 6:10)

  # This should work without errors
  result <- dplyr::filter(test_data, .data$x > 2)
  expect_equal(nrow(result), 3)

  # Test grouping with .data
  grouped_result <- dplyr::group_by(test_data, .data$x)
  expect_s3_class(grouped_result, "grouped_df")
})
