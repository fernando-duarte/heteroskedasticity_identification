test_that("package can be loaded", {
  # Verify the package namespace is available
  expect_true("hetid" %in% loadedNamespaces())
  # Verify key functions are exported
  expect_true(exists("generate_lewbel_data"))
  expect_true(exists("run_single_lewbel_simulation"))
})

# Test utility functions
test_that("create_default_config works", {
  config <- create_default_config()

  expect_type(config, "list")
  expect_true(all(c(
    "num_simulations", "main_sample_size", "gamma1", "beta1_0", "beta1_1",
    "beta2_0", "beta2_1", "alpha1", "alpha2", "delta_het"
  ) %in% names(config)))
  expect_equal(config$gamma1, -0.8)
  expect_equal(config$main_sample_size, 1000) # Default from constants
})

test_that("create_default_config with custom parameters works", {
  config <- create_default_config(num_simulations = 50, main_sample_size = 300)

  expect_equal(config$num_simulations, 50)
  expect_equal(config$main_sample_size, 300)
  expect_equal(config$gamma1, -0.8) # Should keep default
})

test_that("generate_seed_matrix works", {
  seeds <- generate_seed_matrix(123, 3, 5)

  expect_type(seeds, "integer")
  expect_equal(dim(seeds), c(3, 5))
  expect_true(all(seeds > 0))

  # Test reproducibility
  seeds2 <- generate_seed_matrix(123, 3, 5)
  expect_equal(seeds, seeds2)
})

test_that("generate_all_seeds works", {
  config <- create_default_config(num_simulations = 5)
  all_seeds <- generate_all_seeds(config)

  expect_type(all_seeds, "list")
  # Correct expected names based on actual function implementation
  expect_true(all(
    c("main", "by_n", "by_delta", "bootstrap_demo") %in% names(all_seeds)
  ))
  expect_equal(length(all_seeds$main), 5)
})
