# Helper functions for Lewbel GMM tests

# Generate test data for Lewbel GMM
create_lewbel_test_data <- function(n = 500, seed = 42, params = NULL) {
  set.seed(seed)
  if (is.null(params)) {
    params <- create_test_params()
  }
  generate_lewbel_data(n, params)
}

# Run standard Lewbel GMM test
run_lewbel_gmm_test <- function(data, system = "triangular",
                                expected_coef_names = NULL,
                                check_gamma1 = TRUE,
                                true_gamma1 = -0.8,
                                tolerance = 0.7) { # Increased tolerance for new DGP
  gmm_result <- lewbel_gmm(data, system = system)

  # Basic checks
  expect_s3_class(gmm_result, "lewbel_gmm")
  expect_s3_class(gmm_result, "gmm")
  expect_equal(attr(gmm_result, "hetid_system"), system)

  # Coefficient names check
  if (!is.null(expected_coef_names)) {
    coef_names <- names(coef(gmm_result))
    for (name in expected_coef_names) {
      expect_true(name %in% coef_names)
    }
  }

  # Gamma1 estimate check
  if (check_gamma1) {
    gamma1_est <- coef(gmm_result)["gamma1"]
    expect_true(abs(gamma1_est - true_gamma1) < tolerance)
  }

  gmm_result
}

# Compare GMM results
compare_gmm_results <- function(result1, result2,
                                coef_names = c("gamma1", "beta1_(Intercept)", "beta1_Xk"),
                                tolerance = 0.2) { # Increased tolerance for new DGP
  for (coef_name in coef_names) {
    coef1 <- coef(result1)[coef_name]
    coef2 <- coef(result2)[coef_name]
    expect_true(abs(coef1 - coef2) < tolerance,
      info = sprintf(
        "Coefficient %s differs: %f vs %f",
        coef_name, coef1, coef2
      )
    )
  }
}

# Test moment conditions
test_moment_conditions <- function(data, theta, system = "triangular",
                                   n_expected_moments = NULL) {
  if (system == "triangular") {
    moments <- lewbel_triangular_moments(
      theta = theta,
      data = data,
      y1_var = "Y1",
      y2_var = "Y2",
      x_vars = "Xk",
      z_vars = NULL,
      add_intercept = TRUE
    )
  } else {
    moments <- lewbel_simultaneous_moments(
      theta = theta,
      data = data,
      y1_var = "Y1",
      y2_var = "Y2",
      x_vars = "Xk",
      z_vars = NULL,
      add_intercept = TRUE
    )
  }

  expect_equal(nrow(moments), nrow(data))
  if (!is.null(n_expected_moments)) {
    expect_equal(ncol(moments), n_expected_moments)
  }

  # Check that moments have reasonable mean
  expect_true(all(abs(colMeans(moments)) < 10))

  moments
}

# Test heteroskedasticity detection
test_het_detection <- function(data, system = "triangular",
                               expect_het = TRUE,
                               alpha = 0.05) {
  gmm_result <- lewbel_gmm(data, system = system)
  het_test <- test_heteroskedasticity(gmm_result)

  expect_s3_class(het_test, "htest")
  expect_true("p.value" %in% names(het_test))

  if (expect_het) {
    expect_true(het_test$p.value < alpha,
      info = sprintf(
        "Expected heteroskedasticity but p-value = %f",
        het_test$p.value
      )
    )
  } else {
    expect_true(het_test$p.value >= alpha,
      info = sprintf(
        "Expected homoskedasticity but p-value = %f",
        het_test$p.value
      )
    )
  }

  het_test
}

# Create comprehensive GMM test suite
run_comprehensive_gmm_tests <- function(n_sim = 10, n_obs = 500,
                                        systems = c("triangular", "simultaneous"),
                                        gmm_types = c("onestep", "twostep", "iterative"),
                                        verbose = FALSE) {
  results <- list()

  for (system in systems) {
    for (gmm_type in gmm_types) {
      if (verbose) {
        message(sprintf("Testing %s system with %s GMM", system, gmm_type))
      }

      estimates <- numeric(n_sim)
      for (i in seq_len(n_sim)) {
        data <- create_lewbel_test_data(n = n_obs, seed = i)
        gmm_result <- lewbel_gmm(data, system = system, gmm_type = gmm_type)
        estimates[i] <- coef(gmm_result)["gamma1"]
      }

      results[[paste(system, gmm_type, sep = "_")]] <- list(
        mean_estimate = mean(estimates),
        sd_estimate = sd(estimates),
        bias = mean(estimates) - (-0.8) # True gamma1 = -0.8 (note: new DGP has bias)
      )
    }
  }

  results
}
