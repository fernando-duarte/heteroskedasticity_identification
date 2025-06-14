# Global variables used in NSE (non-standard evaluation) contexts
# This avoids R CMD check NOTEs about undefined global variables

utils::globalVariables(c(
  # Variables from analyze_bootstrap_results
  "bound_se_lower",
  "sim_id",
  "bound_lower_tau_set",
  "bound_upper_tau_set",
  "bound_se_upper",

  # Variables from analyze_sample_size_results
  "sample_size",
  "tsls_gamma1",
  "first_stage_F",

  # Variables from analyze_sensitivity_results
  "delta_het",

  # Variables from plot_bootstrap_ci
  "sim_id_ordered",

  # Variables from plot_estimator_distributions
  "ols_gamma1",
  "Estimator",
  "Estimate"
))
