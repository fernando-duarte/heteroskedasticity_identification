% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rigobon.R
\name{run_rigobon_analysis}
\alias{run_rigobon_analysis}
\title{Rigobon (2003) Regime-Based Heteroskedasticity Identification}
\usage{
run_rigobon_analysis(
  n_obs = .hetid_const("N_DEFAULT"),
  params = NULL,
  data = NULL,
  regime_var = "regime",
  endog_var = "Y2",
  exog_vars = "Xk",
  verbose = TRUE,
  return_all = FALSE
)
}
\arguments{
\item{n_obs}{Integer. Sample size (default: 1000).}

\item{params}{List. Parameters for data generation. If NULL, uses default
parameters suitable for demonstration.}

\item{data}{Data.frame. Optional. Pre-existing data with regime indicators.
If provided, skips data generation.}

\item{regime_var}{Character. Name of regime variable in data (default: "regime").}

\item{endog_var}{Character. Name of endogenous variable (default: "Y2").}

\item{exog_vars}{Character vector. Names of exogenous variables (default: "Xk").}

\item{verbose}{Logical. Whether to print progress messages (default: TRUE).}

\item{return_all}{Logical. Whether to return all intermediate results
(default: FALSE).}
}
\value{
A list containing:
\itemize{
\item estimates: Data frame comparing OLS and Rigobon 2SLS estimates
\item diagnostics: Heteroskedasticity test results and first-stage F-stats
\item data: The data used (generated or provided)
\item models: Fitted model objects (if return_all = TRUE)
}
}
\description{
This file implements the Rigobon (2003) procedure for using discrete
regime indicators to generate instruments for identification in triangular
systems with endogenous regressors. The method exploits heteroskedasticity
across different regimes (e.g., policy periods, market conditions).
}
\details{
The Rigobon method is a special case of Lewbel's (2012) approach where
the heteroskedasticity drivers are discrete regime indicators rather than
continuous functions of exogenous variables.
Run Complete Rigobon Analysis

This is the main function that performs a complete Rigobon (2003)
heteroskedasticity-based identification analysis. It combines data generation,
estimation, and diagnostic testing in a single workflow.
}
\examples{
\dontrun{
# Quick analysis with default parameters
results <- run_rigobon_analysis()

# Custom parameters for stronger identification
params <- list(
  beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
  beta2_0 = 1.0, beta2_1 = -1.0,
  alpha1 = -0.5, alpha2 = 1.0,
  regime_probs = c(0.3, 0.7),
  sigma2_regimes = c(1.0, 3.0) # Large difference in variances
)
results <- run_rigobon_analysis(n_obs = 2000, params = params)

# Using existing data
# Assume you have data with a regime indicator
results <- run_rigobon_analysis(data = my_data, regime_var = "period")
}

}
\references{
Rigobon, R. (2003). "Identification Through Heteroskedasticity."
The Review of Economics and Statistics, 85(4), 777-792.

Rigobon, R. (2003). Identification through heteroskedasticity.
Review of Economics and Statistics, 85(4), 777-792.
\doi{10.1162/003465303772815727}
}
\seealso{
\code{\link{generate_rigobon_data}}, \code{\link{run_rigobon_estimation}},
\code{\link{validate_rigobon_assumptions}}
}
