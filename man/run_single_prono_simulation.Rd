% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prono.R
\name{run_single_prono_simulation}
\alias{run_single_prono_simulation}
\title{Run single Prono simulation with GARCH-based instruments}
\usage{
run_single_prono_simulation(config, return_details = FALSE)
}
\arguments{
\item{config}{Configuration list with simulation parameters}

\item{return_details}{If TRUE, return detailed results}
}
\value{
List with estimation results including gamma1_true, gamma1_ols, gamma1_iv,
standard errors, biases, F-statistic, and optionally full model objects
}
\description{
Run single Prono simulation with GARCH-based instruments
}
\references{
Prono, T. (2014). The Role of Conditional Heteroskedasticity in Identifying
and Estimating Linear Triangular Systems, with Applications to Asset Pricing
Models That Include a Mismeasured Factor. Journal of Applied Econometrics,
29(5), 800-824. \doi{10.1002/jae.2387}
}
\seealso{
\code{\link{generate_prono_data}} for data generation
\code{\link{run_prono_monte_carlo}} for Monte Carlo analysis
\code{\link{prono_gmm}} for GMM estimation
}
