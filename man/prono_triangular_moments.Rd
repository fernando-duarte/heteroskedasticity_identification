% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lewbel-gmm.R
\name{prono_triangular_moments}
\alias{prono_triangular_moments}
\title{Define GMM Moment Conditions for Prono Triangular System}
\usage{
prono_triangular_moments(
  theta,
  data,
  y1_var,
  y2_var,
  x_vars,
  garch_order = c(1, 1),
  add_intercept = TRUE
)
}
\arguments{
\item{theta}{Numeric vector. Parameters to estimate: c(beta1, gamma1, beta2).}

\item{data}{Data frame containing the variables.}

\item{y1_var}{Character. Name of the first dependent variable (default: "Y1").}

\item{y2_var}{Character. Name of the second dependent variable/endogenous regressor (default: "Y2").}

\item{x_vars}{Character vector. Names of exogenous variables.}

\item{garch_order}{GARCH(p,q) order for conditional variance estimation.}

\item{add_intercept}{Logical. Whether to add an intercept to the exogenous variables.}
}
\value{
Matrix of moment conditions (n x q).
}
\description{
Creates the moment function for GMM estimation of a triangular system
using Prono's GARCH-based identification.
}
\references{
Prono, T. (2014). The Role of Conditional Heteroskedasticity in Identifying
and Estimating Linear Triangular Systems, with Applications to Asset Pricing
Models That Include a Mismeasured Factor. Journal of Applied Econometrics,
29(5), 800-824. \doi{10.1002/jae.2387}
}
\seealso{
\code{\link{prono_gmm}} for the main GMM estimation function.
\code{\link{run_single_prono_simulation}} for 2SLS estimation.
}
