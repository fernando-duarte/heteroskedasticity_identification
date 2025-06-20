% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lewbel-gmm.R
\name{rigobon_simultaneous_moments}
\alias{rigobon_simultaneous_moments}
\title{Define GMM Moment Conditions for Rigobon Simultaneous System}
\usage{
rigobon_simultaneous_moments(
  theta,
  data,
  y1_var,
  y2_var,
  x_vars,
  regime_var,
  add_intercept = TRUE
)
}
\arguments{
\item{theta}{Numeric vector. Parameters to estimate.}

\item{data}{Data frame containing the variables.}

\item{y1_var}{Character. Name of the first dependent variable.}

\item{y2_var}{Character. Name of the second dependent variable.}

\item{x_vars}{Character vector. Names of exogenous variables.}

\item{regime_var}{Character. Name of the regime indicator variable.}

\item{add_intercept}{Logical. Whether to add an intercept.}
}
\value{
Matrix of moment conditions.
}
\description{
Creates the moment function for GMM estimation of a simultaneous equations
system using Rigobon's regime-based identification.
}
\details{
WARNING: Simultaneous systems with regime-based identification are
numerically challenging. They require many regimes (4+) with substantial
variance differences for reliable estimation. The system may be singular
or near-singular with insufficient heteroskedasticity variation.
}
\references{
Rigobon, R. (2003). Identification through heteroskedasticity.
Review of Economics and Statistics, 85(4), 777-792.
}
