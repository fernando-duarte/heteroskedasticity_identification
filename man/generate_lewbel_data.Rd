% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-generation.R
\name{generate_lewbel_data}
\alias{generate_lewbel_data}
\title{Generate Data for Lewbel (2012) Triangular Model}
\usage{
generate_lewbel_data(n_obs, params, n_x = 1)
}
\arguments{
\item{n_obs}{Integer. Sample size.}

\item{params}{List. Parameters for the data generating process containing:
\itemize{
\item beta1_0, beta1_1: Parameters for first equation (beta1_1 can be
a vector for multiple X)
\item beta2_0, beta2_1: Parameters for second equation (beta2_1 can be
a vector for multiple X)
\item gamma1: Endogenous parameter (key parameter of interest)
\item alpha1, alpha2: Factor loadings for common factor U
\item delta_het: Heteroscedasticity strength parameter
}}

\item{n_x}{Integer. Number of exogenous X variables to generate (default: 1).
If n_x > 1, beta1_1 and beta2_1 should be vectors of length n_x.}
}
\value{
A data.frame with columns Y1, Y2, epsilon1, epsilon2, and:
\itemize{
\item If n_x = 1: Xk, Z
\item If n_x > 1: X1, X2, ..., Z1, Z2, ... (one Z per X)
}
}
\description{
Creates a dataset based on the triangular model with single-factor error
structure that satisfies Lewbel's identifying assumptions. The data
generating process uses
a common factor structure for the errors to ensure the covariance restriction
Cov(Z, \eqn{\epsilon_1 \epsilon_2}) = 0 is satisfied.
}
\details{
The triangular model is:
\deqn{Y_1 = \beta_{1,0} + \beta_{1,1}X + \gamma_1 Y_2 + \epsilon_1}
\deqn{Y_2 = \beta_{2,0} + \beta_{2,1}X + \epsilon_2}

The error structure follows a single-factor model:
\deqn{\epsilon_1 = \alpha_1 U + V_1}
\deqn{\epsilon_2 = \alpha_2 U + V_2}

where U, V_1 are independent standard normal, and V_2 ~ N(0, exp(\eqn{\delta}
Z))
with Z = \eqn{X^2 - E[X^2]} being the heteroscedasticity driver.
}
\examples{
\dontrun{
# Single X variable (backward compatible)
params <- list(
  beta1_0 = 0.5, beta1_1 = 1.5, gamma1 = -0.8,
  beta2_0 = 1.0, beta2_1 = -1.0,
  alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
)
data <- generate_lewbel_data(1000, params)

# Multiple X variables
params_multi <- list(
  beta1_0 = 0.5, beta1_1 = c(1.5, 3.0), gamma1 = -0.8,
  beta2_0 = 1.0, beta2_1 = c(-1.0, 0.7),
  alpha1 = -0.5, alpha2 = 1.0, delta_het = 1.2
)
data_multi <- generate_lewbel_data(1000, params_multi, n_x = 2)
}

}
