% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{lewbel_sim}
\alias{lewbel_sim}
\title{Simulated Lewbel Test Data}
\format{
A data frame with 1000 rows and 5 variables:
\describe{
\item{id}{Observation identifier}
\item{y}{Dependent variable (Y1 in the triangular system)}
\item{P}{Endogenous regressor (Y2 in the triangular system)}
\item{X1}{First exogenous regressor}
\item{X2}{Second exogenous regressor}
}
}
\source{
Simulated data using generate_lewbel_data() function
}
\usage{
lewbel_sim
}
\description{
A simulated dataset for testing Lewbel (2012) identification methods.
This dataset contains a triangular system with endogeneity and
heteroskedasticity suitable for testing the hetid package functions.
}
\details{
The data was generated using the following triangular system:
\deqn{y = 0.5 + 1.5 \cdot X1 + 3.0 \cdot X2 - 0.8 \cdot P + \epsilon_1}
\deqn{P = 1.0 - 1.0 \cdot X1 + 0.7 \cdot X2 + \epsilon_2}

The error structure follows a single-factor model with heteroskedasticity:
\deqn{\epsilon_1 = -0.5 \cdot U + V_1}
\deqn{\epsilon_2 = 1.0 \cdot U + V_2}

where \eqn{V_2 \sim N(0, \exp(1.2 \cdot Z))} with \eqn{Z = X2^2 - E[X2^2]}.
}
\references{
Lewbel, A. (2012). Using heteroscedasticity to identify and estimate
mismeasured and endogenous regressor models. Journal of Business & Economic
Statistics, 30(1), 67-80. \doi{10.1080/07350015.2012.643126}
}
\keyword{datasets}
