% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/klein-vella-semiparametric.R
\name{compare_klein_vella_methods}
\alias{compare_klein_vella_methods}
\title{Compare Klein & Vella methods}
\usage{
compare_klein_vella_methods(
  data,
  true_values = NULL,
  methods = c("ols", "kv_parametric", "kv_semiparametric", "lewbel"),
  verbose = TRUE
)
}
\arguments{
\item{data}{Data to use for comparison}

\item{true_values}{Optional list of true parameter values}

\item{methods}{Which methods to compare}

\item{verbose}{Whether to print progress}
}
\value{
Data frame with comparison results
}
\description{
Compare Klein & Vella methods
}
