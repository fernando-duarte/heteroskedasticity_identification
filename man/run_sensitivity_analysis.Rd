% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation.R
\name{run_sensitivity_analysis}
\alias{run_sensitivity_analysis}
\title{Run Sensitivity Analysis}
\usage{
run_sensitivity_analysis(config, seeds, verbose = TRUE)
}
\arguments{
\item{config}{List. Configuration object from create_default_config().}

\item{seeds}{List. Seed object from generate_all_seeds().}

\item{verbose}{Logical. Whether to print progress messages (default: TRUE).}
}
\value{
A data.frame containing results for different heteroscedasticity
parameters.
}
\description{
Analyzes sensitivity of results to heteroscedasticity strength.
}
\examples{
\dontrun{
config <- create_default_config()
seeds <- generate_all_seeds(config)
sensitivity_results <- run_sensitivity_analysis(config, seeds)
}

}
