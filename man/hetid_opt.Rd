\name{hetid_opt}
\alias{hetid_opt}
\title{Get hetid package options with fallback to constants}
\usage{
hetid_opt(option_name)
}
\arguments{
\item{option_name}{Character. Name of the option to retrieve. Supported
options include:
\itemize{
\item \code{display_digits}: Number of digits for displaying results
\item \code{alpha_level}: Significance level for statistical tests
\item \code{weak_f_threshold}: F-statistic threshold for weak instruments
\item \code{parallel_offset}: Offset for parallel processing workers
\item \code{bootstrap_reps}: Number of bootstrap replications
}}
}
\value{
The option value. Returns the user-set value if available,
otherwise returns the default from the constants environment.
}
\description{
This function retrieves user-configurable options for the hetid package.
If a user hasn't set a specific option, it falls back to the default
value stored in the package's internal constants environment.
}
\examples{
# Get default display digits
hetid_opt("display_digits")

# Set a custom value
options(hetid.display_digits = 6)
hetid_opt("display_digits")

# Reset to default
options(hetid.display_digits = NULL)
hetid_opt("display_digits")

}
