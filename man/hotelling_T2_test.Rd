% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hotelling.R
\name{hotelling_T2_test}
\alias{hotelling_T2_test}
\title{Function for Hotelling T2 test using specified data matrix and null mean matrix}
\usage{
hotelling_T2_test(data, mu_null, alpha = 0.05)
}
\arguments{
\item{data}{A dataframe (nxp) with n observations of p variables}

\item{mu_null}{A px1 matrix for the mean vector under the null hypothesis}

\item{alpha}{The level of significance for the test (default value is 0.05)}
}
\value{
The T2 statistic, the critical value and the result of the test.
}
\description{
Function for Hotelling T2 test using specified data matrix and null mean matrix
}
\examples{
hotelling_T2_test(hotelling_test_data, mu_null, 0.1)
hotelling_T2_test(hotelling_test_data, mu_null)
}
