% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{multiple_run_test_stat_array}
\alias{multiple_run_test_stat_array}
\title{Calculate run test statistics for many sets of chains of binary data.}
\usage{
multiple_run_test_stat_array(bin_chains, p)
}
\arguments{
\item{bin_chains}{A three dimensional integer vector containing sets of
chains of binary data.}

\item{p}{An integer representing the length of run to test for.}
}
\description{
\code{multiple_run_test_stat_array} takes a three dimensional integer vector
containing multiple sets of binary chains of data, and returns a numeric
vector with entries corresponding to the run test statistics for runs of
length p for each set of binary chains of data.
}

