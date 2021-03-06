% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{multiple_chi_sq_test_stat_array}
\alias{multiple_chi_sq_test_stat_array}
\title{Calculate Pearson's chi square test statistics for many sets of binary
chains of data.}
\usage{
multiple_chi_sq_test_stat_array(bin_chains, n_chain_uniques)
}
\arguments{
\item{bin_chains}{A three dimensional vector containing sets of chains of
binary data.}

\item{n_chain_uniques}{An integer value representing the number of unique
elements in the set of chains \code{bin_chains}.}
}
\description{
\code{multiple_chi_sq_test_stat_array} takes a three dimensional vector
containing multiple sets of binary chains of data, and returns a numeric
vector with entries corresponding to the Pearson's chi square test
statistics of each set of binary chains of data.
}

