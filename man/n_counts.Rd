% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{n_counts}
\alias{n_counts}
\title{Second order transition counts for a single binary chain.}
\usage{
n_counts(bin_chain, n_chain_uniques)
}
\arguments{
\item{bin_chain}{An integer vector representing a chain of data.}

\item{n_chain_uniques}{The number or unique values in the chain
\code{bin_chains}, represented as an integer value.}
}
\description{
\code{n_counts} counts the number of second order transitions in a binary
chain of data, then returns a three dimensional vector whose indices
represent the type of transition, and whose values represent the number of
times that each transition occurs in the chain.
}

