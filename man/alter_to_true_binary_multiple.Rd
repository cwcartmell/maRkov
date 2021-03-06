% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binary_swaps.R
\name{alter_to_true_binary_multiple}
\alias{alter_to_true_binary_multiple}
\title{Take a two dimensional matrix with two unique values and change those values
to integers 0 and 1.}
\usage{
alter_to_true_binary_multiple(bin_chains, success)
}
\arguments{
\item{bin_chains}{A two dimensional vector with two unique elements.}

\item{success}{Denotes the data entry to be counted for run statistics.}
}
\description{
\code{alter_to_true_binary_multiple} takes a two dimensional matrix that
contains two unique values and switches all of the values with either integer
values 0 or 1, so that the resulting matrix has a one-to-one correspondence
with the original.
}
\details{
For this function to work properly, its argument should be checked first
using \code{\link{check_false_binary_multiple}} and only used in
\code{alter_to_true_binary} if \code{\link{check_false_binary_multiple}}
returns TRUE.
}
\examples{
alter_to_true_binary_multiple(matrix(data = c("A","B","A","B","A","B","B",
"A","A"), ncol = 3), success = NULL)
alter_to_true_binary_multiple(matrix(data = c(TRUE,TRUE,TRUE,FALSE,FALSE,
TRUE,FALSE,FALSE,TRUE,TRUE,FALSE), ncol = 3), success = NULL)
}

