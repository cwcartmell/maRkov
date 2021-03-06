% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binary_swaps.R
\name{check_false_binary_multiple}
\alias{check_false_binary_multiple}
\title{Check if a two dimensional matrix has two unique elements.}
\usage{
check_false_binary_multiple(bin_chains)
}
\arguments{
\item{bin_chains}{A two dimensional matrix}
}
\description{
\code{check_false_binary_multiple} returns TRUE is there are two unique
elements in its argument, and returns FALSE if there are not two unique
elements in its argument.
}
\details{
This function checks every row and column element to see if they all share
the same two values.
}
\examples{
check_false_binary_multiple(matrix(data = c(1,0,1,0,1,0,0,1,1), ncol = 3))
check_false_binary_multiple(matrix(data = c("A","B","A","B","A","B","B","A",
"A"), ncol = 3))
check_false_binary_multiple(matrix(data = c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,
FALSE,FALSE,TRUE,TRUE,FALSE), ncol = 3))
}

