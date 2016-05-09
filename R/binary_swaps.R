#' Check if a vector has two unique elements.
#'
#' \code{check.false.binary} returns TRUE if there are two unique elements in
#' its argument, and returns FALSE if there are not two unique elements in its
#' argument.
#'
#' This function is not designed to be used outside of this package. It is
#' sufficiently simple as to be practically redundant to the user.
#'
#' @param bin.chain A one dimension vector.
#'
#' @examples
#' check.false.binary(c(1,0,0,1,0,0,0,1))
#' check.false.binary(c("A","B","B","B","A","B","A","A"))
#' check.false.binary(c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE,TRUE,
#' FALSE))
#' @export
check.false.binary <- function(bin.chain) {
    options <- unique(bin.chain)
    if (length(options) == 2) {
        return(TRUE)
    } else if (length(options) == 1){
      print("WARNING: Your binary chain only has one value. This will probably
            cause calculating test statistics impossible, and you should
            reconsider your model.")
      return(TRUE)
    } else {
      return(FALSE)
    }
}
#' Check if a two dimensional matrix has two unique elements.
#'
#' \code{check.false.binary.multiple} returns TRUE is there are two unique
#' elements in its argument, and returns FALSE if there are not two unique
#' elements in its argument.
#'
#' This function checks every row and column element to see if they all share
#' the same two values.
#'
#' @param bin.chains A two dimensional matrix
#'
#' @examples
#' check.false.binary.multiple(matrix(data = c(1,0,1,0,1,0,0,1,1), ncol = 3))
#' check.false.binary.multiple(matrix(data = c("A","B","A","B","A","B","B","A",
#' "A"), ncol = 3))
#' check.false.binary.multiple(matrix(data = c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,
#' FALSE,FALSE,TRUE,TRUE,FALSE), ncol = 3))
#' @export
check.false.binary.multiple <- function(bin.chains) {
    unique <- c()
    for (i in 1:nrow(bin.chains)){
        unique <- union(unique, unique(bin.chains[i,]))
        if (length(unique) != 2 & length(unique) != 1) {
            return(FALSE)
        }
    }
   return(TRUE)
}

#' Check if a one dimensional vector has only integer elements 0 and 1.
#'
#' \code{check.true.binary} returns TRUE if all of the elements in the argument
#' are either integers of value 0 or 1. \code{check.true.binary} returns FALSE
#' if not all of the elements in the argument are integers of value 0 or 1.
#'
#' This function checks every element of its argument to see if they contain
#' either the integer values 0 or 1.
#'
#' @param bin.chain A one dimensional vector.
#'
#' @examples
#' check.true.binary(c(1,0,0,1,0,0,0,1))
#' check.true.binary(c("A","B","B","B","A","B","A","A"))
#' check.true.binary(c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE,TRUE,
#' FALSE))
#' @export
check.true.binary <- function(bin.chain) {
    length <- length(bin.chain)
    for (i in 1:length) {
        if (bin.chain[i] != 0 & bin.chain[i] != 1) {
            return(FALSE)
        }
    }
    return(TRUE)
}

#' Check if a two dimensional matrix has only integer elements 0 and 1.
#'
#' \code{check.true.binary.multiple} returns TRUE if all of the elements in its
#' argument are integers 0 or 1, and FALSE if not all of the values in its
#' argument are integers 0 or 1.
#'
#' This function checks every row and column element of its argument to see if
#' they contain either the integer values 0 or 1.
#'
#' @param bin.chains A two dimensional matrix.
#'
#' @examples
#' check.true.binary.multiple(matrix(data = c(1,0,1,0,1,0,0,1,1), ncol = 3))
#' check.true.binary.multiple(matrix(data = c("A","B","A","B","A","B","B","A",
#' "A"), ncol = 3))
#' check.true.binary.multiple(matrix(data = c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,
#' FALSE,FALSE,TRUE,TRUE,FALSE), ncol = 3))
#' @export
check.true.binary.multiple <- function(bin.chains) {
    for (i in 1:nrow(bin.chains)) {
        for (j in 1:ncol(bin.chains)) {
            if (bin.chains[i, j] != 0 & bin.chains[i, j] != 1) {
                return(FALSE)
            }
        }
    }
    return(TRUE)
}

#' Take a one dimensional vector with two unique values and change those values
#' to integers 0 and 1.
#'
#' \code{alter.to.true.binary} takes a one dimensional vector that contains two
#' unique values and switches all of the values with either integer values 0 or
#' 1, so that the resulting vector has a one-to-one correspondence with the
#' original.
#'
#' For this function to work properly, its argument should be checked first
#' using \code{\link{check.false.binary}}, and only used in
#' \code{alter.to.true.binary} if \code{\link{check.false.binary}} returns TRUE.
#'
#' @param bin.chain A one dimensional vector with two unique elements.
#'
#' @examples
#' alter.to.true.binary(c("A","B","B","B","A","B","A","A"))
#' alter.to.true.binary(c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE,TRUE,
#' FALSE))
#' @export
alter.to.true.binary <- function(bin.chain) {
    uniques <- unique(bin.chain)
    binary <- c(0, 1)

    for (i in 1:2) {
        unique.logic <- bin.chain == uniques[i]
        bin.chain[unique.logic] <- binary[i]
    }
    bin.chain <- as.integer(bin.chain)
    print(bin.chain)
    return(bin.chain)
}

#' Take a two dimensional matrix with two unique values and change those values
#' to integers 0 and 1.
#'
#' \code{alter.to.true.binary.multiple} takes a two dimensional matrix that
#' contains two unique values and switches all of the values with either integer
#' values 0 or 1, so that the resulting matrix has a one-to-one correspondence
#' with the original.
#'
#' For this function to work properly, its argument should be checked first
#' using \code{\link{check.false.binary.multiple}} and only used in
#' \code{alter.to.true.binary} if \code{\link{check.false.binary.multiple}}
#' returns TRUE.
#'
#' @param bin.chains A two dimensional vector with two unique elements.
#'
#' @examples
#' alter.to.true.binary.multiple(matrix(data = c("A","B","A","B","A","B","B",
#' "A","A"), ncol = 3))
#' alter.to.true.binary.multiple(matrix(data = c(TRUE,TRUE,TRUE,FALSE,FALSE,
#' TRUE,FALSE,FALSE,TRUE,TRUE,FALSE), ncol = 3))
#' @export
alter.to.true.binary.multiple <- function(bin.chains) {
    uniques <- c()
    for (i in 1:nrow(bin.chains)) {
        uniques <- union(uniques, unique(bin.chains[i, ]))
    }
    binary <- c(0, 1)
    for (j in 1:nrow(bin.chains)) {
        for (k in 1:ncol(bin.chains)) {
            if (bin.chains[j, k] == uniques[1]) {
                bin.chains[j, k] <- binary[1]
            } else if (bin.chains[j, k] == uniques[2]) {
                bin.chains[j, k] <- binary[2]
            }
        }
    }
    return(bin.chains)
}
