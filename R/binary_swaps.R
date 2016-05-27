#' Check if a vector has two unique elements.
#'
#' \code{check_false_binary} returns TRUE if there are two unique elements in
#' its argument, and returns FALSE if there are not two unique elements in its
#' argument.
#'
#' This function is not designed to be used outside of this package. It is
#' sufficiently simple as to be practically redundant to the user.
#'
#' @param bin_chain A one dimensional vector.
#'
#' @examples
#' check_false_binary(c(1,0,0,1,0,0,0,1))
#' check_false_binary(c("A","B","B","B","A","B","A","A"))
#' check_false_binary(c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE,TRUE,
#' FALSE))
#' @export
check_false_binary <- function(bin_chain) {
    options <- unique(bin_chain)
    if (length(options) == 2) {
        return(TRUE)
    } else if (length(options) == 1){
      stop("Your binary chain only has one value. This will cause calculating test statistics impossible, and you should reconsider your model.",
           call. = FALSE)
    } else {
      return(FALSE)
    }
}
#' Check if a two dimensional matrix has two unique elements.
#'
#' \code{check_false_binary_multiple} returns TRUE is there are two unique
#' elements in its argument, and returns FALSE if there are not two unique
#' elements in its argument.
#'
#' This function checks every row and column element to see if they all share
#' the same two values.
#'
#' @param bin_chains A two dimensional matrix
#'
#' @examples
#' check_false_binary_multiple(matrix(data = c(1,0,1,0,1,0,0,1,1), ncol = 3))
#' check_false_binary_multiple(matrix(data = c("A","B","A","B","A","B","B","A",
#' "A"), ncol = 3))
#' check_false_binary_multiple(matrix(data = c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,
#' FALSE,FALSE,TRUE,TRUE,FALSE), ncol = 3))
#' @export
check_false_binary_multiple <- function(bin_chains) {
    unique <- c()
    for (i in 1:nrow(bin_chains)){
        unique <- union(unique, unique(bin_chains[i,]))
        if (length(unique) != 2 & length(unique) != 1) {
            return(FALSE)
        }
    }
   return(TRUE)
}

#' Check if a one dimensional vector has only integer elements 0 and 1.
#'
#' \code{check_true_binary} returns TRUE if all of the elements in the argument
#' are either integers of value 0 or 1. \code{check_true_binary} returns FALSE
#' if not all of the elements in the argument are integers of value 0 or 1.
#'
#' This function checks every element of its argument to see if they contain
#' either the integer values 0 or 1.
#'
#' @param bin_chain A one dimensional vector.
#'
#' @examples
#' check_true_binary(c(1,0,0,1,0,0,0,1))
#' check_true_binary(c("A","B","B","B","A","B","A","A"))
#' check_true_binary(c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE,TRUE,
#' FALSE))
#' @export
check_true_binary <- function(bin_chain) {
    length <- length(bin_chain)
    for (i in 1:length) {
        if (bin_chain[i] != 0 & bin_chain[i] != 1) {
            return(FALSE)
        }
    }
    return(TRUE)
}

#' Check if a two dimensional matrix has only integer elements 0 and 1.
#'
#' \code{check_true_binary_multiple} returns TRUE if all of the elements in its
#' argument are integers 0 or 1, and FALSE if not all of the values in its
#' argument are integers 0 or 1.
#'
#' This function checks every row and column element of its argument to see if
#' they contain either the integer values 0 or 1.
#'
#' @param bin_chains A two dimensional matrix.
#'
#' @examples
#' check_true_binary_multiple(matrix(data = c(1,0,1,0,1,0,0,1,1), ncol = 3))
#' check_true_binary_multiple(matrix(data = c("A","B","A","B","A","B","B","A",
#' "A"), ncol = 3))
#' check_true_binary_multiple(matrix(data = c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,
#' FALSE,FALSE,TRUE,TRUE,FALSE), ncol = 3))
#' @export
check_true_binary_multiple <- function(bin_chains) {
    for (i in 1:nrow(bin_chains)) {
        for (j in 1:ncol(bin_chains)) {
            if (bin_chains[i, j] != 0 & bin_chains[i, j] != 1) {
                return(FALSE)
            }
        }
    }
    return(TRUE)
}

#' Take a one dimensional vector with two unique values and change those values
#' to integers 0 and 1.
#'
#' \code{alter_to_true_binary} takes a one dimensional vector that contains two
#' unique values and switches all of the values with either integer values 0 or
#' 1, so that the resulting vector has a one-to-one correspondence with the
#' original.
#'
#' For this function to work properly, its argument should be checked first
#' using \code{\link{check_false_binary}}, and only used in
#' \code{alter_to_true_binary} if \code{\link{check_false_binary}} returns TRUE.
#'
#' @param bin_chain A one dimensional vector with two unique elements.
#' @param success Denotes the data entry to be counted for run statistics.
#'
#' @examples
#' alter_to_true_binary(c("A","B","B","B","A","B","A","A"))
#' alter_to_true_binary(c(TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,TRUE,TRUE,
#' FALSE))
#' @export
alter_to_true_binary <- function(bin_chain, success) {
  uniques <- unique(bin_chain)

  if (is.null(success) == TRUE) {
    bin_chain[bin_chain == uniques[1]] <- 0
    bin_chain[bin_chain == uniques[2]] <- 1
  } else {
    if (is.element(success, uniques) == FALSE) {
      stop("The success argument did not match the entries of the given binary chain.",
           call. = FALSE)
    }
    bin_chain[bin_chain != success] <- 0
    bin_chain[bin_chain == success] <- 1
  }
  return(as.integer(bin_chain))
}

#' Take a two dimensional matrix with two unique values and change those values
#' to integers 0 and 1.
#'
#' \code{alter_to_true_binary_multiple} takes a two dimensional matrix that
#' contains two unique values and switches all of the values with either integer
#' values 0 or 1, so that the resulting matrix has a one-to-one correspondence
#' with the original.
#'
#' For this function to work properly, its argument should be checked first
#' using \code{\link{check_false_binary_multiple}} and only used in
#' \code{alter_to_true_binary} if \code{\link{check_false_binary_multiple}}
#' returns TRUE.
#'
#' @param bin_chains A two dimensional vector with two unique elements.
#' @param success Denotes the data entry to be counted for run statistics.
#'
#' @examples
#' alter_to_true_binary_multiple(matrix(data = c("A","B","A","B","A","B","B",
#' "A","A"), ncol = 3))
#' alter_to_true_binary_multiple(matrix(data = c(TRUE,TRUE,TRUE,FALSE,FALSE,
#' TRUE,FALSE,FALSE,TRUE,TRUE,FALSE), ncol = 3))
#' @export
alter_to_true_binary_multiple <- function(bin_chains, success) {
    uniques <- c()
    for (i in 1:nrow(bin_chains)) {
        uniques <- union(uniques, unique(bin_chains[i, ]))
    }
    if (is.null(success) == TRUE) {
      bin_chains[bin_chains == uniques[1]] <- 0
      bin_chains[bin_chains == uniques[2]] <- 1
    } else {
      if (is.element(success, uniques) == FALSE) {
        stop("The success argument did not match the entries of the given, binary chains.",
             call. = FALSE)
      }
      bin_chains[bin_chains != success] <- 0
      bin_chains[bin_chains == success] <- 1
    }
    return(as.integer(bin_chains))
}
