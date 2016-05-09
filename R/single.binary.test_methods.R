#' Print instructions for examining objects of class "single.binary.test".
#'
#' Instruct user to use \code{summary} and \code{print} to examine objects of
#' \link{class} \code{single.binary.test}.
#'
#' @param x An object of \code{\link{class}}
#' \code{single.binary.test}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.single.binary.test <- function(x, ...) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n---\nUse summary() and plot() for more information.")
}

#' Produce a summary of objects of class "single.binary.test".
#'
#' Prints test statistics, p-values, and provides sample data for objects of
#' \link{class} \code{single.binary.test}.
#'
#' @param object An object of \code{\link{class}}
#' \code{single.binary.test}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
summary.single.binary.test <- function(object, ...) {
  cat("\nCall:\n")
  print(object$call)
  cat("\nVarious test statistics:\n")
  cat("                 Min          1Q      Median          3Q         Max   test stat  Pr(>test stat)\n")
  cat("LRT\t", format(quantile(object$test.stats.lrt)[1], trim = FALSE,
                      justify = "right", width = 10),
      format(quantile(object$test.stats.lrt)[2], trim = FALSE,
             justify = "right", width = 10),
      format(quantile(object$test.stats.lrt)[3], trim = FALSE,
             justify = "right", width = 10),
      format(quantile(object$test.stats.lrt)[4], trim = FALSE,
             justify = "right", width = 10),
      format(quantile(object$test.stats.lrt)[5], trim = FALSE,
             justify = "right", width = 10),
      format(object$test.stats.lrt[1], trim = FALSE, justify = "right",
             width = 10),
      format(object$p.value.lrt, trim = FALSE, justify = "right", width = 10),
      sep = "  ")
  if (object$p.value.lrt < 0.001) {
    cat(" ***\n")
  } else if (object$p.value.lrt < 0.01 & object$p.value.lrt >= 0.001) {
    cat(" **\n")
  } else if (object$p.value.lrt < 0.05 & object$p.value.lrt >= 0.01) {
    cat(" *\n")
  } else if (object$p.value.lrt < 0.1 & object$p.value.lrt >= 0.05) {
    cat(" .\n")
  } else if (object$p.value.lrt < 1 & object$p.value.lrt >= 0.1) {
    cat(" \n")
  }

  cat("ChiSq\t",
      format(quantile(object$test.stats.chisq)[1], trim = FALSE,
             justify = "right", width = 10, digits = 6),
      format(quantile(object$test.stats.chisq)[2], trim = FALSE,
             justify = "right", width = 10, digits = 6),
      format(quantile(object$test.stats.chisq)[3], trim = FALSE,
             justify = "right", width = 10, digits = 6),
      format(quantile(object$test.stats.chisq)[4], trim = FALSE,
             justify = "right", width = 10, digits = 6),
      format(quantile(object$test.stats.chisq)[5], trim = FALSE,
             justify = "right", width = 10, digits = 6),
      format(object$test.stats.chisq[1], trim = FALSE, justify = "right",
             width = 10, digits = 6),
      format(object$p.value.chisq, trim = FALSE, justify = "right", width = 10,
             digits = 6),
      sep = "  ")
  if (object$p.value.chisq < 0.001) {
    cat(" ***\n")
  } else if (object$p.value.chisq < 0.01 & object$p.value.chisq >= 0.001) {
    cat(" **\n")
  } else if (object$p.value.chisq < 0.05 & object$p.value.chisq >= 0.01) {
    cat(" *\n")
  } else if (object$p.value.chisq < 0.1 & object$p.value.chisq >= 0.05) {
    cat(" .\n")
  } else if (object$p.value.chisq < 1 & object$p.value.chisq >= 0.1) {
    cat(" \n")
  }

  cat("Run =", object$run, format(quantile(object$test.stats.run)[1],
                                  trim = FALSE, justify = "right", width = 10),
      format(quantile(object$test.stats.run)[2], trim = FALSE,
             justify = "right", width = 10),
      format(quantile(object$test.stats.run)[3], trim = FALSE,
             justify = "right", width = 10),
      format(quantile(object$test.stats.run)[4], trim = FALSE,
             justify = "right", width = 10),
      format(quantile(object$test.stats.run)[5], trim = FALSE,
             justify = "right", width = 10),
      format(object$test.stats.run[1], trim = FALSE, justify = "right",
             width = 10),
      format(object$p.value.run, trim = FALSE, justify = "right", width = 10),
      sep = "  ")
  if (object$p.value.run < 0.001) {
    cat(" ***\n")
  } else if (object$p.value.run < 0.01 & object$p.value.run >= 0.001) {
    cat(" **\n")
  } else if (object$p.value.run < 0.05 & object$p.value.run >= 0.01) {
    cat(" *\n")
  } else if (object$p.value.run < 0.1 & object$p.value.run >= 0.05) {
    cat(" .\n")
  } else if (object$p.value.run < 1 & object$p.value.run >= 0.1) {
    cat(" \n")
  }

  cat("---\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' .05 '.' .1 ' ' 1\n---\n")
  cat("Quick sample of real and generated data:\n")
  cat("       Real data :\t\t", object$data[1, ], "\n")
  cat("Generated data 1 :\t\t", object$data[2, ], "\n")
  cat("Generated data 2 :\t\t", object$data[3, ], "\n")
  cat(".\n.\n.\n")
  cat("Generated data", length(object$data[, 1]) - 1, ":\t\t",
      object$data[length(object$data[, 1]), ], "\n")
}

#' Produce plots from objects of class "single.binary.test".
#'
#' Produces some interesting plots of data and test statistics of objects of
#' \link{class} \code{single.binary.test}.
#'
#' @param x An object of \code{\link{class}}
#' \code{single.binary.test}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
plot.single.binary.test <- function(x, ...) {
  par(ask = TRUE, mar = c(5, 1, 1, 1))
  y <- x$data
  y <- y[1:x$tiles, ]
  reverse <- nrow(y):1
  y <- y[reverse, ]
  image(c(1:ncol(y)), c(1:nrow(y)), t(y), axes = FALSE, ylab = " ", xlab = "Real data above the black line, generated data below it", col = c("aquamarine3",
                                                                                                                                              "chocolate1"))
  abline(h = x$tiles - 0.5, col = "black", lwd = 4)

  par(ask = TRUE, mar = c(5, 4, 4, 2))

  hist(x$test.stats.lrt, breaks = x$bins, xlab = "LRT Test Statistics\nRed line indicates value of test statistic of real data",
       main = NULL)
  abline(v = x$test.stats.lrt[1], col = "red")

  hist(x$test.stats.chisq, breaks = x$bins, xlab = "Pearson's Chi-Square Test Statistics\nRed line indicates value of test statistic of real data",
       main = NULL)
  abline(v = x$test.stats.chisq[1], col = "red")

  hist(x$test.stats.run, breaks = x$bins, xlab = paste("Run Test Statistics For a Run of", x$run,
                                                       "\nRed line indicates value of test statistic of real data"), main = NULL)
  abline(v = x$test.stats.run[1], col = "red")
}
