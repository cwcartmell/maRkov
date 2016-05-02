#' Print instructions for examining objects of class "multiple.binary.test".
#'
#' Instruct user to use \code{summary} and \code{print} to examine objects of
#' \link{class} \code{multiple.binary.test}.
#'
#' @param x An object of \code{\link{class}}
#' \code{multiple.binary.test}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.multiple.binary.test <- function(x, ...) {
    cat("\ncall:\n")
    print(x$call)
    cat("\n---\nUse summary() and plot() for more information.")
}

#' Produce a summary of objects of class "multiple.binary.test".
#'
#' Prints test statistics and p-values for objects of \link{class}
#' \code{multiple.binary.test}.
#'
#' @param object An object of \code{\link{class}}
#' \code{multiple.binary.test}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
summary.multiple.binary.test <- function(object, ...) {
  cat("\ncall:\n")
  print(object$call)
  cat("\nTest statistics\n")
  cat("                 Min          1Q      Median          3Q         Max   test stat  Pr(>test stat)\n")
  cat("LRT\t", format(quantile(object$test.stats.lrt)[1], trim = FALSE, justify = "right", width = 10), format(quantile(object$test.stats.lrt)[2],
                                                                                                               trim = FALSE, justify = "right", width = 10), format(quantile(object$test.stats.lrt)[3], trim = FALSE, justify = "right",
                                                                                                                                                                    width = 10), format(quantile(object$test.stats.lrt)[4], trim = FALSE, justify = "right", width = 10), format(quantile(object$test.stats.lrt)[5],
                                                                                                                                                                                                                                                                                 trim = FALSE, justify = "right", width = 10), format(object$test.stats.lrt[1], trim = FALSE, justify = "right", width = 10),
      format(object$p.value.lrt, trim = FALSE, justify = "right", width = 10), sep = "  ")
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

  cat("ChiSq\t", format(quantile(object$test.stats.chisq), trim = FALSE, justify = "right", width = 10)[1], format(quantile(object$test.stats.chisq)[2],
                                                                                                                   trim = FALSE, justify = "right", width = 10), format(quantile(object$test.stats.chisq)[3], trim = FALSE, justify = "right",
                                                                                                                                                                        width = 10), format(quantile(object$test.stats.chisq)[4], trim = FALSE, justify = "right", width = 10), format(quantile(object$test.stats.chisq)[5],
                                                                                                                                                                                                                                                                                       trim = FALSE, justify = "right", width = 10), format(object$test.stats.chisq[1], trim = FALSE, justify = "right", width = 10),
      format(object$p.value.chisq, trim = FALSE, justify = "right", width = 10), sep = "  ")
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

  cat("Run =", object$run, format(quantile(object$test.stats.run)[1], trim = FALSE, justify = "right", width = 10),
      format(quantile(object$test.stats.run)[2], trim = FALSE, justify = "right", width = 10), format(quantile(object$test.stats.run)[3],
                                                                                                      trim = FALSE, justify = "right", width = 10), format(quantile(object$test.stats.run)[4], trim = FALSE, justify = "right",
                                                                                                                                                           width = 10), format(quantile(object$test.stats.run)[5], trim = FALSE, justify = "right", width = 10), format(object$test.stats.run[1],
                                                                                                                                                                                                                                                                        trim = FALSE, justify = "right", width = 10), format(object$p.value.run, trim = FALSE, justify = "right", width = 10),
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
}

#' Produce plots from objects of class "multiple.binary.test".
#'
#' Produces some interesting plots of data and test statistics of objects of
#' \link{class} \code{multiple.binary.test}.
#'
#' @param x An object of \code{\link{class}}
#' \code{multiple.binary.test}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
plot.multiple.binary.test <- function(x, ...) {
  par(ask = TRUE,  mar = c(5, 1, 1, 1))
  y <- x$data[[1]]
  reverse <- nrow(y):1
  y <- y[reverse, ]
  image(c(1:ncol(y)), c(1:nrow(y)), t(y), axes = FALSE, ylab = " ", xlab = "Visual representation of the actual Markov Chain.", col = c("aquamarine3",
                                                                                                                                        "chocolate1"))

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
