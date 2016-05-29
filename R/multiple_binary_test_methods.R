#' Print instructions for examining objects of class "multiple_binary_test".
#'
#' Instruct user to use \code{summary} and \code{print} to examine objects of
#' \link{class} \code{multiple_binary_test}.
#'
#' @param x An object of \code{\link{class}}
#' \code{multiple_binary_test}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.multiple_binary_test <- function(x, ...) {
    cat("\ncall:\n")
    print(x$call)
    cat("\n---\nUse summary() and plot() for more information.")
}

#' Produce a summary of objects of class "multiple_binary_test".
#'
#' Prints test statistics and p-values for objects of \link{class}
#' \code{multiple_binary_test}.
#'
#' @param object An object of \code{\link{class}}
#' \code{multiple_binary_test}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
summary.multiple_binary_test <- function(object, ...) {
  cat("\nCall:\n")
  print(object$call)
  cat("\nTest statistics\n")
  cat("                 Min          1Q      Median          3Q         Max   test stat  Pr(>test stat)\n")
  cat("LRT\t", format(stats::quantile(object$test_stats_lrt)[1], trim = FALSE,
                      justify = "right", width = 10),
      format(stats::quantile(object$test_stats_lrt)[2], trim = FALSE,
             justify = "right", width = 10),
      format(stats::quantile(object$test_stats_lrt)[3], trim = FALSE,
             justify = "right", width = 10),
      format(stats::quantile(object$test_stats_lrt)[4], trim = FALSE,
             justify = "right", width = 10),
      format(stats::quantile(object$test_stats_lrt)[5], trim = FALSE,
             justify = "right", width = 10),
      format(object$test_stats_lrt[1], trim = FALSE, justify = "right",
             width = 10),
      format(object$p_value_lrt, trim = FALSE, justify = "right", width = 10),
      sep = "  ")
  if (object$p_value_lrt < 0.001) {
    cat(" ***\n")
  } else if (object$p_value_lrt < 0.01 & object$p_value_lrt >= 0.001) {
    cat(" **\n")
  } else if (object$p_value_lrt < 0.05 & object$p_value_lrt >= 0.01) {
    cat(" *\n")
  } else if (object$p_value_lrt < 0.1 & object$p_value_lrt >= 0.05) {
    cat(" .\n")
  } else if (object$p_value_lrt < 1 & object$p_value_lrt >= 0.1) {
    cat(" \n")
  }

  cat("chi_sq\t", format(stats::quantile(object$test_stats_chi_sq), trim = FALSE,
                        justify = "right", width = 10)[1],
      format(stats::quantile(object$test_stats_chi_sq)[2], trim = FALSE,
             justify = "right", width = 10),
      format(stats::quantile(object$test_stats_chi_sq)[3], trim = FALSE,
             justify = "right", width = 10),
      format(stats::quantile(object$test_stats_chi_sq)[4], trim = FALSE,
             justify = "right", width = 10),
      format(stats::quantile(object$test_stats_chi_sq)[5], trim = FALSE,
             justify = "right", width = 10), format(object$test_stats_chi_sq[1],
                                                    trim = FALSE,
                                                    justify = "right",
                                                    width = 10),
      format(object$p_value_chi_sq, trim = FALSE, justify = "right", width = 10),
      sep = "  ")
  if (object$p_value_chi_sq < 0.001) {
    cat(" ***\n")
  } else if (object$p_value_chi_sq < 0.01 & object$p_value_chi_sq >= 0.001) {
    cat(" **\n")
  } else if (object$p_value_chi_sq < 0.05 & object$p_value_chi_sq >= 0.01) {
    cat(" *\n")
  } else if (object$p_value_chi_sq < 0.1 & object$p_value_chi_sq >= 0.05) {
    cat(" .\n")
  } else if (object$p_value_chi_sq < 1 & object$p_value_chi_sq >= 0.1) {
    cat(" \n")
  }

  cat("Run =", object$run, format(stats::quantile(object$test_stats_run)[1],
                                  trim = FALSE, justify = "right", width = 10),
      format(stats::quantile(object$test_stats_run)[2], trim = FALSE,
             justify = "right", width = 10),
      format(stats::quantile(object$test_stats_run)[3], trim = FALSE,
             justify = "right", width = 10),
      format(stats::quantile(object$test_stats_run)[4], trim = FALSE,
             justify = "right", width = 10),
      format(stats::quantile(object$test_stats_run)[5], trim = FALSE,
             justify = "right", width = 10), format(object$test_stats_run[1],
                                                    trim = FALSE,
                                                    justify = "right",
                                                    width = 10),
      format(object$p_value_run, trim = FALSE, justify = "right", width = 10),
      sep = "  ")
  if (object$p_value_run < 0.001) {
    cat(" ***\n")
  } else if (object$p_value_run < 0.01 & object$p_value_run >= 0.001) {
    cat(" **\n")
  } else if (object$p_value_run < 0.05 & object$p_value_run >= 0.01) {
    cat(" *\n")
  } else if (object$p_value_run < 0.1 & object$p_value_run >= 0.05) {
    cat(" .\n")
  } else if (object$p_value_run < 1 & object$p_value_run >= 0.1) {
    cat(" \n")
  }

  cat("---\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' .05 '.' .1 ' ' 1\n---\n")
}

#' Produce plots from objects of class "multiple_binary_test".
#'
#' Produces some interesting plots of data and test statistics of objects of
#' \link{class} \code{multiple_binary_test}.
#'
#' @param x An object of \code{\link{class}}
#' \code{multiple_binary_test}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
plot.multiple_binary_test <- function(x, ...) {
  graphics::par(ask = TRUE,  mar = c(5, 1, 1, 1))
  y <- x$data[[1]]
  reverse <- nrow(y):1
  y <- y[reverse, ]
  graphics::image(c(1:ncol(y)), c(1:nrow(y)), t(y), axes = FALSE, ylab = " ", xlab = "Visual representation of the actual Markov Chain.", col = c("aquamarine3",
                                                                                                                                        "chocolate1"))

  graphics::par(ask = TRUE, mar = c(5, 4, 4, 2))
  graphics::hist(x$test_stats_lrt, breaks = x$bins, xlab = "LRT Test Statistics\nRed line indicates value of test statistic of real data",
       main = NULL)
  graphics::abline(v = x$test_stats_lrt[1], col = "red")

  graphics::hist(x$test_stats_chi_sq, breaks = x$bins, xlab = "Pearson's Chi-Square Test Statistics\nRed line indicates value of test statistic of real data",
       main = NULL)
  graphics::abline(v = x$test_stats_chi_sq[1], col = "red")

  graphics::hist(x$test_stats_run, breaks = x$bins, xlab = paste("Run Test Statistics For a Run of", x$run,
                                                       "\nRed line indicates value of test statistic of real data"), main = NULL)
  graphics::abline(v = x$test_stats_run[1], col = "red")
}
