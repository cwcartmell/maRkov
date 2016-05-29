#' Print instructions for examining objects of class "single_binary_test".
#'
#' Instruct user to use \code{summary} and \code{print} to examine objects of
#' \link{class} \code{single_binary_test}.
#'
#' @param x An object of \code{\link{class}}
#' \code{single_binary_test}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.single_binary_test <- function(x, ...) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n---\nUse summary() and plot() for more information.")
}

#' Produce a summary of objects of class "single_binary_test".
#'
#' Prints test statistics, p-values, and provides sample data for objects of
#' \link{class} \code{single_binary_test}.
#'
#' @param object An object of \code{\link{class}}
#' \code{single_binary_test}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
summary.single_binary_test <- function(object, ...) {
  cat("\nCall:\n")
  print(object$call)
  cat("\nVarious test statistics:\n")
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

  cat("chi_sq\t",
      format(stats::quantile(object$test_stats_chi_sq)[1], trim = FALSE,
             justify = "right", width = 10, digits = 6),
      format(stats::quantile(object$test_stats_chi_sq)[2], trim = FALSE,
             justify = "right", width = 10, digits = 6),
      format(stats::quantile(object$test_stats_chi_sq)[3], trim = FALSE,
             justify = "right", width = 10, digits = 6),
      format(stats::quantile(object$test_stats_chi_sq)[4], trim = FALSE,
             justify = "right", width = 10, digits = 6),
      format(stats::quantile(object$test_stats_chi_sq)[5], trim = FALSE,
             justify = "right", width = 10, digits = 6),
      format(object$test_stats_chi_sq[1], trim = FALSE, justify = "right",
             width = 10, digits = 6),
      format(object$p_value_chi_sq, trim = FALSE, justify = "right", width = 10,
             digits = 6),
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
             justify = "right", width = 10),
      format(object$test_stats_run[1], trim = FALSE, justify = "right",
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
  cat("Quick sample of real and generated data:\n")
  cat("       Real data :\t\t", object$data[1, ], "\n")
  cat("Generated data 1 :\t\t", object$data[2, ], "\n")
  cat("Generated data 2 :\t\t", object$data[3, ], "\n")
  cat(".\n.\n.\n")
  cat("Generated data", length(object$data[, 1]) - 1, ":\t\t",
      object$data[length(object$data[, 1]), ], "\n")
}

#' Produce plots from objects of class "single_binary_test".
#'
#' Produces some interesting plots of data and test statistics of objects of
#' \link{class} \code{single_binary_test}.
#'
#' @param x An object of \code{\link{class}}
#' \code{single_binary_test}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
plot.single_binary_test <- function(x, ...) {
  graphics::par(ask = TRUE, mar = c(5, 1, 1, 1))
  y <- x$data
  y <- y[1:x$tiles, ]
  reverse <- nrow(y):1
  y <- y[reverse, ]
  graphics::image(c(1:ncol(y)), c(1:nrow(y)), t(y), axes = FALSE, ylab = " ", xlab = "Real data above the black line, generated data below it", col = c("aquamarine3",
                                                                                                                                              "chocolate1"))
  graphics::abline(h = x$tiles - 0.5, col = "black", lwd = 4)

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
