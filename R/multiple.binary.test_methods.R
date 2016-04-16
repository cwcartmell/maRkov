#' Print instructions for examining objects of class "multiple.binary.test".
#'
#' Instruct user to use \code{summary} and \code{print} to examine objects of
#' \link{class} \code{multiple.binary.test}.
#'
#' @export
print.multiple.binary.test <- function(multiple.binary.test, ...) {
    cat("\ncall:\n")
    print(multiple.binary.test$call)
    cat("\n---\nUse summary() and plot() for more information.")
}

#' Produce a summary of objects of class "multiple.binary.test".
#'
#' Prints test statistics and p-values for objects of \link{class}
#' \code{multiple.binary.test}.
#'
#' @export
summary.multiple.binary.test <- function(multiple.binary.test, ..) {
    cat("\ncall:\n")
    print(multiple.binary.test$call)
    cat("\nTest statistics\n")
    cat("                 Min          1Q      Median          3Q         Max   test stat  Pr(>test stat)\n")
    cat("LRT\t", format(quantile(multiple.binary.test$test.stats.lrt)[1], trim = FALSE, justify = "right", width = 10), format(quantile(multiple.binary.test$test.stats.lrt)[2],
        trim = FALSE, justify = "right", width = 10), format(quantile(multiple.binary.test$test.stats.lrt)[3], trim = FALSE, justify = "right",
        width = 10), format(quantile(multiple.binary.test$test.stats.lrt)[4], trim = FALSE, justify = "right", width = 10), format(quantile(multiple.binary.test$test.stats.lrt)[5],
        trim = FALSE, justify = "right", width = 10), format(multiple.binary.test$test.stats.lrt[1], trim = FALSE, justify = "right", width = 10),
        format(multiple.binary.test$p.value.lrt, trim = FALSE, justify = "right", width = 10), sep = "  ")
    if (multiple.binary.test$p.value.lrt < 0.001) {
        cat(" ***\n")
    } else if (multiple.binary.test$p.value.lrt < 0.01 & multiple.binary.test$p.value.lrt >= 0.001) {
        cat(" **\n")
    } else if (multiple.binary.test$p.value.lrt < 0.05 & multiple.binary.test$p.value.lrt >= 0.01) {
        cat(" *\n")
    } else if (multiple.binary.test$p.value.lrt < 0.1 & multiple.binary.test$p.value.lrt >= 0.05) {
        cat(" .\n")
    } else if (multiple.binary.test$p.value.lrt < 1 & multiple.binary.test$p.value.lrt >= 0.1) {
        cat(" \n")
    }

    cat("ChiSq\t", format(quantile(multiple.binary.test$test.stats.chisq), trim = FALSE, justify = "right", width = 10)[1], format(quantile(multiple.binary.test$test.stats.chisq)[2],
        trim = FALSE, justify = "right", width = 10), format(quantile(multiple.binary.test$test.stats.chisq)[3], trim = FALSE, justify = "right",
        width = 10), format(quantile(multiple.binary.test$test.stats.chisq)[4], trim = FALSE, justify = "right", width = 10), format(quantile(multiple.binary.test$test.stats.chisq)[5],
        trim = FALSE, justify = "right", width = 10), format(multiple.binary.test$test.stats.chisq[1], trim = FALSE, justify = "right", width = 10),
        format(multiple.binary.test$p.value.chisq, trim = FALSE, justify = "right", width = 10), sep = "  ")
    if (multiple.binary.test$p.value.chisq < 0.001) {
        cat(" ***\n")
    } else if (multiple.binary.test$p.value.chisq < 0.01 & multiple.binary.test$p.value.chisq >= 0.001) {
        cat(" **\n")
    } else if (multiple.binary.test$p.value.chisq < 0.05 & multiple.binary.test$p.value.chisq >= 0.01) {
        cat(" *\n")
    } else if (multiple.binary.test$p.value.chisq < 0.1 & multiple.binary.test$p.value.chisq >= 0.05) {
        cat(" .\n")
    } else if (multiple.binary.test$p.value.chisq < 1 & multiple.binary.test$p.value.chisq >= 0.1) {
        cat(" \n")
    }

    cat("Run =", multiple.binary.test$run, format(quantile(multiple.binary.test$test.stats.run)[1], trim = FALSE, justify = "right", width = 10),
        format(quantile(multiple.binary.test$test.stats.run)[2], trim = FALSE, justify = "right", width = 10), format(quantile(multiple.binary.test$test.stats.run)[3],
            trim = FALSE, justify = "right", width = 10), format(quantile(multiple.binary.test$test.stats.run)[4], trim = FALSE, justify = "right",
            width = 10), format(quantile(multiple.binary.test$test.stats.run)[5], trim = FALSE, justify = "right", width = 10), format(multiple.binary.test$test.stats.run[1],
            trim = FALSE, justify = "right", width = 10), format(multiple.binary.test$p.value.run, trim = FALSE, justify = "right", width = 10),
        sep = "  ")
    if (multiple.binary.test$p.value.run < 0.001) {
        cat(" ***\n")
    } else if (multiple.binary.test$p.value.run < 0.01 & multiple.binary.test$p.value.run >= 0.001) {
        cat(" **\n")
    } else if (multiple.binary.test$p.value.run < 0.05 & multiple.binary.test$p.value.run >= 0.01) {
        cat(" *\n")
    } else if (multiple.binary.test$p.value.run < 0.1 & multiple.binary.test$p.value.run >= 0.05) {
        cat(" .\n")
    } else if (multiple.binary.test$p.value.run < 1 & multiple.binary.test$p.value.run >= 0.1) {
        cat(" \n")
    }

    cat("---\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' .05 '.' .1 ' ' 1\n---\n")
}

#' Produce plots from objects of class "multiple.binary.test".
#'
#' Produces some interesting plots of data and test statistics of objects of
#' \link{class} \code{multiple.binary.test}.
#'
#' @export
plot.multiple.binary.test <- function(multiple.binary.test, ...) {
    par(ask = TRUE)
    x <- multiple.binary.test$data[[1]]
    reverse <- nrow(x):1
    x <- x[reverse, ]
    image(c(1:ncol(x)), c(1:nrow(x)), t(x), axes = FALSE, ylab = " ", xlab = "Visual representation of the actual Markov Chain.", col = c("aquamarine3",
        "chocolate1"))


    hist(multiple.binary.test$test.stats.lrt, breaks = multiple.binary.test$bins, xlab = "LRT Test Statistics\nRed line indicates value of test statistic of real data",
        main = NULL)
    abline(v = multiple.binary.test$test.stats.lrt[1], col = "red")

    hist(multiple.binary.test$test.stats.chisq, breaks = multiple.binary.test$bins, xlab = "Pearson's Chi-Square Test Statistics\nRed line indicates value of test statistic of real data",
        main = NULL)
    abline(v = multiple.binary.test$test.stats.chisq[1], col = "red")

    hist(multiple.binary.test$test.stats.run, breaks = multiple.binary.test$bins, xlab = paste("Run Test Statistics For a Run of", multiple.binary.test$run,
        "\nRed line indicates value of test statistic of real data"), main = NULL)
    abline(v = multiple.binary.test$test.stats.run[1], col = "red")
}
