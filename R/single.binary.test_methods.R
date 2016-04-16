#' Print instructions for examining objects of class "single.binary.test".
#'
#' Instruct user to use \code{summary} and \code{print} to examine objects of
#' \link{class} \code{single.binary.test}.
#'
#' @export
print.single.binary.test <- function(single.binary.test, ...) {
    cat("\nCall:\n")
    print(single.binary.test$call)
    cat("\n---\nUse summary() and plot() for more information.")
}

#' Produce a summary of objects of class "single.binary.test".
#'
#' Prints test statistics, p-values, and provides sample data for objects of
#' \link{class} \code{single.binary.test}.
#'
#' @export
summary.single.binary.test <- function(single.binary.test, ...) {
    cat("\nCall:\n")
    print(single.binary.test$call)
    cat("\nVarious test statistics:\n")
    cat("                 Min          1Q      Median          3Q         Max   test stat  Pr(>test stat)\n")
    cat("LRT\t", format(quantile(single.binary.test$test.stats.lrt)[1], trim = FALSE, justify = "right", width = 10), format(quantile(single.binary.test$test.stats.lrt)[2],
        trim = FALSE, justify = "right", width = 10), format(quantile(single.binary.test$test.stats.lrt)[3], trim = FALSE, justify = "right", width = 10),
        format(quantile(single.binary.test$test.stats.lrt)[4], trim = FALSE, justify = "right", width = 10), format(quantile(single.binary.test$test.stats.lrt)[5],
            trim = FALSE, justify = "right", width = 10), format(single.binary.test$test.stats.lrt[1], trim = FALSE, justify = "right", width = 10),
        format(single.binary.test$p.value.lrt, trim = FALSE, justify = "right", width = 10), sep = "  ")
    if (single.binary.test$p.value.lrt < 0.001) {
        cat(" ***\n")
    } else if (single.binary.test$p.value.lrt < 0.01 & single.binary.test$p.value.lrt >= 0.001) {
        cat(" **\n")
    } else if (single.binary.test$p.value.lrt < 0.05 & single.binary.test$p.value.lrt >= 0.01) {
        cat(" *\n")
    } else if (single.binary.test$p.value.lrt < 0.1 & single.binary.test$p.value.lrt >= 0.05) {
        cat(" .\n")
    } else if (single.binary.test$p.value.lrt < 1 & single.binary.test$p.value.lrt >= 0.1) {
        cat(" \n")
    }

    cat("ChiSq\t", format(quantile(single.binary.test$test.stats.chisq), trim = FALSE, justify = "right", width = 10)[1], format(quantile(single.binary.test$test.stats.chisq)[2],
        trim = FALSE, justify = "right", width = 10), format(quantile(single.binary.test$test.stats.chisq)[3], trim = FALSE, justify = "right",
        width = 10), format(quantile(single.binary.test$test.stats.chisq)[4], trim = FALSE, justify = "right", width = 10), format(quantile(single.binary.test$test.stats.chisq)[5],
        trim = FALSE, justify = "right", width = 10), format(single.binary.test$test.stats.chisq[1], trim = FALSE, justify = "right", width = 10),
        format(single.binary.test$p.value.chisq, trim = FALSE, justify = "right", width = 10), sep = "  ")
    if (single.binary.test$p.value.run < 0.001) {
        cat(" ***\n")
    } else if (single.binary.test$p.value.run < 0.01 & single.binary.test$p.value.run >= 0.001) {
        cat(" **\n")
    } else if (single.binary.test$p.value.run < 0.05 & single.binary.test$p.value.run >= 0.01) {
        cat(" *\n")
    } else if (single.binary.test$p.value.run < 0.1 & single.binary.test$p.value.run >= 0.05) {
        cat(" .\n")
    } else if (single.binary.test$p.value.run < 1 & single.binary.test$p.value.run >= 0.1) {
        cat(" \n")
    }

    cat("Run =", single.binary.test$run, format(quantile(single.binary.test$test.stats.run)[1], trim = FALSE, justify = "right", width = 10), format(quantile(single.binary.test$test.stats.run)[2],
        trim = FALSE, justify = "right", width = 10), format(quantile(single.binary.test$test.stats.run)[3], trim = FALSE, justify = "right", width = 10),
        format(quantile(single.binary.test$test.stats.run)[4], trim = FALSE, justify = "right", width = 10), format(quantile(single.binary.test$test.stats.run)[5],
            trim = FALSE, justify = "right", width = 10), format(single.binary.test$test.stats.run[1], trim = FALSE, justify = "right", width = 10),
        format(single.binary.test$p.value.run, trim = FALSE, justify = "right", width = 10), sep = "  ")
    if (single.binary.test$p.value.run < 0.001) {
        cat(" ***\n")
    } else if (single.binary.test$p.value.run < 0.01 & single.binary.test$p.value.run >= 0.001) {
        cat(" **\n")
    } else if (single.binary.test$p.value.run < 0.05 & single.binary.test$p.value.run >= 0.01) {
        cat(" *\n")
    } else if (single.binary.test$p.value.run < 0.1 & single.binary.test$p.value.run >= 0.05) {
        cat(" .\n")
    } else if (single.binary.test$p.value.run < 1 & single.binary.test$p.value.run >= 0.1) {
        cat(" \n")
    }

    cat("---\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' .05 '.' .1 ' ' 1\n---\n")
    cat("Quick sample of real and generated data:\n")
    cat("       Real data :\t\t", single.binary.test$data[1, ], "\n")
    cat("Generated data 1 :\t\t", single.binary.test$data[2, ], "\n")
    cat("Generated data 2 :\t\t", single.binary.test$data[3, ], "\n")
    cat(".\n.\n.\n")
    cat("Generated data", length(single.binary.test$data[, 1]) - 1, ":\t", single.binary.test$data[length(single.binary.test$data[, 1]), ], "\n")
}

#' Produce plots from objects of class "single.binary.test".
#'
#' Produces some interesting plots of data and test statistics of objects of
#' \link{class} \code{single.binary.test}.
#'
#' @export
plot.single.binary.test <- function(single.binary.test, ...) {
    par(ask = TRUE)
    x <- single.binary.test$data
    x <- x[1:single.binary.test$tiles, ]
    reverse <- nrow(x):1
    # yLabels <- yLabels[reverse]
    x <- x[reverse, ]
    image(c(1:ncol(x)), c(1:nrow(x)), t(x), axes = FALSE, ylab = " ", xlab = "Real data above the black line, generated data below it", col = c("aquamarine3",
        "chocolate1"))
    abline(h = single.binary.test$tiles - 0.5, col = "black", lwd = 4)

    hist(single.binary.test$test.stats.lrt, breaks = single.binary.test$bins, xlab = "LRT Test Statistics\nRed line indicates value of test statistic of real data",
        main = NULL)
    abline(v = single.binary.test$test.stats.lrt[1], col = "red")

    hist(single.binary.test$test.stats.chisq, breaks = single.binary.test$bins, xlab = "Pearson's Chi-Square Test Statistics\nRed line indicates value of test statistic of real data",
        main = NULL)
    abline(v = single.binary.test$test.stats.chisq[1], col = "red")

    hist(single.binary.test$test.stats.run, breaks = single.binary.test$bins, xlab = paste("Run Test Statistics For a Run of", single.binary.test$run,
        "\nRed line indicates value of test statistic of real data"), main = NULL)
    abline(v = single.binary.test$test.stats.run[1], col = "red")
}
