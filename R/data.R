#' Rainfall data for the month of January in Snoqualmie Falls, 1948 - 1983.
#'
#' A dataset documenting the presence of precipitation in Snoqualmie Falls
#' Washington on every day of every January from 1948 to 1983. 1 indicates a day
#' with >=0.01 inches of precipitation, 0 indicates a day with <0.01 inches of
#' precipitation. Data sourced from Peter Guttorp's Stochastic Modeling of
#' Scientific Data, 1995.
#'
#' @format A data frame with 36 rows and 31 columns.
"snoqualmie"

#' Tornado data for Oklahoma, 1950 - 2015
#'
#' A dataset documenting the presence of tornadic activity in Oklahoma. Each
#' row represents a year, starting with 1950, and ending in 2015, and each
#' column is a day of that year. Each day on which a tornado occured is marked
#' encoded as a 1 and each day without tornadic activity is encoded as a 0. The
#' data is inspired by, but not drawn from, the paper "A Markov Chain Model of
#' Tornadic Activity" by Mathias Drton, Caren Marzban, Peter Guttorp, and Joseph
#' T. Schaefer. As mentioned in their paper, "day 366 of a non-leap year is
#' coded as a non-tornadic day. This is not expected to adversely affect the
#' results." Data is sourced from the National Weather Service Weather Forecast
#' Office in Norman, Oklahoma, and can be found at
#' http://www.srh.noaa.gov/oun/?n=tornadodata-ok-monthlyannual .
#'
#' @format A data frame with 66 rows and 366 columns.
"ok_tornado"
