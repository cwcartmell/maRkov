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

#' Data from the Madras schizophrenia study
#'
#' A dataset documenting the presence of thought disorders across 12 months
#' since hospitalization. Each column represents a single patient, and each
#' row represents a seperate month. Months with thought disorders are encoded as
#' 1 and those without are encoded as 0. Data is sourced from "Analysis of
#' Longitudinal Data" by Peter J. Diggle, Patrick J. Heagerty, Kung-Yee Liang,
#' and Scott L. Zeger. It was retrieved from
#' http://faculty.washington.edu/heagerty/Books/AnalysisLongitudinal/madras.data
#' . Individuals who did not have 12 months of post hospitalization data were
#' removed to make the data rectangular.
#'
#' @format A data frame with 69 rows and 12 columns.
"madras"
