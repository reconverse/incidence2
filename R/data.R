#' Regional data for COVID-19 cases in the UK
#'
#' A dataset containing the daily time-series of cases, tests, hospitalisations,
#' and deaths for UK.
#'
#' Extracted using the
#' [covidregionaldata](https://CRAN.R-project.org/package=covidregionaldata)
#' package on 2021-06-03.
#'
#' @format A data frame with 6370 rows and 26 variables:
#' \describe{
#'   \item{date}{the date that the counts were reported (YYYY-MM-DD)}
#'   \item{region}{the region name}
#'   \item{region_code}{the region code}
#'   \item{cases_new}{new reported cases for that day}
#'   \item{cases_total}{total reported cases up to and including that day}
#'   \item{deaths_new}{new reported deaths for that day}
#'   \item{deaths_total}{total reported deaths up to and including that day}
#'   \item{recovered_new}{new reported recoveries for that day}
#'   \item{recovered_total}{total reported coveries up to and including that day}
#'   \item{hosp_new}{new reported hospitalisations for that day}
#'   \item{hosp_total}{total reported hospitalisations up to and including that day (note this is cumulative total of new reported, not total currently in hospital).}
#'   \item{tested_new}{tests for that day}
#'   \item{tested_total}{total tests completed up to and including that day}
#' }
#' @source \url{https://CRAN.R-project.org/package=covidregionaldata}
"covidregionaldataUK"
