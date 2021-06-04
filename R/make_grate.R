make_grate <- function(x, interval, firstdate) {

  n <- get_interval_number(interval)
  type <- get_interval_type(interval)

  if (is.numeric(x)) {
    stopifnot("Invalid interval for numeric `date_index`" = type == "period")
    stopifnot("`firstdate` must be numeric" = is.numeric(firstdate))
    out <- x
    if (n > 1) {
      origin <- unclass(firstdate) %% n
      out <- grates::as_int_period(x, n = n, origin = origin)
    }
  } else if (type == "week") {
    firstday <- get_yearweek_start(interval)
    out <- grates::as_yearweek(x, firstday = firstday)
    if (n > 1) {
      firstdate <- as.Date(min(out, na.rm = TRUE))
      origin <- unclass(firstdate) %% (7*n)
      out <- grates::as_period(as.Date(out), n = (7*n), origin = origin)
    }
  } else if (type == "month") {
    firstdate <- as.Date(firstdate)
    origin <- origin_from_firstdate(firstdate)
    out <- grates::as_month(x, n = n, origin = origin)
  } else if (type == "quarter") {
    stopifnot("Invalid interval - cannot group by multiple quarters" = n == 1)
    out <- grates::as_quarter(x)
  } else if (type == "year") {
    out <- grates::as_year(x)
    stopifnot("Invalid interval - cannot group by multiple years" = n == 1)
  } else {
    if (n > 1) {
      firstdate <- as.Date(firstdate)
      origin <- unclass(firstdate) %% n
      out <- grates::as_period(x, n = n, origin = origin)
    } else if (is.character(x)){
      out <- clock::date_parse(x)
    } else {
      out <- x
    }
  }
  out
}

# parse interval type
get_interval_type <- function(interval) {

  # if interval is numeric check cast to integer (will error if not possible)
  if (is.numeric(interval)) {
    interval <- vctrs::vec_cast(interval, integer())
    vctrs::vec_assert(interval, size = 1L)
    return("period")
  }

  # be careful to check for day first due to monDAY, tuesDAY, etc.
  day <- "^\\s*days?\\s*$|\\sdays?\\s+|\\sdays?\\s*$"

  if (grepl(day, interval, ignore.case = TRUE)) {
    return("period")
  } else if (grepl("week", interval, ignore.case = TRUE)) {
    return("week")
  }  else if (grepl("month", interval, ignore.case = TRUE)) {
    return("month")
  } else if (grepl("quarter", interval, ignore.case = TRUE)) {
    return("quarter")
  } else if (grepl("year", interval, ignore.case = TRUE)) {
    return("year")
  } else {
    abort(c(
      'Invalid interval. Interval must be a whole number or one of the following',
      '     "(x) day(s)"',
      '     "(x) weeks(s)"',
      '     "(x) epiweeks(s)"',
      '     "(x) isoweeks(s)"',
      '     "(x) months(s)"',
      '     "(x) quarter(s)"',
      '     "(x) years(s)"'
    ))
  }
}

# parse interval number
get_interval_number <- function(x) {
  if (!grepl("^\\d", x)) return(1L)
  as.integer(gsub("^(\\d*).*$", "\\1", x))
}

# parse yearweek interval
get_yearweek_start <- function(interval, numeric = TRUE) {
  weekday <- gsub("weeks?", "", tolower(interval))
  weekday <- gsub("[[:punct:][:blank:][:digit:]]*", "", weekday)

  if (weekday == "") return(1) # input was week or weeks => default to monday

  weekday <- switch(
    weekday,
    "mmwr" = "sunday", # MMWR == CDC epiweek
    "epi"  = "sunday", # CDC epiweek
    "iso"  = "monday", # ISOweek == WHO epiweek
    weekday # all others
  )
  weekday_from_char(weekday, numeric)
}

origin_from_firstdate <- function(x) {
  x <- as.POSIXlt(x, tz="UTC")
  yr <- x$year + 1900L # calculate the year
  mon <- (yr - 1970L) * 12L + x$mon # calculate the month relative to unix epoch
  mon
}

# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# -------- The following function is modified from the aweek package --------- #
# -------------------------- copyright Zhian Kamvar -------------------------- #
# ---------------------------------------------------------------------------- #

# Copy of the aweek licence
# MIT License
# Copyright (c) 2019
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in
#   all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' Helper function to find the weekday from a character string
#'
#' @param x a character string specifying the weekday in the current locale or
#'   English.
#'
#' @return an integer from 1 to 7 indicating the day of the ISO 8601 week.
#' @keywords internal
#' @noRd
#' @examples
#'
#' # Will always work
#' weekday_from_char("Monday")
#' weekday_from_char("Tue")
#' weekday_from_char("W")
#'
#' # Change to a German locale
#' lct <- Sys.getlocale("LC_TIME")
#' Sys.setlocale("LC_TIME", "de_DE.utf8")
#'
#' weekday_from_char("Sonntag")
#'
#' # Reset locale
#' Sys.setlocale("LC_TIME", lct)
weekday_from_char <- function(x, numeric = TRUE) {

  # First try with an English locale
  w <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
  weekday <- grep(x, w, ignore.case = TRUE, value = !numeric)

  if (length(weekday) == 0) {
    # find the definitions of the weekdays in the current locale
    w <- weekdays(as.Date(grates::as_yearweek(as.Date("2020-01-01"), firstday = 1L)) + 0:6)
    weekday <- grep(x, w, ignore.case = TRUE, value = !numeric)
  }

  if (length(weekday) != 1) {
    msg <- paste(
      "The weekday '%s' did not unambiguously match (via grep) any of the",
      "valid weekdays in the current locale ('%s') or an English locale:\n  %s"
    )
    abort(sprintf(msg, x, Sys.getlocale('LC_TIME'), paste(w, collapse = ", ")))
  }

  if (numeric) {
    return(as.integer(weekday))
  } else {
    return(weekday)
  }
}


