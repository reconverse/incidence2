#' @import grates
NULL

make_grate <- function(x, interval, firstdate) {

  n <- get_interval_number(interval)
  type <- get_interval_type(interval)

  if (type == "week") {
    firstday <- get_yearweek_start(interval)
    out <- as_yearweek(x, firstday = firstday)
    if (n > 1) {
      out <- as_period(as.Date(out), interval = 7 * n)
    }
  } else if (type == "month") {
    out <- as_month(x, interval = n, origin = firstdate)
  } else if (type == "quarter") {
    if (n > 1) {
      abort("Invalid interval - cannot group by multiple quarters")
    }
    out <- as_quarter(x)
  } else if (type == "year") {
    out <- as_year(x)
    if (n > 1) {
      abort("Invalid interval - cannot group by multiple years")
    }
  } else {
    out <- as_period(x, interval = n, origin = firstdate)
  }
  out
}

# parse interval type
get_interval_type <- function(interval) {

  # if interval is numeric check cast to integer (will error if not possible)
  if (is.numeric(interval)) {
    vctrs::vec_cast(interval, integer(), x_arg = "interval")
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
    w <- weekdays(as.Date(as_yearweek(as.Date("2020-01-01"), firstday = 1L)) + 0:6)
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


