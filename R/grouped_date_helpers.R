#' Date generator
#'
#' This function allows the quick creation of Date. It is based on the internal
#'   `.Date()` function.
#'
#' @param x A double vector representing the number of days since the UNIX
#'   "epoch", 1970-01-01.
#'
#' @return a ([Date]) object.
#' * `new_posixct`: a ([POSIXct]) object.
#'
#' @examples
#' new_date(0)
#'
#' @keywords internal
#' @export
new_date <- function(x = double()) {
  class(x) <- "Date"
  x
}


# ------------------------------------------------------------------------- #
# ----------------------------- INTERNALS --------------------------------- #
# ------------------------------------------------------------------------- #

#' POSIXct generator
#'
#' This function allows the quick creation of Date. It is based on the internal
#'   `.Date()` function.
#' @param x A double vector representing the number of seconds since the UNIX
#'   "epoch", 1970-01-01.
#' @param tzone A character vector representing the desired time zone.  Defaults
#'   to "" for the local time zone.  Possible values can be found with
#'   [OlsonNames()].
#'
#' @return a ([POSIXct]) object
#' @keywords internal
new_posixct <- function(x = double(), tzone = "") {
  class(x) <- c("POSIXct", "POSIXt")
  attr(x, "tzone") <- tzone
  x
}


# check for suggested packages --------------------------------------------
check_suggests <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    msg <- sprintf("Suggested package '%s' not present.", package)
    stop(msg, call. = FALSE)
  }
}


# check if entries of a vector are whole numbers
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

# cast a vector to an integer
int_cast <- function(x) {
  if (!all(is.wholenumber(x) | is.na(x))) {
    msg <- paste(deparse1(substitute(x)), "must be a vector of whole numbers")
    stop(msg, call. = FALSE)
  }
  as.integer(x)
}

# check if interval valid
is_valid_interval <- function(x) {
  if (is.character(x)) {
    pattern <- "^\\d?\\s?(day|week|month|quarter|year)s?$"
    return(grepl(pattern, x, ignore.case = TRUE))
  } else if (is.numeric(x)) {
    if (!all(is.wholenumber(x) | is.na(x))) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  FALSE
}

# get interval prefix
get_interval_number <- function(x) {
  if (!grepl("^\\d", x)) return(1L)
  as.integer(gsub("^(\\d*).+$", "\\1", x))
}

# check if vector is Date
is.Date <- function(x) inherits(x, "Date")


# pull out tzone (timezone) of object (returns "" if it does not exist)
tzone <- function(x) {
  tz <- attr(x, "tzone")
  if(is.null(tz)) "" else tz
}


# The following is based on a functions of Davis Vaughan in
# https://github.com/DavisVaughan/datea/blob/master/R/ymon-as.R.
# It is quicker than doing as.POSIXct.Date and will work with
# all date and grate objects.
as_utc_posixct_from_int <- function(x) {
  attributes(x) <- NULL
  x <- x * 86400 # multiply by seconds in day (24 * 60 * 60)
  structure(x, tzone = "UTC", class = c("POSIXct", "POSIXt"))
}

as_zoned_posixct_from_int <- function(x, tz) {
  attributes(x) <- NULL
  x <- as.character(new_date(x))
  as.POSIXct(x, tz = tz)
}

# The following is based on a functions of Davis Vaughan in
# https://github.com/DavisVaughan/datea/blob/master/R/ymon-as.R.
# It is quicker than doing as.POSIXlt.Date and will work with
# all date and grate objects.
as_utc_posixlt_from_int <- function(x) {
  attributes(x) <- NULL
  x <- x * 86400 # multiply by seconds in day (24 * 60 * 60)
  as.POSIXlt(x, tz = "UTC", origin = new_posixct(x = 0, tzone = "UTC"))
}

as_zoned_posixlt_from_int <- function(x, tz) {
  attributes(x) <- NULL
  x <- as.character(new_date(x))
  as.POSIXlt(x, tz = tz)
}


# The following is based on the approach Davis Vaughan took in
# https://github.com/DavisVaughan/datea/blob/master/src/month.c but in R and
# extended to other cases.

# Constants ---------------------------------------------------------------

delayedAssign(
  "DAYS_IN_QUARTER",
  c(90L, 91L, 92L, 92L)
)


delayedAssign(
  "QUARTER_DAYS_IN_MONTH_BEFORE",
  c(0L, 31L, 59L, 0L, 30L, 61L, 0L, 31L, 62L, 0L, 31L, 61L)
)

delayedAssign(
  "DAYS_IN_MONTH",
  c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
)


delayedAssign(
  "DAYS_BEFORE_MONTH",
  c(0L, 31L, 59L, 90L, 120L, 151L, 181L, 212L, 243L, 273L, 304L, 334L)
)

# other useful conversions ------------------------------------------------


is_leap_year <- function(year) {
  ((((year) %% 4) == 0 & ((year) %% 100) != 0) | ((year) %% 400) == 0)
}

days_in_month <- function(year, month) {
  DAYS_IN_MONTH[month] + ((month == 2) & is_leap_year(year))
}

days_in_quarter <- function(year, quarter) {
  DAYS_IN_QUARTER[quarter] + ((quarter == 1) & is_leap_year(year))
}

quarter_days_before_month <- function(year, month) {
  QUARTER_DAYS_IN_MONTH_BEFORE[month] + ((month == 3) & is_leap_year(year))
}

days_before_month <- function(year, month) {
  DAYS_BEFORE_MONTH[month] + ((month > 2) & is_leap_year(year))
}

month_to_days <- function(months) {
  year <- months %/% 12L + 1970L
  month <- months %% 12L + 1L
  days_before_year(year) + days_before_month(year, month) - 719162L
}

date_to_month <- function(x) {
  x <- as_utc_posixlt_from_int(x)
  yr <- x$year + 1900L
  mon <- x$mon
  mon <- (yr - 1970L) * 12L + mon
  mon
}

days_before_year <- function(year = integer()) {
  year <- year - 1L
  (year * 365) + (year %/% 4) - (year %/% 100) + (year %/% 400)
}



# week helpers ------------------------------------------------------------

#' Translate user input to the start date of the week
#'
#' @param a Weekday specification: ISOweek, MMWRweek, EPIweek, Mon-week,
#'   Tue-week, etc.
#'
#' @return the corresponding weekday
#'
#' @examples
#' get_week_start("ISOweek")
#' get_week_start("MMWRweek")
#' get_week_start("EPIweek")
#'
#' # weeks that start on saturday
#' get_week_start("Sat-week")
#' get_week_start("week: Saturday")
#' get_week_start("2 weeks: Saturday")
#' get_week_start("epiweek: Saturday")
#'
#' @noRd
get_week_start <- function(weekday) {
  wkdy <- gsub("weeks?", "", tolower(weekday))
  wkdy <- gsub("[[:punct:][:blank:][:digit:]]*", "", wkdy)
  wkdy <- if (wkdy == "") "monday" else wkdy # the input was "weeks"
  res <- switch(
    wkdy,
    "mmwr" = "sunday", # MMWR == CDC epiweek
    "epi"  = "sunday", # CDC epiweek
    "iso"  = "monday", # ISOweek == WHO epiweek
    wkdy # all others
  )
  weekday_from_char(res)
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
weekday_from_char <- function(x) {

  # First try with an English locale
  w <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
  weekdate <- grep(x, w, ignore.case = TRUE)

  if (length(weekdate) == 0) {
    # find the definitions of the weekdays in the current locale
    w <- weekdays(as.Date(as_yrwk(as.Date("2020-01-01"), firstday = 1L)))
    weekdate <- grep(x, w, ignore.case = TRUE)
  }

  if (length(weekdate) != 1) {
    msg <- paste(
      "The weekday '%s' did not unambiguously match (via grep) any of the",
      "valid weekdays in the current locale ('%s') or an English locale:\n  %s"
    )
    stop(
      sprintf(msg, x, Sys.getlocale('LC_TIME'), paste(w, collapse = ", ")),
      call. = FALSE
    )
  }

  return(as.integer(weekdate))

}
