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
  x <- unclass(x)
  if (!all(is.wholenumber(x) | is.na(x))) {
    msg <- paste(deparse1(substitute(x)), "must be a vector of whole numbers")
    stop(msg, call. = FALSE)
  }
  res <- as.integer(x)
  names(res) <- names(x)
  res
}


get_interval_number <- function(x) {
  if (!grepl("^\\d", x)) return(1L)
  as.integer(gsub("^(\\d*).*$", "\\1", x))
}


get_interval_type <- function(x) {

  if (!is.character(x)) {
    return(typeof(x))
  }

  day <- "^\\s*days?\\s*$|\\sdays?\\s+|\\sdays?\\s*$"
  if (grepl(day, x, ignore.case = TRUE)) {
    return("day")
  } else if (grepl("week", x, ignore.case = TRUE)) {
    return("week")
  }  else if (grepl("month", x, ignore.case = TRUE)) {
    return("month")
  } else if (grepl("quarter", x, ignore.case = TRUE)) {
    return("quarter")
  } else if (grepl("year", x, ignore.case = TRUE)) {
    return("year")
  }  else {
    return("day")
  }
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
get_week_start <- function(weekday, numeric = TRUE) {
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
  weekday_from_char(res, numeric)
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
    w <- weekdays(as.Date(as_yrwk(as.Date("2020-01-01"), firstday = 1L)) + 0:6)
    weekday <- grep(x, w, ignore.case = TRUE, value = !numeric)
  }

  if (length(weekday) != 1) {
    msg <- paste(
      "The weekday '%s' did not unambiguously match (via grep) any of the",
      "valid weekdays in the current locale ('%s') or an English locale:\n  %s"
    )
    stop(
      sprintf(msg, x, Sys.getlocale('LC_TIME'), paste(w, collapse = ", ")),
      call. = FALSE
    )
  }

  if (numeric) {
    return(as.integer(weekday))
  } else {
    return(weekday)
  }
}

