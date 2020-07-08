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
  res <- switch(wkdy,
    "mmwr" = "sunday", # MMWR == CDC epiweek
    "epi"  = "sunday", # CDC epiweek
    "iso"  = "monday", # ISOweek == WHO epiweek
    wkdy # all others
  )
  gsub("epi", "", res) # if they specify something like "epiweek:saturday"
}


#' Translate a custom interval to a valid interval
#'
#' @param the_interval An interval like `"2 epiweeks"` or `"1 ISOweek"`.
#'
#' @return An interval compatible with `seq.Date()`.
#' @examples
#' get_week_duration("2 weeks (wednesday)") # 2 weeks
#' get_week_duration("2 epiweeks") # 2 weeks
#'
#' @noRd
get_week_duration <- function(the_interval) {
  if (the_interval == 7) {
    return(the_interval)
  }
  res <- gsub("^(\\d*) ?.*(weeks?).*$", "\\1 \\2", tolower(the_interval), perl = TRUE)
  trimws(res)
}


get_type_of_week <- function(x) {
  date_var <- attr(x, "date")
  week_var <- attr(x, "date_group")

  switch(
    as.character(attr(x[[week_var]], "week_start")),
    "1" = "ISO",
    "7" = "MMWR",
    sprintf("(%s)", weekdays(x[[date_var]][1]))
  )
}




