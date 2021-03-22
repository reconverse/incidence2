#' @import grates
NULL

make_grate <- function(x, interval, firstdate, ...) {
  stopifnot("Interval is not valid" = valid_interval(interval))
  UseMethod("make_grate")
}

make_grate.default <- function(x, interval, firstdate, ...) {

  if (is.numeric(interval)) {
    return(as_period(x, interval = interval, firstdate = firstdate))
  }

  n <- get_interval_number(interval)
  type <- get_interval_type(interval)
  if (n == 1L) {
    if (type == "week") {
      fd <- get_week_start(interval)
      res <- as_yrwk(x, firstday = fd)
    } else if (type == "month") {
      res <- as_yrmon(x)
    } else if (type == "quarter") {
      res <- as_yrqtr(x)
    } else if (type == "year") {
      res <- as_yr(x)
    } else {
      res <- as_period(x, interval = interval, firstdate = firstdate)
    }
  } else {
    res <- as_period(x, interval = interval, firstdate = firstdate)
  }

  res
}

make_grate.integer <- function(x, interval, firstdate, ...) {
  as_int_period(x, interval = interval, firstdate = firstdate)
}

make_grate.numeric <- make_grate.integer

valid_interval <- function(interval) {

  # integer intervals are fine
  if (is.integer(interval)) {
    return(TRUE)
  }

  # numeric intervals are ok if they are effectively integers
  if (is.numeric(interval)) {
    if (all(is.wholenumber(interval))) {
      return(TRUE)
    }
  }

  # character intervals are more tricky
  if (is.character(interval)) {
    if (!valid_date_period_character(interval)) {
      suppressWarnings(interval <- as.numeric(interval))
      if (is.na(interval)) {
        stop(
          'The interval must be a whole number or one of the following:\n',
          '     "(x) day(s)"\n',
          '     "(x) weeks(s)"\n',
          '     "(x) epiweeks(s)"\n',
          '     "(x) isoweeks(s)"\n',
          '     "(x) months(s)"\n',
          '     "(x) quarter(s)"\n',
          '     "(x) years(s)"\n',
          call. = FALSE
        )
      } else {
        return(TRUE)
      }
    }
  }

  return(TRUE)
}

valid_date_period_character <- function(x) {
  # have to ensure saturday does not cause issues
  day <- paste0(
    "^\\s*days?\\s*$|",
    "\\sdays?\\s+|",
    "\\sdays?\\s*$|"
  )
  pattern <- paste0(day,"week|epiweek|isoweek|month|quarter|year")
  grepl(pattern, x, ignore.case = TRUE)
}

