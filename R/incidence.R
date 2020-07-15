#' Compute the incidence of events
#'
#' @param x A tibble or a data frame (see Note) representing a linelist.
#'
#' @param date_index The time index of the given data.  This should be the name,
#'   with or without quotation, corresponding to a date column in x of the
#'   class:  integer, numeric, Date, POSIXct, POSIXlt, and character. (See Note
#'   about `numeric` and `character` formats)
#'
#' @param interval An integer or character indicating the (fixed) size of the
#'   time interval used for computing the incidence; defaults to 1 day. This can
#'   also be a text string that corresponds to a valid date interval: day, week,
#'   month, quarter, or year. (See Note).
#'
#' @param groups An optional vector giving the names of the groups of
#'   observations for which incidence should be grouped.  This can be given with
#'   or without quotation.`
#'
#' @param na_as_group A logical value indicating if missing group values (NA)
#'   should treated as a separate category (`TRUE`) or removed from
#'   consideration (`FALSE`).
#'
#' @param first_date,last_date optional first/last dates to be used. When
#'   these are `NULL` (default), the dates from the first/last dates are taken
#'   from the observations. If these dates are provided, the observations will
#'   be trimmed to the range of \[first_date, last_date\].
#'
#' @param ... Additional arguments used by other methods.
#'
#' @return An incidence object.  This is a subclass of tibble that represents
#'   and aggregated count of observations grouped according to the specified
#'   interval and, optionally, the given groups.  By default it will contain the
#'   following columns:
#'
#'   - **bin_date**:  The dates marking the left side of the bins used for
#'   counting events. When `standard = TRUE` and the interval represents weeks,
#'   months, quarters, or years, the first date will represent the first
#'   standard date (See Interval specification, below).
#'
#'   - **-groups-**: If specified, column(s) containing the categories of the
#'   given groups.
#'
#'   - **count**: The aggregated observation count.
#'
#'   If a "week" interval is specified then the object may also contain
#'   additional columns:
#'
#'   - **weeks**: Dates in week format (YYYY-Www), where YYYY corresponds to the
#'   year of the given week and ww represents the numeric week of the year.
#'   This will be a produced from the function [aweek::date2week()]. Note that
#'   these will have a special `"week_start"` attribute indicating which day of
#'   the ISO week the week starts on (see Weeks, below).
#'
#'   - **isoweeks**: ISO 8601 week format YYYY-Www, which is returned only when
#'   ISO week-based weekly incidence is computed.
#'
#' @note
#'
#' \subsection{Input data (`dates`)}{
#'  - **Decimal (numeric) dates**: will be truncated with a warning
#'  - **Character dates** should be in the unambiguous `yyyy-mm-dd` (ISO 8601)
#'   format. Any other format will trigger an error.
#' }
#'
#' \subsection{Interval specification (`interval`)}{
#' If `interval` is a valid character (e.g. "week" or "1 month"), then
#' the bin will start at the beginning of the interval just before the first
#' observation by default. For example, if the first case was recorded on
#' Wednesday, 2018-05-09:
#'
#'  - "week"    : first day of the week (i.e. Monday, 2018-05-07) (defaults to ISO weeks, see "Week intervals", below)
#'  - "month"   : first day of the month (i.e. 2018-05-01)
#'  - "quarter" : first day of the quarter (i.e. 2018-04-01)
#'  - "year"    : first day of the calendar year (i.e. 2018-01-01)
#'
#' These default intervals can be overridden with `standard = FALSE`, which
#' sets the interval to begin at the first observed case.
#' }
#'
#' \subsection{Week intervals}{
#'
#' As of _incidence_ version 1.7.0, it is possible to construct standardized
#' incidence objects standardized to any day of the week thanks to the
#' [aweek::date2week()] function from the \pkg{aweek} package. The default
#' state is to use ISO 8601 definition of weeks, which start on Monday. You can
#' specify the day of the week an incidence object should be standardised to by
#' using the pattern "{n} {W} weeks" where "{W}" represents the weekday in an
#' English or current locale and "{n}" represents the duration, but this can be
#' ommitted.  Below are examples of specifying weeks starting on different days
#' assuming we had data that started on 2016-09-05, which is ISO week 36 of
#' 2016:
#'
#'  - interval = "2 monday weeks" (Monday 2016-09-05)
#'  - interval = "1 tue week" (Tuesday 2016-08-30)
#'  - interval = "1 Wed week" (Wednesday 2016-08-31)
#'  - interval = "1 Thursday week" (Thursday 2016-09-01)
#'  - interval = "1 F week" (Friday 2016-09-02)
#'  - interval = "1 Saturday week" (Saturday 2016-09-03)
#'  - interval = "Sunday week" (Sunday 2016-09-04)
#'
#' It's also possible to use something like "3 weeks: Saturday"; In addition,
#' there are keywords reserved for specific days of the week:
#'
#'   - interval = "week", standard = TRUE (Default, Monday)
#'   - interval = "ISOweek"  (Monday)
#'   - interval = "EPIweek"  (Sunday)
#'   - interval = "MMWRweek" (Sunday)
#'
#' The "EPIweek" specification is not strictly reserved for CDC epiweeks, but
#' can be prefixed (or posfixed) by a day of the week: "1 epiweek: Saturday".
#'
#' }
#'
#' The intervals for "month", "quarter", and "year" will necessarily vary in the
#' number of days they encompass and warnings will be generated when the first
#' date falls outside of a calendar date that is easily represented across the
#' interval.
#'
#' @author Thibaut Jombart, Rich Fitzjohn, Zhian Kamvar, Tim Taylor
#'
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'   withAutoprint({
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'
#'
#'     # daily incidence
#'     dat %>%
#'       incidence(date_of_onset)
#'
#'     # weekly incidence
#'     dat %>%
#'       incidence(date_of_onset, interval = 7, standard = FALSE)
#'
#'     # starting on a Monday
#'     dat %>%
#'       incidence(date_of_onset, interval = "isoweek")
#'
#'     # starting on a Sunday
#'     dat %>%
#'       incidence(date_of_onset, interval = "epiweek")
#'
#'     # starting on a Saturday
#'     dat %>%
#'       incidence(date_of_onset, interval = "saturday epiweek")
#'
#'     # group by gender
#'     dat %>%
#'       incidence(date_of_onset, interval = 7, groups = gender)
#'
#'     # group by gender and hospital
#'     dat %>%
#'       incidence(date_of_onset,
#'                 interval = "2 weeks",
#'                 groups = c(gender, hospital))
#'   })
#' }
#'
#' # use of first_date
#' dat <- data.frame(dates = Sys.Date() + sample(-3:10, 10, replace = TRUE))
#' dat %>% incidence(dates,
#'                   interval = "week",
#'                   first_date = Sys.Date() + 1,
#'                   standard = TRUE)
#' @export
incidence <- function(x, date_index, interval = 1L, ...) {

  # change date_index to character vector
  date_index <- arg_values(!!rlang::enexpr(date_index))
  UseMethod("incidence", x[[date_index]])
}


#' @export
#' @rdname incidence
incidence.default <- function(x, date_index, interval = 1L, ...) {

  # change date_index to character vector
  date_index <- arg_values(!!rlang::enexpr(date_index))

  x[[date_index]] <- check_dates(x[[date_index]])
  incidence(x, date_index, interval = interval, ...)
}

#' @param standard (Only applicable where date_index references a Date object)
#'   When `TRUE` (default) and the `interval` one of "week", "month", "quarter",
#'   or "year", then this will cause the bins for the counts to start at the
#'   beginning of the interval (See Note).

#' @export
#' @rdname incidence
incidence.Date <- function(x, date_index, interval = 1L, standard = TRUE,
                           groups = NULL, na_as_group = TRUE,
                           first_date = NULL, last_date = NULL, ...) {

  # Dots only present for potential future use.
  # This should catch and error if used
  # TODO - check this and later on.exit actions
  #ellipsis::check_dots_used(action = rlang::abort)
  ellipsis::check_dots_empty()


  # change date_index and group to character vectors
  date_index <- arg_values(!!rlang::enexpr(date_index))
  groups <- arg_values(!!rlang::enexpr(groups))

  # make sure input can be used (other checks in make incidence)
  # R 4.0. makes use of a new `stopifnot` format
  stopifnot(
    "The argument `date_index` should be of length one" =
      (length(date_index) == 1),
    "The argument `first_date` should be of length one if not null" =
      (length(first_date) == 1 || is.null(first_date)),
    "The argument `last_date` should be of length one if not null" =
      (length(last_date) == 1 || is.null(last_date)),
    "The argument `standard` must be either `TRUE` or `FALSE`." =
      (is.logical(standard)),
    "The argument `na_as_group` must be either `TRUE` or `FALSE`." =
      (is.logical(na_as_group))
  )

  # check variables present
  column_names <- names(x)
  check_presence(c(groups, date_index), column_names)

  out <- make_incidence(x,
    date_index = date_index,
    interval = interval,
    groups = groups,
    na_as_group = na_as_group,
    first_date = first_date,
    last_date = last_date,
    standard = standard,
    ...
  )

  out <- group_labels(out, interval, standard)

  out
}


#' @param standard (Only applicable where date_index references a Date object)
#'   When `TRUE` (default) and the `interval` one of "week", "month", "quarter",
#'   or "year", then this will cause the bins for the counts to start at the
#'   beginning of the interval (See Note).

#' @export
#' @rdname incidence
incidence.character <- function(x, date_index, interval = 1L, standard = TRUE,
                                groups = NULL, na_as_group = TRUE,
                                first_date = NULL, last_date = NULL, ...) {

  # Dots only present for potential future use.
  # This should catch and error if used
  # TODO - check this and later on.exit actions
  #ellipsis::check_dots_used()
  ellipsis::check_dots_empty()

  # change date_index and group to character vectors
  date_index <- arg_values(!!rlang::enexpr(date_index))
  groups <- arg_values(!!rlang::enexpr(groups))

  # make sure input can be used (other checks in make incidence)
  # R 4.0. makes use of a new `stopifnot` format
  stopifnot(
    "The argument `date_index` should be of length one" =
      (length(date_index) == 1),
    "The argument `first_date` should be of length one if not null" =
      (length(first_date) == 1 || is.null(first_date)),
    "The argument `last_date` should be of length one if not null" =
      (length(last_date) == 1 || is.null(last_date)),
    "The argument `standard` must be either `TRUE` or `FALSE`." =
      (is.logical(standard)),
    "The argument `na_as_group` must be either `TRUE` or `FALSE`." =
      (is.logical(na_as_group))
  )


  dates <- x[[date_index]]
  iso_std <- grepl("^[0-9]{4}-[01][0-9]-[0-3][0-9]$", trimws(dates))
  iso_std[is.na(dates)] <- TRUE # prevent false alarms
  if (!all(iso_std)) {
    msg <- paste(
      "Not all dates are in ISO 8601 standard format (yyyy-mm-dd).",
      "The first incorrect date is %s"
    )
    stop(sprintf(msg, dates[!iso_std][1]))
  }
  dates <- check_dates(dates)

  x[[date_index]] <- as.Date(trimws(dates))

  out <- make_incidence(x,
    date_index = date_index,
    interval = interval,
    groups = groups,
    na_as_group = na_as_group,
    first_date = first_date,
    last_date = last_date,
    standard = standard,
    ...
  )

  out <- group_labels(out, interval, standard)

  out
}


# The default incidence is designed for dates provided as integers, and a fixed
# time interval defaulting to 1. 'bins' are time intervals, identified by the
# left date, left-inclusive and right-exclusive, i.e. the time interval defined
# by d1 and d2 is [d1, d2[.

#' @export
#' @rdname incidence
incidence.integer <- function(x, date_index, interval = 1L,
                              groups = NULL, na_as_group = TRUE,
                              first_date = NULL, last_date = NULL, ...) {

  # Dots only present for potential future use.
  # This should catch and error if used
  ellipsis::check_dots_empty()

  # change date_index and group to character vectors
  date_index <- arg_values(!!rlang::enexpr(date_index))
  groups <- arg_values(!!rlang::enexpr(groups))

  # make sure input can be used (other checks in make incidence)
  # R 4.0. makes use of a new `stopifnot` format
  stopifnot(
    "The argument `date_index` should be of length one" =
      (length(date_index) == 1),
    "The argument `first_date` should be of length one if not null" =
      (length(first_date) == 1 || is.null(first_date)),
    "The argument `last_date` should be of length one if not null" =
      (length(last_date) == 1 || is.null(last_date)),
    "The argument `na_as_group` must be either `TRUE` or `FALSE`." =
      (is.logical(na_as_group))
  )

  interval <- valid_interval_integer(interval)

  out <- make_incidence(x,
    date_index = date_index,
    interval = interval,
    groups = groups,
    na_as_group = na_as_group,
    first_date = first_date,
    last_date = last_date,
    ...
  )

  out$bin_date <- as.integer(out$bin_date)
  attr(out, "interval") <- as.integer(attr(out, "interval"))

  out
}

#' @export
#' @rdname incidence
incidence.numeric <- function(x, date_index, interval = 1L,
                              groups = NULL, na_as_group = TRUE,
                              first_date = NULL, last_date = NULL, ...) {

  # Dots only present for potential future use.
  # This should catch and error if used
  # TODO - check this and later on.exit actions
  #ellipsis::check_dots_used()
  ellipsis::check_dots_empty()

  # change date_index and group to character vectors
  date_index <- arg_values(!!rlang::enexpr(date_index))
  groups <- arg_values(!!rlang::enexpr(groups))

  # make sure input can be used (other checks in make incidence)
  # R 4.0. makes use of a new `stopifnot` format
  stopifnot(
    "The argument `date_index` should be of length one" =
      (length(date_index) == 1),
    "The argument `first_date` should be of length one if not null" =
      (length(first_date) == 1 || is.null(first_date)),
    "The argument `last_date` should be of length one if not null" =
      (length(last_date) == 1 || is.null(last_date)),
    "The argument `na_as_group` must be either `TRUE` or `FALSE`." =
      (is.logical(na_as_group))
  )

  interval <- valid_interval_integer(interval)

  out <- make_incidence(x,
    date_index = date_index,
    interval = interval,
    groups = groups,
    na_as_group = na_as_group,
    first_date = first_date,
    last_date = last_date,
    ...
  )

  out$bin_date <- as.numeric(out$bin_date)
  out
}


#' @export
#' @rdname incidence
incidence.POSIXt <- function(x, date_index, interval = 1L, standard = TRUE,
                             groups = NULL, na_as_group = TRUE,
                             first_date = NULL, last_date = NULL, ...) {

  # Dots only present for potential future use.
  # This should catch and error if used
  # TODO - check this and later on.exit actions
  #ellipsis::check_dots_used()
  ellipsis::check_dots_empty()

  # change date_index and group to character vectors
  date_index <- arg_values(!!rlang::enexpr(date_index))
  groups <- arg_values(!!rlang::enexpr(groups))

  # make sure input can be used (other checks in make incidence)
  # R 4.0. makes use of a new `stopifnot` format
  stopifnot(
    "The argument `date_index` should be of length one" =
      (length(date_index) == 1),
    "The argument `first_date` should be of length one if not null" =
      (length(first_date) == 1 || is.null(first_date)),
    "The argument `last_date` should be of length one if not null" =
      (length(last_date) == 1 || is.null(last_date)),
    "The argument `na_as_group` must be either `TRUE` or `FALSE`." =
      (is.logical(na_as_group))
  )

  dates <- check_dates(as.POSIXct(x[[date_index]]))
  x[[date_index]] <- as.Date(dates)

  out <- make_incidence(x,
    date_index = date_index,
    interval = interval,
    groups = groups,
    na_as_group = na_as_group,
    first_date = first_date,
    last_date = last_date,
    ...
  )

  attr(out, "type") <- "POSIXt"
  out$bin_date <- as.POSIXlt(out$bin_date)
  if (inherits(dates, "POSIXct")) {
    out$bin_date <- as.POSIXct(out$bin_date)
  }
  out
}


group_labels <- function(x, interval, standard) {
  date_var <- get_date_name(x)


  if (check_week(interval) && standard) {
    week_start <- get_week_start(interval)
    x$week_group <- aweek::date2week(x[[date_var]], week_start, floor_day = TRUE)
    attr(x, "date_group") <- "week_group"
    x <- dplyr::relocate(x, .data$week_group, .after = .data[[date_var]])
  }

  date_interval <- is.character(interval) && is_date_interval(interval)
  is_month <- interval == "month" || interval == "1 month" || interval == "1 months"
  is_quarter <- interval == "quarter" || interval == "1 quarter" || interval == "1 quarters"
  is_year <- interval == "year" || interval == "1 year" || interval == "1 years"

  if (date_interval && is_month) {
    x$date_group <- format(x[[date_var]], "%b %y")
    attr(x, "date_group") <- "date_group"
    x <- dplyr::relocate(x, .data$date_group, .after = .data[[date_var]])
  }

  if (date_interval && is_quarter) {
    x$date_group <- paste(quarters(x[[date_var]]), format(x[[date_var]], "%Y"))
    attr(x, "date_group") <- "date_group"
    x <- dplyr::relocate(x, .data$date_group, .after = .data[[date_var]])
  }

  if (date_interval && is_year) {
    x$date_group <- format(x[[date_var]], "%Y")
    attr(x, "date_group") <- "date_group"
    x <- dplyr::relocate(x, .data$date_group, .after = .data[[date_var]])
  }
  x
}



