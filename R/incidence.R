#' Create an incidence object
#'
#' @param x A tibble or a data frame (see Note).
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
#'   or without quotation.
#'
#' @param na_as_group A logical value indicating if missing group (NA) should be
#'   treated as a separate group.
#'
#' @param first_date,last_date optional first/last dates to be used. When
#'   these are `NULL` (default), the dates from the first/last dates are taken
#'   from the observations. If these dates are provided, the observations will be
#'   trimmed to the range of \[first_date, last_date\].
#'
#' @param ... Additional arguments used by other methods.
#'
#' @return An incidence object.
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'   data(ebola_sim_clean, package = "outbreaks")
#'   dat <- ebola_sim_clean$linelist
#'   i1 <- incidence(dat,
#'     date_index = date_of_onset,
#'     interval = 1L,
#'     first_date = "2014-05-20",
#'     last_date = "2014-06-10",
#'     groups = c(hospital, gender)
#'   )
#' }
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

  if (check_week(interval) && standard) {
    # dates are the first days of corresponding ISOweeks.
    week_start <- get_week_start(interval)
    out$weeks <- aweek::date2week(out$date_group, week_start, floor_day = TRUE)
    attr(out, "date") <- c(attr(out, "date"), "weeks")
    out <- dplyr::relocate(out, .data$weeks, .after = .data$date_group)
    if (attr(out$weeks, "week_start") == 1) {
      out$isoweeks <- as.character(out$weeks)
      attr(out, "date") <- c(attr(out, "date"), "isoweeks")
      out <- dplyr::relocate(out, .data$isoweeks, .after = .data$weeks)
    }
  }

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

  if (check_week(interval) && standard) {
    # dates are the first days of corresponding ISOweeks.
    week_start <- get_week_start(interval)
    out$weeks <- aweek::date2week(out$date_group, week_start, floor_day = TRUE)
    attr(out, "date") <- c(attr(out, "date"), "weeks")
    out <- dplyr::relocate(out, .data$weeks, .after = .data$date_group)
    if (attr(out$weeks, "week_start") == 1) {
      out$isoweeks <- as.character(out$weeks)
      attr(out, "date") <- c(attr(out, "date"), "isoweeks")
      out <- dplyr::relocate(out, .data$isoweeks, .after = .data$weeks)
    }
  }

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

  out$date_group <- as.integer(out$date_group)
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

  out$date_group <- as.numeric(out$date_group)
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

  out$date_group <- as.POSIXlt(out$date_group)
  if (inherits(dates, "POSIXct")) {
    out$date_group <- as.POSIXct(out$date_group)
  }
  out
}
