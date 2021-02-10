# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- AS_PERIOD ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' Convert an object to a period
#'
#' @param x `An object to coerce to period.
#' @param firstdate The date to firstdate the intervals from.  If NULL (default) the
#'   earliest date in the vector will be used.
#' @param interval How many days to include in each period.
#' @param ... Not used.
#'
#' @export
as_period <- function(x, interval, firstdate, ...) {
  UseMethod("as_period")
}


#' @rdname as_period
#' @export
as_period.default <- function(x, interval, firstdate, ...) {
  stop(sprintf("Can't convert a <%s> to a <period>" , class(x)[1]), call. = FALSE)
}


#' @rdname as_period
#' @export
as_period.period <- function(x, interval = 1L, firstdate = NULL, ...) {
  x
}


#' @rdname as_period
#' @export
as_period.Date <- function(x, interval = 1L, firstdate = NULL, ...) {

  # truncate days
  x <- trunc(x)

  # ensure we have a firstdate value
  if (is.null(firstdate)) {
    firstdate <- min(x, na.rm = TRUE)
  } else {
    if (!inherits(firstdate, "Date")) {
      stop("`firstdate` should have the same class (Date) as `x`", call. = FALSE)
    }
    if (firstdate > min(x, na.rm = TRUE)) {
      stop("`firstdate` should be at or before the minimum date in `x`", call. = FALSE)
    }
    if (length(firstdate) != 1L) {
      stop(sprintf(
        "Exactly one value should be provided for `interval` (%d provided)",
        length(interval),
        call. = FALSE
      ))
    }
  }

  # Ensure interval is of length one
  if (length(interval) != 1L) {
    stop(sprintf(
      "Exactly one value should be provided for `interval` (%d provided)",
      length(interval),
      call. = FALSE
    ))
  }

  # Ensure numeric intervals are whole numbers
  if (is.numeric(interval)) {
    interval <- int_cast(interval)
    if (interval < 1L) stop("interval must be positive (>= 1)", call. = FALSE)
  }

  # No need to change anything if the interval is 1
  if (interval == 1L || interval == 1 || interval == "1 day" || interval == "1 days") {
    return(x)
  }

  if (is.numeric(interval)) {
    period <- break_dates(x, interval, firstdate)
  } else if (is.character(interval)){

    # First deal with numeric character intervals
    if (!is_valid_date_interval(interval)) {
      suppressWarnings({
        interval <- as.numeric(interval)
      })

      if (is.na(interval)) {
        stop(
          'The interval must be a whole number or one of the following:\n',
          '     "(x) day(s)"\n',
          '     "(x) weeks(s)"\n',
          '     "(x) months(s)"\n',
          '     "(x) quarter(s)"\n',
          '     "(x) years(s)"\n',
          call. = FALSE
        )
      } else {
        period <- break_dates(x, interval, firstdate)
      }
    } else {
      type <- get_interval_type(interval)
      period <- break_dates(x, interval, firstdate)
      if (type == "week") {
        fd <- get_week_start(interval)
        period <- break_dates(x, interval, as.Date(firstdate))
        period <- as.Date(as_yrwk(period, firstday = fd))
      } else if (type == "month") {
        period <- break_dates(x, interval, as.Date(as_yrmon(firstdate)))
        period <- as.Date(as_yrmon(period))
      } else if (type == "quarter") {
        period <- break_dates(x, interval, as.Date(as_yrqtr(firstdate)))
        period <- as.Date(as_yrqtr(period))
      } else if (type == "year") {
        period <- break_dates(x, interval, as.Date(as_yr(firstdate)))
        period <- as.Date(as_yr(period))
      }
    }
  } else {
    stop(
      "`interval` not valid.  See `?as_period` for valid intervals",
      call. = FALSE
    )
  }

  # create class
  period <- new_period(
    unclass(period),
    firstdate = unclass(firstdate),
    interval = interval
  )

  # finishing touches
  period[is.na(x)] <- NA_real_
  names(period) <- names(x)
  period
}


#' @rdname as_period
#' @export
as_period.POSIXt <- function(x, interval = 1L, firstdate = NULL, ...) {

  # convert to date
  if (is.null(firstdate)) {
    firstdate <- as.POSIXlt(min(x, na.rm = TRUE))
  } else {
    if(!inherits(firstdate, "POSIXt")) {
      stop("`firstdate` should have the same class (POSIXt) as `x`", call. = FALSE)
    }
    if (firstdate > min(x, na.rm = TRUE)) {
      stop("`firstdate` should be at or before the minimum date in `x`", call. = FALSE)
    }
  }
  firstdate <- as.Date(firstdate, tz = tzone(firstdate))

  x <- as.POSIXlt(x)
  out <- as.Date(x, tz = tzone(x))
  out <- as_period.Date(out, firstdate = firstdate, interval = interval)

  # finishing touches
  out[is.na(x)] <- NA_real_
  names(out) <- names(x)
  out
}


#' @rdname as_period
#' @export
as_period.character <- function(x, interval = 1L, firstdate = NULL, ...) {

  # ISO 8601 standard (YYYY-MM-DD)
  iso_pattern <- "(^\\d{4}-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[0-1])$)"


  # iso_pattern is allowed, as are NA's
  allowed <- grepl(iso_pattern, trimws(x))
  allowed[is.na(x)] <- TRUE
  if (!all(allowed)) {
    stop(
      "Not all dates are in a valid formate:",
      sprintf("The first incorrect date is: %s", x[!allowed][1]),
      call. = FALSE
    )
  }

  # remove extraneous whitespace
  dat <- trimws(x)

  if (!is.null(firstdate)) {
    if (!(grepl(iso_pattern, firstdate) || is.Date(firstdate) || is.null(firstdate))) {
      stop(
        "`firstdate` must a character vector convertible to Date or a Date object",
        call. = FALSE
      )
    }

    if (grepl(iso_pattern, firstdate)) {
      firstdate <- as.Date(firstdate)
    }
  }

  # convert to dates
  dat <- as.Date(dat)

  # convert to period
  dat <- as_period.Date(dat, interval = interval, firstdate = firstdate)
  names(dat) <- names(x)
  dat
}


#' @rdname as_period
#' @export
as_period.factor <- function(x, interval = 1L, firstdate = NULL, ...) {
  as_period.character(as.character(x), firstdate = NULL, interval = 1L, ...)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------- FORMATING / PRINTING -------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
format.period <- function(x, ...) {
  if (length(x) == 0) return(character(0))
  sprintf("[%s", format.Date(new_date(x)))
}

#' @export
print.period <- function(x, ...) {
  firstdate <- attr(x, "firstdate")
  interval <- attr(x, "interval")
  if (is.integer(interval)) {
    interval <- sprintf("%d days", interval)
  }
  firstdate <- new_date(firstdate)
  cat(sprintf("<period> firstdate = %s, interval = %s\n", firstdate, interval))
  print(format.period(x, ...))
  invisible(x)
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------- METHODS: CONVERSIONS FROM period -------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
as.POSIXct.period <- function(x, tz = "UTC", ...) {
  if (tz == "UTC") {
    as_utc_posixct_from_int(x)
  } else {
    as_zoned_posixct_from_int(x, tz = tz)
  }
}


#' @export
as.POSIXlt.period <- function(x, tz = "UTC", ...) {
  if (tz == "UTC") {
    as_utc_posixlt_from_int(x)
  } else {
    as_zoned_posixlt_from_int(x, tz = tz)
  }
}


#' @export
as.Date.period <- function(x, ...) {
  attributes(x) <- NULL
  new_date(x)
}


#' @export
as.character.period <- function(x, ...) format(x, ...)


#' @export
as.list.period <- function(x, ...) {
  dat <- unclass(x)
  fd <- attr(x, "firstdate")
  dur <- attr(x, "interval")
  lapply(dat, new_period, interval = dur, firstdate = fd)
}

#' @export
as.numeric.period <- function(x, ...) {
  attributes(x) <- NULL
  x
}


# This code is the same as that of the as.data.frame.yearmon code in Zoo by
# Achim Zeileis et al.
#' @export
as.data.frame.period <- function(x, row.names = NULL, optional = FALSE, ...) {
  nrows <- length(x)
  nm <- paste(deparse(substitute(x), width.cutoff = 500), collapse = " ")
  if (is.null(row.names)) {
    if (nrows == 0)
      row.names <- character(0)
    else if(length(row.names <- names(x)) == nrows && !any(duplicated(row.names))) {
    }
    else if(optional) row.names <- character(nrows)
    else row.names <- seq_len(nrows)
  }
  names(x) <- NULL
  value <- list(x)
  if(!optional) names(value) <- nm
  attr(value, "row.names") <- row.names
  class(value) <- "data.frame"
  value
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------ METHODS: MISCELLANEOUS ------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
is.numeric.period <- function(x) FALSE


#' @export
`[.period` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  firstdate <- attr(x, "firstdate")
  interval <- attr(x, "interval")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "firstdate") <- firstdate
  attr(val, "interval") <- interval
  val
}


#' @export
`[[.period` <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  firstdate <- attr(x, "firstdate")
  interval <- attr(x, "interval")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "firstdate") <- firstdate
  attr(val, "interval") <- interval
  val
}


#' @export
`[<-.period` <- function(x, i, value) {
  cl <- oldClass(x)
  firstdate <- attr(x, "firstdate")
  interval <- attr(x, "interval")
  if (!all(inherits(value, "period") | is.na(value))) {
    stop("Can only assign period objects in to a period object", call. = FALSE)
  }
  val <- NextMethod("[<-")
  class(val) <- cl
  attr(val, "firstdate") <- firstdate
  attr(val, "interval") <- interval
  val
}


#' @export
rep.period <- function (x, ..., drop = TRUE) {
  cl <- oldClass(x)
  firstdate <- attr(x, "firstdate")
  interval <- attr(x, "interval")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "firstdate") <- firstdate
  attr(val, "interval") <- interval
  val
}


#' @export
unique.period <- function (x, incomparables = FALSE, ...) {
  cl <- oldClass(x)
  firstdate <- attr(x, "firstdate")
  interval <- attr(x, "interval")
  class(x) <- NULL
  val <- NextMethod()
  class(val) <- cl
  attr(val, "firstdate") <- firstdate
  attr(val, "interval") <- interval
  val
}


#' @export
c.period <- function(..., recursive = FALSE, use.names = TRUE) {
  dots <- list(...)

  if (!all(vapply(dots, inherits, logical(1), what = "period") | is.na(dots))) {
    stop(
      "To combine <period> objects with different objects first convert to a common class",
      call. = FALSE
    )
  }

  firstdate <- attr(dots[[1]], "firstdate")
  firstdates <- lapply(dots, attr, numeric(1), which = "firstdate")
  if (!all(vapply(firstdates, function(x) {is.null(x) || x == firstdate}, logical(1)))) {
    stop(
      "Unable to combine <period> objects with different `firstdate` attributes",
      call. = FALSE
    )
  }

  interval <- attr(dots[[1]], "interval")
  intervals <- lapply(dots, attr, numeric(1), which = "interval")
  if (!all(vapply(intervals, function(x) {is.null(x) || x == interval}, logical(1)))) {
    stop(
      "Unable to combine <period> objects with different `interval` attributes",
      call. = FALSE
    )
  }

  res <- NextMethod()
  class(res) <- c("period", "grate")
  attr(res, "firstdate") <- firstdate
  attr(res, "interval") <- interval
  res
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# --------------------------------- MATHS --------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Math.period <- function(x, ...) {
  .fn <- .Generic
  fn <- switch(
    .fn,
    is.nan = is.nan.period(x),
    is.finite = is.finite.period(x),
    is.infinite = is.infinite.period(x),
    stop(sprintf("`%s()` is not supported for <period>", .fn), call. = FALSE)
  )
}

is.nan.period <- function(x, ...) vector("logical", length(x))

is.finite.period <- function(x, ...) !is.na(unclass(x))

is.infinite.period <- function(x, ...) vector("logical", length(x))


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ---------------------------------- OPS ---------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Ops.period <- function(e1, e2) {
  op <- .Generic
  if (op %in% c("==", "!=", "<", ">", "<=", ">=")) {
    if (inherits(e2, "period")) {
      return(NextMethod())
    } else {
      stop("Can only compare <period> objects with <period> objects", call. = FALSE)
    }
  }

  switch(
    op,
    "+" = {
      if (missing(e2)) {
        return(e1)
      } else if (inherits(e1, "period") && inherits(e2, "period")) {
        stop("Cannot add <period> objects to each other", call. = FALSE)
      } else if (inherits(e1, "period") && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        add_periods(e1, unclass(e2))
      } else if (inherits(e2, "period") && (all(is.wholenumber(unclass(e1)),  na.rm = TRUE))) {
        add_periods(e2, unclass(e1))
      } else {
        stop("Can only add whole numbers to <period> objects", call. = FALSE)
      }
    },
    "-" = {
      if (missing(e2)) {
        stop("Cannot negate a <period> object", call. = FALSE)
      } else if (inherits(e1, "period") && !(inherits(e2, "period")) && (all(is.wholenumber(unclass(e2)), na.rm = TRUE))) {
        add_periods(e1, -unclass(e2))
      } else {
        stop("Can only subtract whole numbers from <period> objects", call. = FALSE)
      }
    },
    stop(sprintf("%s is not compatible with <period> objects", op), call. = FALSE)
  )
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- SUMMARY -------------------------------- #
# ------------------------------------------------------------------------- #
# ---- THE FOLLOWING IS BASED ON THE FUNCTION IN ZOO BY ACHIM ZEILEIS ----- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

#' @export
Summary.period <- function (..., na.rm)
{
  ok <- switch(.Generic, max = TRUE, min = TRUE, range = TRUE, FALSE)
  if (!ok) stop(.Generic, " not defined for period objects")
  firstdate <- attr(list(...)[[1]], "firstdate")
  interval <- attr(list(...)[[1]], "interval")
  val <- NextMethod(.Generic)
  class(val) <- oldClass(list(...)[[1]])
  attr(val, "firstdate") <- firstdate
  attr(val, "interval") <- interval
  val
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- INTERNALS ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

new_period <- function(x = numeric(), firstdate = new_date(), interval = integer()) {
  structure(x, firstdate = firstdate, interval = interval, class = c("period", "grate"))
}


add_periods <- function(x, n) {
  out <- unclass(x)
  d <- attr(x, "interval")
  if (is.integer(d)) {
    out <- out + (n * d)
  } else {
    dn <- get_interval_number(d) * n
    dt <- get_interval_type(d)
    by = paste(dn, dt)
    out <- vapply(
      new_date(out),
      function(x) seq.Date(x, by = by, length.out = 2)[2],
      double(1)
    )
  }

  start <- min(attr(x, "firstdate"), min(out))
  new_period(out, interval = d, firstdate = start)
}


break_dates <- function(x, interval, firstdate) {
  breaks <- seq(from = firstdate, to = max(x, na.rm = TRUE), by = interval)
  period <- cut(x, breaks = c(breaks, Inf), labels = FALSE)
  breaks[period]
}

#' Is the interval a valid date character
#'
#' @param interval A interval string.
#'
#' @return a logical value
#' @noRd
is_valid_date_interval <- function(interval) {
  valid_intervals <- "day|week|month|quarter|year$"
  grepl(valid_intervals, interval, ignore.case = TRUE)
}
