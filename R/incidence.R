#' Compute the incidence of events
#'
#' @param x A tibble or a data frame (see Note) representing a linelist.
#' @param date_index The time index of the given data.  This should be a name
#'   corresponding to a date column in x of class:  integer, numeric, Date,
#'   POSIXct, POSIXlt, and character. (See Note  about `numeric` and `character`
#'   formats).
#' @param interval An integer or character indicating the (fixed) size of the
#'   time interval used for computing the incidence; defaults to 1 day. This can
#'   also be a text string that corresponds to a valid date interval, e.g.
#'     * (x) day(s)
#'     * (x) weeks(s)
#'     * (x) epiweeks(s)
#'     * (x) isoweeks(s)
#'     * (x) months(s)
#'     * (x) quarter(s)
#'     * (x) years(s)
#'   More details can be found in the "Interval specification" and "Week
#'   intervals" sections below.
#' @param groups An optional vector giving the names of the groups of
#'   observations for which incidence should be grouped.
#' @param na_as_group A logical value indicating if missing group values (NA)
#'   should treated as a separate category (`TRUE`) or removed from
#'   consideration (`FALSE`).
#' @param count The count variable of the given data.  If NULL (default) the
#'   data is taken to be a linelist of individual observations.
#' @param firstdate When the interval is in days, or numeric, and also has a
#'   numeric prefix greater than 1, then you can optionally specify the date
#'   that you wish your intervals to begin from.  If NULL (default) then the
#'   intervals will start at the minimum value contained in the date_index
#'   column.
#'
#' @return An incidence2 object.  This is a subclass of tibble that represents
#'   and aggregated count of observations grouped according to the specified
#'   interval and, optionally, the given groups.  By default it will contain the
#'   following columns:
#'
#'   - **date** / **date_index**:  If the default interval of 1 day is used then
#'     this will be the dates of the given observations and given the name
#'     "date", otherwise, this will be values obtained from the specified date
#'     grouping with column name "date_index" (See Interval specification below).
#'
#'   - **-groups-**: If specified, column(s) containing the categories of the
#'   given groups.
#'
#'   - **count**: The aggregated observation count.
#'
#' @note
#'
#' \subsection{Input data (`date_index`)}{
#'  - **Decimal (numeric) dates**: will be truncated.
#'  - **Character dates** should be in the unambiguous `yyyy-mm-dd` (ISO 8601)
#'   format. Any other format will trigger an error.
#' }
#'
#' \subsection{Interval specification (`interval`)}{
#' `incidence2` includes generators for different grouped date s3 classes.
#'   Which one the `incidence()` function uses depends on the value of
#'   `interval`. This can be specified as either an integer value or a more
#'   standard specification such as "day", "week", "month", "quarter" or "year".
#'   The format in this situation is similar to that used by [`seq.Date()`]
#'   where these values can optionally be preceded by a (positive or negative)
#'   integer and a space, or followed by "s".  When no prefix is given:
#'
#'   - "week"    : uses the "yrwk" class (see [`as_yrwk()`]).
#'   - "month"   : uses the "yrmon" class (see [`as_yrmon()`]).
#'   - "quarter" : uses the "yrqtr" class (see [`as_yrqtr()`]).
#'   - "year"    : uses the "yr" class (see [`as_yr()`]).
#'
#'   When a prefix is provided (e.g. 2 weeks) the output is an object of class
#'   "period" (see [`as_period()`]).  Note that for the values "month",
#'   "quarter" and "year" intervals are always chosen to start at the beginning
#'   of the calendar equivalent.  If the input is an integer value the input is
#'   treated as if it was specified in days (i.e. 2 and 2 days) produce the
#'   same output.
#'
#'   The only interval values that do not produce these grouped classes are 1,
#'   1L, "day" or "days" (both without prefix) are used.  In this situation the
#'   returned object is of the standard "Date" class.
#' }
#'
#' \subsection{Week intervals}{
#'
#' It is possible to construct incidence objects standardized to any day of the
#' week. The default state is to use ISO 8601 definition of weeks, which start
#' on Monday. You can specify the day of the week an incidence object should
#' be standardised to by using the pattern "{n} {W} weeks" where "{W}"
#' represents the weekday in an English or current locale and "{n}" represents
#' the duration, but this can be ommitted.  Below are examples of specifying
#' weeks starting on different days assuming we had data that started on
#' 2016-09-05, which is ISO week 36 of 2016:
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
#' }
#'
#'
#' @export
incidence <- function(x, date_index, groups = NULL, interval = 1L,
                      na_as_group = TRUE, count = NULL, firstdate = NULL) {

  # Convert groups, date and count variables
  groups <- rlang::enquo(groups)
  idx <- tidyselect::eval_select(groups, x)
  groups <- names(x)[idx]
  if (length(groups) == 0) groups <- NULL

  date_index <- rlang::enquo(date_index)
  idx <- tidyselect::eval_select(date_index, x)
  date_index <- names(x)[idx]

  count <- rlang::enquo(count)
  idx <- tidyselect::eval_select(count, x)
  count <- names(x)[idx]
  if (length(count) == 0) count <- NULL

  # Basic checks
  stopifnot(
    "The argument `date_index` should be of length one." = (length(date_index) == 1),
    "The argument `interval` should be of length one." = (length(interval) == 1),
    "The argument `na_as_group` must be either `TRUE` or `FALSE`." =
      (is.logical(na_as_group))
  )
  if (!is.null(count)) {
    if (length(count) != 1) {
      stop(
        "The argument `count` should be either NULL or of length one.",
        call. = FALSE
      )
    }
  }

  # check that variables are present in x
  check_presence(c(groups, date_index, count), column_names = names(x))

  incidence_(
    x = x,
    date_index = date_index,
    groups = groups,
    interval = interval,
    na_as_group = na_as_group,
    count = count,
    firstdate = firstdate
  )

}


incidence_ <- function(x, date_index, ...) {
  UseMethod("incidence_", x[[date_index]])
}


incidence_.default <- function(x, date_index, ...) {
  formats <- c("Date", "POSIXt", "integer", "numeric", "character")
  stop(
    "Unable to convert date_index variable to a grouped date.\n",
    "    Accepted formats are: ",
    paste(formats, collapse = ", "),
    call. = FALSE
  )
}

incidence_.Date <- function(x, date_index, groups, interval, na_as_group, count,
                            firstdate = firstdate, ...) {
  make_incidence(
    x = x,
    date_index = date_index,
    groups = groups,
    interval = interval,
    na_as_group = na_as_group,
    count = count,
    firstdate = firstdate
  )
}

incidence_.POSIXt <- incidence_.Date

incidence_.integer <- incidence_.Date

incidence_.numeric <- function(x, date_index, groups, interval, na_as_group, count,
                               firstdate = firstdate, ...) {

  # Attempt to cast to integer and give useful error message if not possible
  tmp <- try(int_cast(x[[date_index]]), silent = TRUE)
  if (inherits(tmp, "try-error")) {
    stop(
      "Where numeric, x[[date_index]] must be a vector of whole numbers",
      call. = FALSE
    )
  }
  x[[date_index]] <- tmp

  incidence_.integer(x, date_index, groups, interval, na_as_group, count,
                     firstdate = firstdate, ...)
}

incidence_.character <- incidence_.Date



#' Default internal constructor for incidence objects.
#'
#' @param x A tibble.
#' @param date_index The time index of the given data.  This should be the name
#'   corresponding to a date column in x.
#' @param interval An integer or character indicating the (fixed) size of the
#'   time interval used for computing the incidence; defaults to 1 day.
#' @param groups An optional character vector defining groups of observations
#'   for which incidence should be computed separately.
#' @param na_as_group A logical value indicating if missing group (NA) should be
#'   treated as a separate group.
#' @param count The count variable of the given data.  If NULL (default) the
#'   data is taken to be a linelist of individual observations.
#' @param firstdate When the interval is in days, or numeric, and also has a
#'   numeric prefix greater than 1, then you can optionally specify the date
#'   that you wish your intervals to begin from.  If NULL (default) then the
#'   intervals will start at the minimum value contained in the date_index
#'   column.
#'
#' @return An incidence2 object.
#'
#' @import data.table
#' @importFrom stats complete.cases na.omit
#' @noRd
make_incidence <- function(x, date_index, groups, interval, na_as_group, count,
                           firstdate) {

  # due to NSE notes in R CMD check
  ..count_var <- . <- ..count <- NULL

  # ensure we have a firstdate value
  if (is.null(firstdate)) {
    firstdate <- min(x[[date_index]], na.rm = TRUE)
  }

  # trim observations
  n_orig <- nrow(x)
  x <- x[x[[date_index]] >= firstdate, , drop = FALSE]
  n_new <- nrow(x)
  if (n_new < n_orig) {
    message(sprintf("%d observations were removed.", n_orig - n_new))
  }

  # Group the dates
  x[[date_index]] <- make_grate(
    x[[date_index]],
    interval = interval,
    firstdate = firstdate
  )

  # Remove the missing observations
  n_orig <- nrow(x)
  x <- x[!is.na(x[[date_index]]), , drop=FALSE]
  n_new <- nrow(x)
  if (n_new < n_orig) {
    message(sprintf("%d missing observations were removed.", n_orig - n_new))
  }

  # generate grouped_dates
  setDT(x)
  if (is.null(count)) {
    x <- x[,.(count = .N), keyby = c(date_index, groups)]
  } else {
    x <- x[,.(count = sum(get(..count), na.rm = TRUE)), keyby = c(date_index, groups)]
  }
  setDF(x)

  # set name for date column
  date_col <- "date_index"

  # give date column correct name
  colnames(x)[1] <- date_col

  # filter out NA groups if desired
  if (!na_as_group) {
    x <- x[complete.cases(x[, groups, drop = FALSE]), , drop = FALSE]
  }

  # reorder (dates, groups, counts)
  x <- x[c(date_col, groups, "count")]

  # standardise interval
  if (!is.integer(x[[date_col]])) {
    interval <- standardise_interval(interval)
  }

  # create subclass of tibble
  tbl <- tibble::new_tibble(
    x,
    groups = groups,
    date = date_col,
    count = "count",
    interval = interval,
    cumulative = FALSE,
    nrow = nrow(x),
    class = "incidence2"
  )

  tibble::validate_tibble(tbl)

}

standardise_interval <- function(interval) {

  cond1 <- interval == 1L
  cond2 <- get_interval_type(interval) == "day"
  cond3 <- as.character(get_interval_number(interval)) == "1"

  if (cond1 || (cond2 && cond3)) {
    interval = "1 day"
  } else if (is.integer(interval) || is.numeric(interval)) {
    interval <- sprintf("%d days", interval)
  } else if (is.character(interval)) {
    type <- get_interval_type(interval)
    n <- get_interval_number(interval)
    if (type == "week") {
      wd <- get_week_start(interval, numeric = FALSE)
      interval <- sprintf("%d %s %s", n, wd, type)
    } else {
      interval <- sprintf("%d %s", n, type)
    }
    if (n > 1) interval <- paste0(interval, "s")
  }
  interval
}

