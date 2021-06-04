#' Compute the incidence of events
#'
#' @param x A data frame representing a linelist (or potentially a
#'   pre-aggregated dataset).
#' @param date_index The time index(es) of the given data.  This should be the
#'   name(s) corresponding to the desired date column(s) in x of class:
#'   integer, numeric, Date, POSIXct, POSIXlt, and character. (See Note  about
#'   `numeric` and `character` formats). Multiple inputs only make sense when
#'   x is a linelist, and in this situation, to avoid ambiguity, the vector must
#'   be named.  These names will be used for the resultant count columns.
#' @param interval An integer or character indicating the (fixed) size of the
#'   time interval used for computing the incidence; defaults to 1 day. This can
#'   also be a text string that corresponds to a valid date interval, e.g.
#'
#'     * (x) day(s)
#'     * (x) weeks(s)
#'     * (x) epiweeks(s)
#'     * (x) isoweeks(s)
#'     * (x) months(s)
#'     * (x) quarter(s)
#'     * (x) years(s)
#'
#'   More details can be found in the "Interval specification" and "Week
#'   intervals" sections below.
#' @param groups An optional vector giving the names of the groups of
#'   observations for which incidence should be grouped.
#' @param na_as_group A logical value indicating if missing group values (NA)
#'   should treated as a separate category (`TRUE`) or removed from
#'   consideration (`FALSE`). Defaults to `TRUE`.
#' @param counts The count variables of the given data.  If NULL (default) the
#'   data is taken to be a linelist of individual observations.
#' @param firstdate When the interval is numeric or in days/months and has a
#'   numeric prefix greater than 1, then you can optionally specify the date
#'   that you wish to anchor your intervals to begin from.  If NULL (default)
#'   then the intervals will start at the minimum value contained in the
#'   `date_index` column. Note that the class of `firstdate` must be `Date` if
#'   the `date_index` column is Date, POSIXct, POSIXlt, or character and integer
#'   otherwise.
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
#'   - **groups**: If specified, column(s) containing the categories of the
#'   given groups.
#'
#'   - **count** (or name of count variables): The aggregated observation counts.
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
#' `incidence()` uses the [`grates`](https://cran.r-project.org/package=grates)
#'   package to generate date groupings. The grouping used depends on the value
#'   of `interval`. This can be specified as either an integer value or a more
#'   standard specification such as "day", "week", "month", "quarter" or "year".
#'   The format in this situation is similar to that used by [`seq.Date()`]
#'   where these values can optionally be preceded by a (positive or negative)
#'   integer and a space, or followed by "s".  When no prefix is given:
#'
#'   - "week"    : uses the "grates_yearweek" class (see [`grates::as_yearweek()`]).
#'   - "month"   : uses the "grates_month" class (see [`grates::as_month()`]).
#'   - "quarter" : uses the "grates_quarter" class (see [`grates::as_quarter()`]).
#'   - "year"    : uses the "grates_year" class (see [`grates::as_year()`]).
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
#'   - interval = "week", (Default, Monday)
#'   - interval = "ISOweek"  (Monday)
#'   - interval = "EPIweek"  (Sunday)
#'   - interval = "MMWRweek" (Sunday)
#'
#' }
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'   withAutoprint({
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'
#'     # daily incidence
#'     incidence(dat, date_of_onset)
#'
#'     # weekly incidence
#'     incidence(dat, date_of_onset, interval = "week")
#'
#'     # starting on a Monday
#'     incidence(dat, date_of_onset, interval = "isoweek")
#'
#'     # starting on a Sunday
#'     incidence(dat, date_of_onset, interval = "epiweek")
#'
#'     # group by gender
#'     incidence(dat, date_of_onset, interval = 7, groups = gender)
#'
#'     # group by gender and hospital
#'     incidence(dat, date_of_onset, interval = "2 weeks", groups = c(gender, hospital))
#'   })
#' }
#'
#' # use of first_date
#' dat <- data.frame(dates = Sys.Date() + sample(-3:10, 10, replace = TRUE))
#' incidence(dat, dates, interval = "week", firstdate = Sys.Date() + 1)
#'
#' @export
incidence <- function(x, date_index, groups = NULL, interval = 1L,
                      na_as_group = TRUE, counts = NULL, firstdate = NULL) {


  en_date <- rlang::enexpr(date_index)
  en_groups <- rlang::enexpr(groups)
  en_counts <- rlang::enexpr(counts)

  # minimal checks (others come later or in nested functions)
  vctrs::vec_assert(interval, size = 1)

  # ensure we have a firstdate value and that we can work with dates
  idx <- tidyselect::eval_select(en_date, x)
  if (is.null(firstdate)) {
    firstdate <- do.call(min, args = c(x[idx], na.rm = TRUE))
  }
  n_orig <- nrow(x)
  x <- x[x[[idx]] >= firstdate, , drop = FALSE]
  n_new <- nrow(x)
  if (n_new < n_orig) message(sprintf("%d observations were removed.", n_orig - n_new))
  x[idx] <- lapply(x[idx], check_dates)

  dat <- build_incidence.data.frame(
    x,
    date_index = !!en_date,
    groups = !!en_groups,
    counts = !!en_counts,
    na_as_group = na_as_group,
    FUN = make_grate,
    args = list(interval = interval, firstdate = firstdate)
  )

  # standardise interval (below is for historical compatibility in incidence2)
  interval <- create_interval_string(dat$date_index)
  attr(dat, "interval") <- interval
  attr(dat, "cumulative") <- FALSE
  class(dat) <- c("incidence2", class(dat))
  dat
}


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

check_dates <- function(x) {
  if (inherits(x, "numeric")) {
    # Attempt to cast to integer and give useful error message if not possible
    tmp <- try(vctrs::vec_cast(x, integer()), silent = TRUE)
    if (inherits(tmp, "try-error")) {
      abort("Where numeric, x[[date_index]] must be a vector of whole numbers")
    }
    x <- tmp
  }  else {
    formats <- c("Date", "POSIXt", "integer", "numeric", "character")
    if (!rlang::inherits_any(x, formats)) {
      abort(c(
        "Unable to convert date_index variable to a grouped date. Accepted formats are:",
        paste(formats, collapse = ", ")
      ))
    }
  }
  x
}

create_interval_string <- function(x) {
  UseMethod("create_interval_string")
}

create_interval_string.default <- function(x) {
  abort(
    sprintf("Not implemented for class %s", paste(class(x), collapse = ", "))
  )
}

create_interval_string.grates_month <- function(x) {
  n <- grates::get_n(x)
  if (n > 1) sprintf("%d months", n) else "1 month"
}

create_interval_string.grates_yearweek <- function(x) {
  firstday <- grates::get_firstday(x)
  weekday <- get_weekday_name(firstday)
  sprintf("1 (%s) week ", weekday)
}

create_interval_string.grates_quarter <- function(x) "1 quarter"

create_interval_string.grates_year <- function(x) "1 year"

create_interval_string.grates_period <- function(x) {
  n <- grates::get_n(x)
  sprintf("%d days", n)
}

create_interval_string.grates_int_period <- function(x) grates::get_n(x)

create_interval_string.numeric <- function(x) 1

create_interval_string.Date <- function(x) "1 day"


