#' Compute the incidence of events
#'
#' @param x A tibble or a data frame (see Note) representing a linelist.
#' @param date_index The time index(es) of the given data.  This should be the
#'   name(s) corresponding to the desired date column(s) in x of class:
#'   integer, numeric, Date, POSIXct, POSIXlt, and character. (See Note  about
#'   `numeric` and `character` formats). Where the input is a linelist, if a
#'   named vector is given then the names will be used for the resultant count
#'   columns.  Named vectors must be used when the number of time indexes is
#'   2 or more.
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
#' @param counts The count variables of the given data.  If NULL (default) the
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

  # tidyselect is used so we can rely on that for dealing with a lot of the
  # non-standard evaluation issues that arrise.  We could just use
  # `rlang::enquo()` in combination with the `return_args()` function to achieve
  # the same thing but there are then more things that can catch us out. I've
  # been forced to adopt this approach for date_index below as we are allowing
  # the names of the date_index vector to be used later on.  This may be
  # overly complex so may eventually change

  # Convert groups to character variables - us
  groups <- rlang::enquo(groups)
  idx <- tidyselect::eval_select(groups, x)
  groups <- names(x)[idx]
  if (length(groups) == 0) groups <- NULL

  date_index <- rlang::enquo(date_index)
  date_expr <- rlang::quo_get_expr(date_index)
  if (rlang::quo_is_call(date_index)) {
    # here we deal with interactive input of the form
    # date_index = c(nm1 = date_of_onset, nm2 = date_of_indection)
    date_arg_names <- return_args_names(!!date_expr)
  } else {
    # here we deal with input passed non interactively with !!, e.g.
    # var = c(nm1 = "date_of_onset", nm2 = "date_of_indection")
    # f <- function(date_index = !!var)
    date_arg_names <- names(date_expr)
  }
  idx <- tidyselect::eval_select(date_index, x)
  date_index <- names(x)[idx]

  counts <- rlang::enquo(counts)
  idx <- tidyselect::eval_select(counts, x)
  counts <- names(x)[idx]
  if (length(counts) == 0) counts <- NULL

  # work out the names for count variables
  count_names <- date_arg_names
  if (is.null(counts)) {
    if (length(date_index) == 1) {
      if (is.null(count_names)) count_names <- "count"
    } else {
      if (is.null(count_names) || length(count_names) != length(date_index)) {
        stop( "'date_index' must be a named vector", call. = FALSE)
      }
    }
  }

  # minimal checks
  stopifnot(
    "The argument `interval` should be of length one." = (length(interval) == 1),
    "The argument `na_as_group` must be either `TRUE` or `FALSE`." =
      (is.logical(na_as_group))
  )

  # ensure all date_index are of same class
  date_classes <- vapply(x[date_index], function(x) class(x)[1], character(1))
  if (length(unique(date_classes)) != 1L) {
    stop("date_index columns must be of the same class", call. = FALSE)
  }

  # ensure we have a firstdate value
  if (is.null(firstdate)) {
    firstdate <- do.call(min, args = c(x[date_index], na.rm = TRUE))
  }

  # ensure we can work with dates (done here mainly for error message)
  x[date_index] <- lapply(x[date_index], standardise_dates)

  # Calculate an incidence object for each value of date_index
  res <-
    lapply(
      seq_along(date_index),
      function(i) {
        make_incidence(
          x = x,
          date_index = date_index[i],
          groups = groups,
          interval = interval,
          na_as_group = na_as_group,
          counts = counts,
          count_name = count_names[i],
          firstdate = firstdate
        )
      }
    )

  # if there is only 1 value for date_index we can just return the entry.
  # Otherwise we need to set the relevant names and count_names attributes (but
  # can take other attributes from any entry of the returned list).
  # TODO - can we streamline this
  if (length(date_index) == 1) {
    res <- res[[1]]
  } else {
    res_attributes <- attributes(res[[1]])
    res_attributes$counts <- count_names
    nms <- lapply(res, names)
    nms <- Reduce(union, nms)
    res_attributes$names <- nms
    res <- lapply(res, setDT)
    by_var <- c("date_index", groups)
    res <- do.call(merge, args = c(res, all = TRUE, by = by_var))
    setnafill(res, fill = 0, cols = count_names)
    setDF(res)
    res_row_names <- attr(res,"row.names")
    attributes(res) <- res_attributes
    attr(res, "row.names") <- res_row_names
  }

  res
}


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
#' @param counts The count variables of the given data.  If NULL (default) the
#'   data is taken to be a linelist of individual observations.
#' @param count_name The names to give the resultant count variable.  Only
#'   used when counts = NULL.
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
make_incidence <- function(x, date_index, groups, interval, na_as_group, counts,
                           count_name, firstdate) {

  # due to NSE notes in R CMD check
  . <- ..counts <- NULL

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
  if (is.null(counts)) {
    x <- x[,.(count__ = .N), keyby = c(date_index, groups)]
    setnames(x, length(x), count_name)
  } else {
    x <- x[, lapply(.SD, sum, na.rm = TRUE), keyby = c(date_index, groups), .SDcols = counts]
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
  if (is.null(counts)) {
    counts <- count_name
  }
  x <- x[c(date_col, groups, counts)]

  # standardise interval
  if (!is.integer(x[[date_col]])) {
    interval <- standardise_interval(interval)
  }

  # create subclass of tibble
  tbl <- tibble::new_tibble(
    x,
    groups = groups,
    date = date_col,
    counts = counts,
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


standardise_dates <- function(x) {
  if (inherits(x, "numeric")) {
    # Attempt to cast to integer and give useful error message if not possible
    tmp <- try(int_cast(x), silent = TRUE)
    if (inherits(tmp, "try-error")) {
      stop(
        "Where numeric, x[[date_index]] must be a vector of whole numbers",
        call. = FALSE
      )
    }
    x <- tmp
  }  else {
    formats <- c("Date", "POSIXt", "integer", "numeric", "character")
    if (!rlang::inherits_any(x, formats)) {
      stop(
        "Unable to convert date_index variable to a grouped date.\n",
        "    Accepted formats are: ",
        paste(formats, collapse = ", "),
        call. = FALSE
      )
    }
  }
  x
}
