#' Compute the incidence of events
#'
#' @param x A tibble or a data frame (see Note) representing a linelist.
#' @param date_index The time index of the given data.  This should be a name
#'   corresponding to a date column in x of class:  integer, numeric, Date,
#'   POSIXct, POSIXlt, and character. (See Note  about `numeric` and `character`
#'   formats).
#' @param interval An integer or character indicating the (fixed) size of the
#'   time interval used for computing the incidence; defaults to 1 day. This can
#'   also be a text string that corresponds to a valid date interval:
#'   "yearweek", "yearmonth", "yearquarter" and also "day", "week", "month",
#'   "quarter", or "year". (See Note).
#' @param groups An optional vector giving the names of the groups of
#'   observations for which incidence should be grouped.
#' @param na_as_group A logical value indicating if missing group values (NA)
#'   should treated as a separate category (`TRUE`) or removed from
#'   consideration (`FALSE`).
#' @param count The count variable of the given data.  If NULL (default) the
#'   data is taken to be a linelist of individual observations.
#' @param ... Additional arguments passed to the underlying date grouping
#'   functions when the interval is "yearweek" or one of "day", "week", "month",
#'   "quarter" or "year". In the case of "yearweek" you can optionally specify
#'   an integer value `firstday` representing the day the week should start on;
#'   1 (Monday) to 7 (Sunday).  If no value is specified then the default value
#'   of 1 (Monday) is used which corresponds to the ISO 8601 definition of
#'   weeks. For the other values you can optionally specify a value `firstdate`
#'   which will be passed to the underlying function [`as_period()`].  This
#'   will cause the intervals created to begin on that date.
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
#' \subsection{Input data (`dates`)}{
#'  - **Decimal (numeric) dates**: will be truncated.
#'  - **Character dates** should be in the unambiguous `yyyy-mm-dd` (ISO 8601)
#'   format. Any other format will trigger an error.
#' }
#'
#' \subsection{Interval specification (`interval`)}{
#' [`incidence2`] includes generators for 5 different grouped date s3 classes.
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
#'
#' @export
incidence <- function(x, date_index, groups = NULL, interval = 1L,
                      na_as_group = TRUE, count = NULL, ...) {

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
    "The argument `interval` is not valid." = is_valid_interval(interval),
    "The argument `na_as_group` must be either `TRUE` or `FALSE`." =
      (is.logical(na_as_group))
  )
  if (!is.null(count)) {
    if (length(count) != 0) {
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
    ...
  )

  # make_incidence(
  #   x = x,
  #   date_index = date_index,
  #   groups = groups,
  #   interval = interval,
  #   na_as_group = na_as_group,
  #   count = count,
  #   ...
  # )
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
                            ...) {
  make_incidence(
    x = x,
    date_index = date_index,
    groups = groups,
    interval = interval,
    na_as_group = na_as_group,
    count = count,
    ...
  )
}

incidence_.POSIXt <- incidence_.Date

incidence_.integer <- incidence_.Date

incidence_.numeric <- incidence_.Date

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
#' @param ... Additional arguments passed to the underlying date grouping
#'   functions when the interval is "yearweek" or one of "day", "week", "month",
#'   "quarter" or "year". In the case of "yearweek" you can optionally specify
#'   an integer value `firstday` representing the day the week should start on;
#'   1 (Monday) to 7 (Sunday).  If no value is specified then the default value
#'   of 1 (Monday) is used which corresponds to the ISO 8601 definition of
#'   weeks. For the other values you can optionally specify a value `firstdate`
#'   which will be passed to the underlying function [`as_period()`].  This
#'   will cause the intervals created to begin on that date.
#'
#' @return An incidence2 object.
#'
#' @importFrom dplyr grouped_df summarise n .data
#' @importFrom stats complete.cases na.omit
#' @noRd
make_incidence <- function(x, date_index, groups, interval, na_as_group, count,
                           ...) {



  if (inherits(x[[date_index]], "integer") || inherits(x[[date_index]], "numeric")) {
    x[[date_index]] <- as_int_period(x[[date_index]], interval = interval, ...)
  } else {
    if (is.character(interval) && (get_interval_number(interval) == 1L)) {
      if (interval == "year") {
        x[[date_index]] <- as_yrwk(x[[date_index]], ...)
      } else if (interval == "month") {
        x[[date_index]] <- as_yrmon(x[[date_index]])
      } else if (interval == "quarter") {
        x[[date_index]] <- as_yrqtr(x[[date_index]])
      } else if (interval == "year") {
        x[[date_index]] <- as_yr(x[[date_index]])
      } else {
        x[[date_index]] <- as_period(x[[date_index]], interval = interval, ...)
      }
    } else {
      x[[date_index]] <- as_period(x[[date_index]], interval = interval, ...)
    }
  }

  # Remove the missing observations
  n_orig <- nrow(x)
  x <- x[!is.na(x[[date_index]]), , drop=FALSE]
  n_new <- nrow(x)
  if (n_new < n_orig) {
    message(sprintf("%d missing observations were removed.", n_orig - n_new))
  }

  # set name for date column
  if (interval == 1 || interval == 1L || interval == "1 day" || interval == "1 days") {
    date_col <- "date"
  } else {
    date_col <- "date_index"
  }

  # generate grouped_dates
  x <- grouped_df(x, c(date_index, groups))
  if (is.null(count)) {
    x <- summarise(x, count = n(), .groups = "drop")
  } else {
    x <- summarise(x, count = sum(.data[[count]], na.rm = TRUE), .groups = "drop")
  }
  colnames(x)[1] <- date_col

  # filter out NA groups if desired
  if (!na_as_group) {
    x <- x[complete.cases(x[, groups, drop = FALSE]), , drop = FALSE]
  }

  # reorder (dates, groups, counts)
  x <- x[c(date_col, groups, "count")]

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


