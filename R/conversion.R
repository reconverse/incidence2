# -------------------------------------------------------------------------
#' Convert incident object to dataframe
#'
#' @param x An [incidence()] object.
#'
#' @param ... Not used.
#'
#' @examples
#' dat <- data.frame(dates = Sys.Date() + 1:100,
#'                   names = rep(c("Jo", "John"), 5))
#'
#' dat <- incidence(dat, date_index = dates, groups = names)
#' as.data.frame(dat)
#'
#' @export
as.data.frame.incidence2 <- function(x, ...) {
  ellipsis::check_dots_empty()
  vctrs::new_data_frame(x)
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' Convert incident2 object to a tibble
#'
#' @param x An [incidence()] object.
#'
#' @param ... Not used.
#'
#' @examples
#' dat <- data.frame(dates = Sys.Date() + 1:100,
#'                   names = rep(c("Jo", "John"), 5))
#'
#' dat <- incidence(dat, date_index = dates, groups = names)
#' as_tibble(dat)
#'
#' @export
#' @name as_tibble
as_tibble.incidence2 <- function(x, ...) {
  ellipsis::check_dots_empty()
  new_bare_tibble(x)
}
# -------------------------------------------------------------------------

#' @importFrom tibble as_tibble
#' @export
#' @name as_tibble
tibble::as_tibble()
# -------------------------------------------------------------------------

#' Convert a data frame to incidence object
#'
#' @param x An object that can be treated as a data frame.
#'
#' @param date_index The time index of the given data in x.  This should be the
#'   name, with or without quotation, corresponding to a date column in x of the
#'   class:  integer, numeric, Date, POSIXct, POSIXlt, and character.
#'
#' @param counts_var The count variable in x.
#'
#' @param group_vars An optional vector giving the names of the groups of
#'   observations for which incidence should be grouped.  This can be given with
#'   or without quotation.
#'
#' @param interval An integer or character indicating the (fixed) size of the
#'   time interval used for computing the incidence; defaults to 1 day. This can
#'   also be a text string that corresponds to a valid date interval: day, week,
#'   month, quarter, or year.
#'
#' @param standard (Only applicable where date_index references a Date object)
#'   When `TRUE` (default) and the `interval` one of "week", "month", "quarter",
#'   or "year", then this will cause the bins for the counts to start at the
#'   beginning of the interval.
#'
#' @param ... Not currently used.
#'
#' @rdname as_incidence
#' @export
as_incidence <- function(x, date_index, counts_var,
                         group_vars = NULL, interval = 1L,
                         standard = TRUE, ...) {

  UseMethod("as_incidence", x)
}


#' @rdname as_incidence
#' @aliases as_incidence.default
#' @export
as_incidence.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}


#' @rdname as_incidence
#' @aliases as_incidence.default
#' @export
as_incidence.data.frame <- function(x, date_index, counts_var,
                                    group_vars = NULL, interval = 1L,
                                    standard = TRUE, ...) {

  # change date_index and group to character vectors
  dates <- arg_values(!!rlang::enexpr(date_index))
  count <- arg_values(!!rlang::enexpr(counts_var))
  groups <- arg_values(!!rlang::enexpr(group_vars))

  stopifnot(
    "The argument `date_index` should be of length one" =
      (length(dates) == 1),
    "The argument `counts_var` should be of length one" =
      (length(count) == 1),
    "The argument `standard` must be either `TRUE` or `FALSE`." =
      (is.logical(standard))
  )

  if (anyDuplicated(x)) {
    stop("Cannot convert a dataframe with duplicated rows into an incidence object")
  }
  dat <- x[c(dates, groups)]

  cnt <- x[[count]]
  cnt[is.na(cnt)] <- 0
  dat <- dat[rep(seq_len(nrow(dat)), cnt), ]
  incidence(dat,
            date_index = !!rlang::enexpr(date_index),
            groups = !!rlang::enexpr(group_vars),
            interval = interval,
            standard = standard)

}
