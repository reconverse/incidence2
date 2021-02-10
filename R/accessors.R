#' Access various elements of an incidence object
#'
#' @param x An [incidence()] object.
#' @param ... Not used.
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'   withAutoprint({
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'     i <- incidence(dat,
#'                    date_index = date_of_onset,
#'                    groups = c(gender, hospital))
#'
#'     get_counts(i)
#'     get_counts_name(i)
#'
#'     get_group_names(i)
#'
#'     get_dates(i)
#'     get_dates_name(i)
#'
#'     get_interval(i)
#'
#'     get_n(i)
#'
#'     get_timespan(i)
#'   })
#' }
#'
#' @name accessors
NULL


# -------------------------------------------------------------------------
#' @return
#'   - `get_counts`: The count vector from x.
#' @aliases get_counts
#' @export
#' @rdname accessors
get_counts <- function(x, ...) {
  UseMethod("get_counts")
}

#' @rdname accessors
#' @aliases get_counts.default
#' @export
get_counts.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @rdname accessors
#' @aliases get_counts.incidence2
#' @export
get_counts.incidence2 <- function(x, ...) {
  ellipsis::check_dots_empty()
  x[[attr(x, "count")]]
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' @return
#'   - `get_counts_name()`: The name of the count variable of x.
#' @aliases get_counts_name
#' @export
#' @rdname accessors
get_counts_name <- function(x, ...) {
  UseMethod("get_counts_name")
}

#' @rdname accessors
#' @aliases get_counts_name.default
#' @export
get_counts_name.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @rdname accessors
#' @aliases get_counts_name.incidence2
#' @export
get_counts_name.incidence2 <- function(x, ...) {
  ellipsis::check_dots_empty()
  attr(x, "count")
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' @return
#'   - `get_dates()`: The date vector from x.
#' @rdname accessors
#' @aliases get_dates_name
#' @export
get_dates <- function(x, ...) {
  UseMethod("get_dates")
}

#' @rdname accessors
#' @aliases get_dates.default
#' @export
get_dates.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @rdname accessors
#' @aliases get_dates.incidence2
#' @export
get_dates.incidence2 <- function(x, ...) {
  ellipsis::check_dots_empty()
  x[[attr(x, "date")]]
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' @return
#'   - `get_dates_name()`: The name of the date variable of x.
#' @rdname accessors
#' @aliases get_dates_name
#' @export
get_dates_name <- function(x, ...) {
  UseMethod("get_dates_name")
}

#' @rdname accessors
#' @aliases get_dates_name.default
#' @export
get_dates_name.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @rdname accessors
#' @aliases get_dates_name.incidence2
#' @export
get_dates_name.incidence2 <- function(x, ...) {
  ellipsis::check_dots_empty()
  attr(x, "date")
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' @return
#'   - `get_group_names()`: a character vector of the group variables of x or
#'   NULL if none are present.
#' @rdname accessors
#' @aliases get_group_names
#' @export
get_group_names <- function(x, ...) {
  UseMethod("get_group_names")
}

#' @rdname accessors
#' @aliases get_group_names.default
#' @export
get_group_names.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @rdname accessors
#' @aliases get_group_names.incidence2
#' @export
get_group_names.incidence2 <- function(x, ...) {
  ellipsis::check_dots_empty()
  attr(x, "groups")
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' @return
#'   - `get_timespan()`: an `integer` denoting the timespan in days represented
#'   by the incidence object.
#' @rdname accessors
#' @aliases get_timespan
#' @export
get_timespan <- function(x, ...) {
  UseMethod("get_timespan")
}

#' @rdname accessors
#' @aliases get_timespan.default
#' @export
get_timespan.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @rdname accessors
#' @aliases get_timespan.incidence2
#' @export
get_timespan.incidence2 <- function(x, ...) {
  ellipsis::check_dots_empty()
  date_var <- get_dates_name(x)
  dat <- x[[date_var]]
  r <- range(dat, na.rm = TRUE)
  r <- c(r[1], r[2] + 1)
  if (is.integer(dat)) {
    unclass(r[2]) - unclass(r[1])
  } else {
    r <- as.Date(r)
    as.integer(diff(r))
  }

}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' @return
#'   - `get_n()` The total number of cases stored in the object
#' @export
#' @rdname accessors
#' @aliases get_n
get_n <- function(x) {
  UseMethod("get_n")
}

#' @export
#' @rdname accessors
#' @aliases get_n.default
get_n.default <- function(x) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @export
#' @rdname accessors
#' @aliases get_n.incidence2
get_n.incidence2 <- function(x) {
  count_var <- get_counts_name(x)
  sum(x[[count_var]])
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' @return
#'   - `get_interval()`: if `integer = TRUE`, an integer vector, otherwise the
#'     character value of the `interval`

#' @rdname accessors
#' @aliases get_interval
#' @export
get_interval <- function(x, ...) {
  UseMethod("get_interval")
}

#' @rdname accessors
#' @aliases get_interval.default
#' @export
get_interval.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @param integer When `TRUE`, the interval will be converted to an
#'   integer vector if it is stored as a character in the incidence object.
#' @rdname accessors
#' @aliases get_interval.incidence2
#' @export
get_interval.incidence2 <- function(x, integer = FALSE, ...) {
  ellipsis::check_dots_empty()

  interval <- attr(x, "interval")

  if (!integer || is.numeric(interval)) {
    return(interval)
  }
  dat <- get_dates(x)
  out <- get_interval(dat, days = integer)
  attributes(out) <- NULL
  as.integer(out)
}
# -------------------------------------------------------------------------
