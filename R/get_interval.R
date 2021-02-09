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
  get_interval(dat, days = integer)
}
