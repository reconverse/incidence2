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
  as.integer(diff(range(x[[date_var]], na.rm = TRUE)) + 1)
}
