#' @return
#'   - `get_date_group_vars()`: a character vector of the date variables of x.
#' @rdname accessors
#' @aliases get_date_vars
#' @export
get_date_group_vars <- function(x, ...) {
  UseMethod("get_date_group_vars")
}

#' @rdname accessors
#' @aliases get_date_group_vars.default
#' @export
get_date_group_vars.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @rdname accessors
#' @aliases get_date_vars.incidence
#' @export
get_date_group_vars.incidence <- function(x, ...) {
  ellipsis::check_dots_empty()
  attr(x, "date_group")
}
