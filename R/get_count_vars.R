#' @return
#'   - `get_count_vars()`: The count variable of x.
#' @rdname accessors
#' @aliases get_count_vars
#' @export
get_count_vars <- function(x, ...) {
  UseMethod("get_count_vars")
}

#' @rdname accessors
#' @aliases get_count_vars.default
#' @export
get_count_vars.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @rdname accessors
#' @aliases get_count_vars.incidence
#' @export
get_count_vars.incidence <- function(x, ...) {
  ellipsis::check_dots_empty()
  attr(x, "count")
}
