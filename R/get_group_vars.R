#' @return
#'   - `get_group_vars()`: a character vector of the group variables of x or
#'   NULL if none are present.
#' @rdname accessors
#' @aliases get_group_vars
#' @export
get_group_vars <- function(x, ...) {
  UseMethod("get_group_vars")
}

#' @rdname accessors
#' @aliases get_group_vars.default
#' @export
get_group_vars.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @rdname accessors
#' @aliases get_group_vars.incidence
#' @export
get_group_vars.incidence <- function(x, ...) {
  ellipsis::check_dots_empty()
  attr(x, "groups")
}
