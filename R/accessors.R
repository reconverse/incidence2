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
#'     get_count_name(i)
#'
#'     get_group_names(i)
#'
#'     get_date_name(i)
#'
#'     get_date_group_names(i)
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
#'   - `get_count_name()`: The count variable of x.
#' @aliases get_count_name
#' @export
#' @rdname accessors
get_count_name <- function(x, ...) {
  UseMethod("get_count_name")
}

#' @rdname accessors
#' @aliases get_count_name.default
#' @export
get_count_name.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @rdname accessors
#' @aliases get_count_name.incidence
#' @export
get_count_name.incidence <- function(x, ...) {
  ellipsis::check_dots_empty()
  attr(x, "count")
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' @return
#'   - `get_date_group_names()`: a character vector of the date variables of x.
#' @rdname accessors
#' @aliases get_date_name
#' @export
get_date_group_names <- function(x, ...) {
  UseMethod("get_date_group_names")
}

#' @rdname accessors
#' @aliases get_date_group_names.default
#' @export
get_date_group_names.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @rdname accessors
#' @aliases get_date_name.incidence
#' @export
get_date_group_names.incidence <- function(x, ...) {
  ellipsis::check_dots_empty()
  attr(x, "date_group")
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' @return
#'   - `get_date_name()`: a character vector of the date variables of x.
#' @rdname accessors
#' @aliases get_date_name
#' @export
get_date_name <- function(x, ...) {
  UseMethod("get_date_name")
}

#' @rdname accessors
#' @aliases get_date_name.default
#' @export
get_date_name.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @rdname accessors
#' @aliases get_date_name.incidence
#' @export
get_date_name.incidence <- function(x, ...) {
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
#' @aliases get_group_names.incidence
#' @export
get_group_names.incidence <- function(x, ...) {
  ellipsis::check_dots_empty()
  attr(x, "groups")
}
# -------------------------------------------------------------------------
