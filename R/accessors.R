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
#' @aliases get_counts.incidence
#' @export
get_counts.incidence <- function(x, ...) {
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
#' @aliases get_counts_name.incidence
#' @export
get_counts_name.incidence <- function(x, ...) {
  ellipsis::check_dots_empty()
  attr(x, "count")
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' @return
#'   - `get_date_group_names()`: The names of the date group variables of x.
#' @rdname accessors
#' @aliases get_dates_name
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
#' @aliases get_dates_name.incidence
#' @export
get_date_group_names.incidence <- function(x, ...) {
  ellipsis::check_dots_empty()
  attr(x, "date_group")
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
get_datese.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @rdname accessors
#' @aliases get_dates.incidence
#' @export
get_dates.incidence <- function(x, ...) {
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
#' @aliases get_dates_name.incidence
#' @export
get_dates_name.incidence <- function(x, ...) {
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
