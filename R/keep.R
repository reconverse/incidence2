#' Keep first and last occurences
#'
#' `keep_first()` (`keep_last`) keeps the first (last) `n` entries to occur
#'   by date ordering.
#'
#' @param x Object to filter.
#' @param n Number of entries to keep.
#' @param ... Not currently used.
#'
#' @return The objected with the chosen entries.
#' @name keep
NULL
# -------------------------------------------------------------------------

#' @rdname keep
#' @export
keep_first <- function(x, n, ...) {
  UseMethod("keep_first")
}


#' @rdname keep
#' @export
keep_first.default <- function(x, n, ...) {
  abort(
    sprintf("Not implemented for class %s", paste(class(x), collapse = ", "))
  )
}


#' @rdname keep
#' @export
keep_first.incidence2 <- function(x, n, ...) {
  dat <- get_dates(x)
  idx <- keep_idx(dat, n = n)
  x[idx, ]
}


#' @rdname keep
#' @export
keep_first.grate_yearweek <- function(x, n, ...) {
  idx <- keep_idx(x, n = n)
  x[idx]
}


#' @rdname keep
#' @export
keep_first.grate_month <- keep_first.grate_yearweek


#' @rdname keep
#' @export
keep_first.grate_quarter <- keep_first.grate_yearweek


#' @rdname keep
#' @export
keep_first.grate_year <- keep_first.grate_yearweek


#' @rdname keep
#' @export
keep_first.grate_period <- keep_first.grate_yearweek


# -------------------------------------------------------------------------


#' @rdname keep
#' @export
keep_last <- function(x, n, ...) {
  UseMethod("keep_last")
}


#' @rdname keep
#' @export
keep_last.default <- function(x, n, ...) {
  abort(
    sprintf("Not implemented for class %s", paste(class(x), collapse = ", "))
  )
}


#' @rdname keep
#' @export
keep_last.incidence2 <- function(x, n, ...) {
  dat <- get_dates(x)
  idx <- keep_idx(dat, n = n, from_last = TRUE)
  x[idx, ]
}


#' @rdname keep
#' @export
keep_last.grate_yearweek <- function(x, n, ...) {
  idx <- keep_idx(x, n = n, from_last = TRUE)
  x[idx]
}


#' @rdname keep
#' @export
keep_last.grate_month <- keep_last.grate_yearweek


#' @rdname keep
#' @export
keep_last.grate_quarter <- keep_last.grate_yearweek


#' @rdname keep
#' @export
keep_last.grate_year <- keep_last.grate_yearweek


#' @rdname keep
#' @export
keep_last.grate_period <- keep_last.grate_yearweek


# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- INTERNALS ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #


keep_idx <- function(x, n, from_last = FALSE) {
  uniq <- unique(x)
  if (n > length(uniq)) {
    message("n greater than number of unique date groupings. Returning all rows.")
    n <- length(uniq)
  } else if (n < 0) {
    abort("'n' must be non-negative")
  }

  if (from_last) {
    id <- rev(uniq)[n]
    return(x >= id)
  } else {
    id <- uniq[n]
    return(x <= id)
  }
}
