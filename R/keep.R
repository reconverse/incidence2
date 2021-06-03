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
keep_first.incidence <- function(x, n, ...) {
  dat <- get_dates(x)
  idx <- keep_idx(dat, n = n)
  x[idx, ]
}


#' @rdname keep
#' @export
keep_first.grates_yearweek <- function(x, n, ...) {
  idx <- keep_idx(x, n = n)
  x[idx]
}


#' @rdname keep
#' @export
keep_first.grates_month <- keep_first.grates_yearweek


#' @rdname keep
#' @export
keep_first.grates_quarter <- keep_first.grates_yearweek


#' @rdname keep
#' @export
keep_first.grates_year <- keep_first.grates_yearweek


#' @rdname keep
#' @export
keep_first.grates_period <- keep_first.grates_yearweek


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
keep_last.incidence <- function(x, n, ...) {
  dat <- get_dates(x)
  idx <- keep_idx(dat, n = n, from_last = TRUE)
  x[idx, ]
}


#' @rdname keep
#' @export
keep_last.grates_yearweek <- function(x, n, ...) {
  idx <- keep_idx(x, n = n, from_last = TRUE)
  x[idx]
}


#' @rdname keep
#' @export
keep_last.grates_month <- keep_last.grates_yearweek


#' @rdname keep
#' @export
keep_last.grates_quarter <- keep_last.grates_yearweek


#' @rdname keep
#' @export
keep_last.grates_year <- keep_last.grates_yearweek


#' @rdname keep
#' @export
keep_last.grates_period <- keep_last.grates_yearweek


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
