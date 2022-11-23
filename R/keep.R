#' Keep first and last occurences
#'
#' `keep_first()` (`keep_last`) keeps the first (last) `n` rows to occur for
#' each group when in ascending date order.
#'
#' @param x `[incidence]` object.
#'
#' @param n `[integer]`
#'
#' Number of entries to keep.
#'
#' `double` vectors will be converted via `as.integer(n)`.
#'
#' @param complete_dates `[bool]`
#'
#' Should `complete_dates()` be called on the data prior to keeping the first
#' entries.
#'
#' Defaults to TRUE.
#'
#' @param ...
#'
#' Other arguments passed to `complete_dates()`.
#'
#' @return
#'
#' Incidence object with the chosen entries.
#'
#' @examples
#'
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#' \dontshow{withAutoprint(\{}
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'     inci <- incidence(dat, "date_of_onset")
#'     keep_first(inci, 3)
#'     keep_last(inci, 3)
#' \dontshow{\})}
#' }
#' @name keep
NULL

#' @rdname keep
#' @export
keep_first <- function(x, n, complete_dates = TRUE, ...) {

    if (!inherits(x, "incidence"))
        stopf("`x` must be an incidence object.")

    if (!is.integer(n)) {
        if (is.vector(n, "double")) {
            n <- as.integer(n)
        } else {
            stopf("`n` must be integer.")
        }
    }
    if (n <= 0L)
        stopf("`n` must be non-negative")

    .assert_bool(complete_dates)
    if (complete_dates)
        x <- complete_dates(x, ...)

    dat <- get_dates(x)[[1L]]
    idx <- .keep_idx(dat, n = n, from_last = FALSE)
    x[idx, ]
}

#' @rdname keep
#' @export
keep_last <- function(x, n, complete_dates = TRUE, ...) {

    if (!inherits(x, "incidence"))
        stopf("`x` must be an incidence object.")

    if (!is.integer(n)) {
        if (is.vector(n, "double")) {
            n <- as.integer(n)
        } else {
            stopf("`n` must be integer.")
        }
    }
    if (n <= 0L)
        stopf("`n` must be non-negative")

    .assert_bool(complete_dates)
    if (complete_dates)
        x <- complete_dates(x, ...)

    dat <- get_dates(x)[[1L]]
    idx <- .keep_idx(dat, n = n, from_last = TRUE)
    x[idx, ]
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------- INTERNALS ------------------------------- #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.keep_idx <- function(x, n, from_last = FALSE) {
    uniq <- sort(unique(x), decreasing = FALSE)
    len <- length(uniq)
    if (n > len) {
        message("n greater than number of unique date groupings. Returning all rows.")
        n <- len
    }

    if (from_last) {
        id <- rev(uniq)[n]
        return(x >= id)
    } else {
        id <- uniq[n]
        return(x <= id)
    }
}
