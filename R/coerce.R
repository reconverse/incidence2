#' @importFrom data.table as.data.table
#' @export
data.table::as.data.table

#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

#' Coerce to and from an incidence object
#'
# -------------------------------------------------------------------------
#' Generic for coercion to an `<incidence2>` object.
#'
# -------------------------------------------------------------------------
#' @param x An \R object.
#'
#' @param row.names Not used.
#'
#' @param optional Not used.
#'
#' @param keep.rownames Not used.
#'
#' @param ... Additional arguments to be passed to or from other methods.
#'
#' @inheritParams tibble::as_tibble
#'
# -------------------------------------------------------------------------
#' @return An object of the desired type with additional attributes dropped.
#'
# -------------------------------------------------------------------------
#' @examples
#' \dontshow{.old <- data.table::setDTthreads(2)}
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'     x <- incidence(dat, "date_of_onset")
#'     as.data.frame(dat)
#'     as.data.table(x)
#'     as_tibble(x)
#' }
#' \dontshow{data.table::setDTthreads(.old)}
#'
# -------------------------------------------------------------------------
#' @name as
NULL


# -------------------------------------------------------------------------
#' @rdname as
#' @export
as_incidence <- function(x, ...) {
    UseMethod("as_incidence")
}


# -------------------------------------------------------------------------
#' @rdname as
#' @export
as_incidence.default <- function(x, ...) {
    .stopf("Not implemented for objects of class <%s>.", toString(class(x)))
}


# -------------------------------------------------------------------------
#' @rdname as
#' @export
as_incidence.incidence2 <- function(x, ...) {
    x
}


# -------------------------------------------------------------------------
#' @export
#' @rdname as
as.data.frame.incidence2 <- function(x, row.names, optional, ...) {
    attributes(x) <- list(
        names = attr(x, "names"),
        row.names = .row_names_info(x, type = 0L),
        class = "data.frame"
    )
    x
}


# -------------------------------------------------------------------------
#' @rdname as
#' @export
as.data.table.incidence2 <- function(x, keep.rownames, ...) {
    x <- as.data.frame(x)
    NextMethod()
}


# -------------------------------------------------------------------------
#' @rdname as
#' @export
as_tibble.incidence2 <- function(x, ..., .rows, .name_repair, rownames) {
    x <- as.data.frame(x)
    NextMethod()
}
