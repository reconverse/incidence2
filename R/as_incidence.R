#' Coerce to an incidence object
#'
# -------------------------------------------------------------------------
#' Generic for coercion to an `<incidence2>` object.
#'
# -------------------------------------------------------------------------
#' @param x An \R object.
#'
#' @param ... Additional arguments to be passed to or from other methods.
#'
# -------------------------------------------------------------------------
#' @return An `<incidence2>` object.
#'
# -------------------------------------------------------------------------
#' @export
as_incidence <- function(x, ...) {
    UseMethod("as_incidence")
}

#' @rdname as_incidence
#' @export
as_incidence.default <- function(x, ...) {
    .stopf("Not implemented for objects of class <%s>.", toString(class(x)))
}

#' @rdname as_incidence
#' @export
as_incidence.incidence2 <- function(x, ...) {
    x
}
