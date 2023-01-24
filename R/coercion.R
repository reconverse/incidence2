#' Convert incident object to a data frame
#'
#' @param x An `[incidence2]` object.
#'
#' @param row.names Not used.
#'
#' @param optional Not used.
#'
#' @param ... Not used.
#'
#' @examples
#' dat <- data.frame(dates = Sys.Date() + 1:100,
#'                   names = rep(c("Jo", "John"), 5))
#'
#' dat <- incidence(dat, date_index = "dates", groups = "names")
#' as.data.frame(dat)
#'
#' @export
as.data.frame.incidence2 <- function(x, row.names, optional,...) {
    attributes(x) <- list(
        names = attr(x, "names"),
        row.names = .row_names_info(x, type = 0L),
        class = "data.frame"
    )
    x
}

#' Coerce to an incidence object
#'
#' Generic for coercion to an `[incidence]` object.
#'
#' @param x An \R object.
#'
#' @param ... Additional arguments to be passed to or from other methods.
#'
#' @return An `[incidence]` object.
#'
#' @export
as_incidence <- function(x, ...) {
    UseMethod("as_incidence")
}

#' @rdname as_incidence
#' @export
as_incidence.default <- function(x, ...) {
    stopf("Not implemented for class [%s].", paste(class(x), collapse = ", "))
}

#' @rdname as_incidence
#' @export
as_incidence.incidence2 <- function(x, ...) {
    x
}

