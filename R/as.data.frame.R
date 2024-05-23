#' Convert incident object to a data frame
#'
# -------------------------------------------------------------------------
#' @param x [incidence2][incidence2::incidence] object.
#'
#' @param row.names Not used.
#'
#' @param optional Not used.
#'
#' @param ... Not used.
#'
# -------------------------------------------------------------------------
#' @examples
#' \dontshow{.old <- data.table::setDTthreads(2)}
#' dat <- data.frame(
#'     dates = Sys.Date() + 1:100,
#'     names = rep(c("Jo", "John"), 5)
#' )
#'
#' dat <- incidence(dat, date_index = "dates", groups = "names")
#' as.data.frame(dat)
#' \dontshow{data.table::setDTthreads(.old)}
#'
# -------------------------------------------------------------------------
#' @seealso [as.data.frame] for the underlying generic.
#'
# -------------------------------------------------------------------------
#' @export
as.data.frame.incidence2 <- function(x, row.names, optional,...) {
    attributes(x) <- list(
        names = attr(x, "names"),
        row.names = .row_names_info(x, type = 0L),
        class = "data.frame"
    )
    x
}
