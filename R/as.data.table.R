#' @importFrom data.table as.data.table
#' @export
data.table::as.data.table


#' Coerce to a data.table
#'
# -------------------------------------------------------------------------
#' @param x An [incidence2][incidence2::incidence] object.
#'
#' @param keep.rownames Not used.
#'
#' @param ... Passed to other methods.
#'
# -------------------------------------------------------------------------
#' @return
#' A [data.table][data.table::data.table] of the original input but with no
#' additional attributes.
#'
# -------------------------------------------------------------------------
#' @examples
#' \dontshow{.old <- data.table::setDTthreads(2)}
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'     x <- incidence(dat, "date_of_onset")
#'     as.data.table(x)
#' }
#' \dontshow{data.table::setDTthreads(.old)}
#'
# -------------------------------------------------------------------------
#' @seealso [data.table::as.data.table] for the underlying generic.
#'
# -------------------------------------------------------------------------
#' @export
as.data.table.incidence2 <- function(x, keep.rownames, ...) {
    x <- as.data.frame(x)
    NextMethod()
}
