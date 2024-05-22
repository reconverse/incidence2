#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

#' Coerce to a tibble
#'
#' @param x An [incidence2][incidence2::incidence] object.
#' @inheritParams tibble::as_tibble
#'
#' @return A [tibble][tibble::tibble] of the original input but with no
#' additional attributes.
#'
#' @examples
#' \dontshow{.old <- data.table::setDTthreads(2)}
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'     x <- incidence(dat, "date_of_onset")
#'     as_tibble(x)
#' }
#' \dontshow{data.table::setDTthreads(.old)}
#'
#' @export
as_tibble.incidence2 <- function(x, ..., .rows, .name_repair, rownames) {
    x <- as.data.frame(x)
    NextMethod()
}
