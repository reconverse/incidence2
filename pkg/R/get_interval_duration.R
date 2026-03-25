#' @importFrom grates get_interval_duration
#' @export
grates::get_interval_duration

# -------------------------------------------------------------------------
#' Days covered by the date index of an incidence object
#'
# -------------------------------------------------------------------------
#' Utility function for returning the number of days covered by an element
#' of the incidence objects date_index column.
#'
# -------------------------------------------------------------------------
#' @param x An incidence object.
#'
#' @param ... Not currently used.
#'
# -------------------------------------------------------------------------
#' @return
#' The number of days covered from the start to end dates for each element in
#' the date_index.
#'
# -------------------------------------------------------------------------
#' @examples
#' \dontshow{.old <- data.table::setDTthreads(2)}
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'     i <- incidence(dat, "date_of_onset", interval = "isoweek")
#'     get_interval_duration(i)
#' }
#' \dontshow{data.table::setDTthreads(.old)}
#'
#'
# -------------------------------------------------------------------------
#' @export
get_interval_duration.incidence2 <- function(x, ...) {
    dates <- get_date_index(x)
    if (inherits(dates, "Date")) {
        if (.is_whole_or_NA(dates)) {
            return(rep(1, length(dates)))
        }
        .stopf(
            "Some values of '%s' are fractional dates so we cannot infer a duration.",
            get_date_index_name(x)
        )
    } else {
        get_interval_duration(dates)
    }
}
