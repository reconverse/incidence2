#' Count dates within bins
#'
#' @param dates A vector of dates, integers, or numerics.
#' @param breaks An ordered vector of dates or integers.
#'
#' @author Thibaut Jombart
#' @return An integer vector of the number of incidences per date.
#' @noRd
#'
count_dates <- function(dates, breaks) {
  counts <- table(cut(as.integer(dates), breaks = c(breaks, Inf), right = FALSE))
  as.integer(counts)
}
