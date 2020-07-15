#' Repeat breaks by count they contain.
#'
#' @param dates A vector of dates, integers, or numerics.
#' @param breaks An ordered vector of dates or integers.
#'
#' @return A vector of breaks with frequency equal to the count of dates they
#'   contain.
#' @noRd
group_dates <- function(dates, breaks) {
  counts <- count_dates(dates, breaks)
  rep(breaks, counts)
}
