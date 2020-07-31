#' Check for a valid week interval
#'
#' @param the_interval character, integer, or numeric.
#'
#' @return A logical value indicating if any of the tests pass.
#'
#' @noRd
check_week <- function(the_interval) {
  is.character(the_interval) && grepl("week", the_interval, ignore.case = TRUE)
}
