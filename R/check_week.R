#' Check for a valid week interval
#'
#' @param the_interval character, integer, or numeric.
#'
#' @return A logical value indicating if any of the tests pass.
#'
#' @noRd
check_week <- function(the_interval) {
  # TODO - check if we want interval = 7 treated like a week or not
  # num_week <- is.numeric(the_interval) && the_interval == 7
  # int_week <- is.integer(the_interval) && the_interval == 7L
  # char_week <- is.character(the_interval) && grepl("week", the_interval, ignore.case = TRUE)
  # num_week || int_week || char_week
  is.character(the_interval) && grepl("week", the_interval, ignore.case = TRUE)
}
