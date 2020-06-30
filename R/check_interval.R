#' Check the interval between bins
#'
#' This enforces that an interval is:
#'   * Strictly positive.
#'   * Integer (rounded) OR compatible with date.
#'   * Finite.
#'   * Of length 1.
#'
#' @param x An integer or numeric interval.
#' @param standard If TRUE, bins start at the beginning of intervals.
#'
#' @return An integer interval.
#' @noRd
check_interval <- function(x, standard = TRUE) {
  if (missing(x) || is.null(x)) {
    stop("Interval is missing or NULL")
  }
  if (length(x) != 1L) {
    stop(sprintf(
      "Exactly one value should be provided as interval (%d provided)",
      length(x)
    ))
  }
  if (!is.finite(x)) {
    if (is.character(x)) {
      x <- valid_interval_character(x, standard)
    } else {
      stop("Interval is not finite")
    }
  }
  if (is.numeric(x)) {
    x <- as.integer(round(old <- x))
  }
  if (x < 1L) {
    stop(sprintf(
      "Interval must be at least 1 (input: %.3f; after rounding: %d)",
      old, x
    ))
  }
  x
}
