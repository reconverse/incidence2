#' Trim observations based on the first and last dates
#'
#' @param x A data.frame or tibble.
#' @param date_index Name of variable representing the dates.
#' @param first_date A single date or integer.
#' @param last_date A single date or integer.
#'
#' @return A tibble trimmed to required date and with NA dates removed.
#' @noRd
trim_observations <- function(x, date_index, first_date = NULL, last_date = NULL) {

  # Remove the missing observations --------------------
  n_orig <- nrow(x)
  x <- x[!is.na(x[[date_index]]), , drop=FALSE]
  n_new <- nrow(x)

  if (n_new < n_orig) {
    message(sprintf("%d missing observations were removed.", n_orig - n_new))
  }

  # Trim ends ------------------------------------------
  n_orig <- nrow(x)
  if (!is.null(first_date)) {
    x <- x[x[[date_index]] >= first_date, , drop=FALSE]
  }
  if (!is.null(last_date)) {
    x <- x[x[[date_index]] <= last_date, , drop=FALSE]
  }
  n_new <- nrow(x)

  if (n_new < n_orig) {
    message(sprintf("%d observations outside of [%s, %s] were removed.",
                    n_orig - n_new,
                    format(first_date),
                    format(last_date)))
  }

  x
}
