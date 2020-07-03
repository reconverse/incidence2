#' Print an incidence object.
#'
#' @param x An 'incidence' object.
#' @param ... Not used.
#'
#' @export
print.incidence <- function(x, ...) {


  # get the date and count variables
  count_var <- attr(x, "count")
  date_var <- attr(x, "date")[1]

  # title
  cat("<incidence object>\n")

  # cases over date range
  cat(sprintf(
    "[%d cases from days %s to %s]\n",
    sum(x[[count_var]]), min(x[[date_var]]), max(x[[date_var]])
  ))

  # interval
  interval <- attr(x, "interval")
  if (is.integer(interval)) {
    cat(sprintf("[interval: %d %s]\n", interval, ifelse(interval < 2, "day", "days")))
  } else if (grepl("\\d", interval)) {
    cat(sprintf("[interval: %s]\n", interval))
  } else {
    cat(sprintf("[interval: 1 %s]\n", interval))
  }

  # cumulative
  cumulative <- attr(x, "cumulative")
  if (!is.null(cumulative)) {
    cat(sprintf("[cumulative: %s]\n\n", cumulative))
  }

  # print tibble but remove title
  out <- format(tibble::as_tibble(x))
  cat(out[-1], sep = "\n")
  cat("\n")

  invisible(x)
}
