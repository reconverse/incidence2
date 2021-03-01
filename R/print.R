#' Print an incidence object.
#'
#' @param x An 'incidence2' object.
#' @param ... Not used.
#'
#' @export
print.incidence2 <- function(x, ...) {

  # get the date and count variables
  count_var <- get_count_names(x)
  date_var <- get_dates_name(x)

  # header
  header <- sprintf("An incidence2 object: %s x %s\n",
                    formatC(nrow(x), big.mark = ","),
                    formatC(ncol(x), big.mark = ","))
  cat(pillar::style_subtle(header))

  # cases over date range
  for (i in count_var) {
    if (inherits(x[[date_var]], "period")) {
      d1 <- as.Date(min(x[[date_var]]))
      d2 <- as.Date(max(x[[date_var]]) + 1) - 1
    } else if (inherits(x[[date_var]], "int_period")) {
      d1 <- as.integer(min(x[[date_var]]))
      d2 <- as.integer(max(x[[date_var]]) + 1) - 1
    } else {
      d1 <- min(x[[date_var]])
      d2 <- max(x[[date_var]])
    }

    if(i == "count") {
      msg <- sprintf("%d cases from %s to %s\n", sum(x[[i]]), d1, d2)
    } else {
      msg <- sprintf("%d %s from %s to %s\n", sum(x[[i]]), i, d1, d2)
    }
    cat(msg)
  }


  # interval
  interval <- get_interval(x)
  if (is.numeric(interval)) {
    cat(sprintf("interval: %d %s\n", interval, ifelse(interval < 2, "day", "days")))
  } else if (grepl("\\d", interval)) {
    cat(sprintf("interval: %s\n", interval))
  } else {
    cat(sprintf("interval: 1 %s\n", interval))
  }

  # cumulative
  cumulative <- attr(x, "cumulative")
  if (!is.null(cumulative)) {
    cat(sprintf("cumulative: %s\n\n", cumulative))
  }

  # print tibble but remove title
  out <- format(tibble::as_tibble(x))
  cat(out[-1], sep = "\n")
  cat("\n")

  invisible(x)
}
