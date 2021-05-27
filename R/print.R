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
  dat <- x[[date_var]]

  # header
  header <- sprintf("An incidence2 object: %s x %s\n",
                    formatC(nrow(x), big.mark = ","),
                    formatC(ncol(x), big.mark = ","))
  cat(pillar::style_subtle(header))

  # cases over date range
  for (i in count_var) {
    if (inherits(dat, "Date") ||
        inherits(dat, "numeric") ||
        inherits(dat, "grates_yearweek") ||
        inherits(dat, "grates_quarter") ||
        inherits(dat, "grates_year") ||
        (inherits(dat, "grates_month") && attr(dat, "n") == 1)) {
      d1 <- min(dat)
      d2 <- max(dat)
    } else if (inherits(dat, "grates_int_period") || inherits(dat, "numeric") || inherits(dat, "integer")) {
      d1 <- as.integer(min(dat))
      d2 <- as.integer(max(dat + 1)) - 1
    } else {
      d1 <- as.Date(min(dat))
      d2 <- as.Date(max(dat + 1)) - 1
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

