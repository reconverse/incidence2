#' Summary of a given incidence object
#'
#' @param object An 'incidence' object.
#' @param ... Not used.
#' @export
summary.incidence <- function(object, ...) {

  # get the date and count variables
  count_var <- attr(object, "count")
  date_var <- attr(object, "date")

  # title
  cat("<incidence object>\n\n")

  # cases over date range
  cat(sprintf(
    "%d cases from days %s to %s\n",
    sum(object[[count_var]]), min(object[[date_var]]), max(object[[date_var]])
  ))

  # interval
  interval <- attr(object, "interval")
  if (is.integer(interval)) {
    cat(sprintf("interval: %d %s\n", interval, ifelse(interval < 2, "day", "days")))
  } else if (grepl("\\d", interval)) {
    cat(sprintf("interval: %s\n", interval))
  } else {
    cat(sprintf("interval: 1 %s\n", interval))
  }

  # cumulative
  cumulative <- attr(object, "cumulative")
  if (!is.null(cumulative)) {
    cat(sprintf("cumulative: %s\n", cumulative))
  }

  # timespan
  cat(sprintf("timespan: %d days\n\n", get_timespan(object)))

  # groups
  groups <- attr(object, "groups")
  if (!is.null(groups)) {
    cat(sprintf("%d grouped %s\n\n", length(groups), ifelse(length(groups) < 2, "variable", "variables")))

    for (gr in groups) {
      tmp <- group_by(object, .data[[gr]])
      tmp <- summarise(tmp, count = sum( .data[[count_var]] ))
      tmp <- format(tmp)
      cat(tmp[-1], sep = "\n")
      cat("\n\n")
    }
  }

  cat("\n")
  invisible(object)
}
