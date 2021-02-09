#' Summary of a given incidence object
#'
#' @param object An 'incidence' object.
#' @param ... Not used.
#'
#' @return object (invisibly).
#'
#' @export
summary.incidence2 <- function(object, ...) {

  # get the date and count variables
  count_var <- get_counts_name(object)
  date_var <- get_dates_name(object)

  # header
  header <- sprintf("An incidence2 object: %s x %s\n",
                    formatC(nrow(object), big.mark = ","),
                    formatC(ncol(object), big.mark = ","))
  cat(pillar::style_subtle(header))

  # cases over date range
  cat(sprintf(
    "%d cases from %s to %s\n",
    sum(object[[count_var]]), min(object[[date_var]]), max(object[[date_var]])
  ))

  # interval
  interval <- get_interval(object)
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
  groups <- get_group_names(object)
  if (!is.null(groups)) {
    cat(sprintf("%d grouped %s\n\n",
                length(groups),
                ifelse(length(groups) < 2, "variable", "variables")))

    for (gr in groups) {
      tmp <- grouped_df(object, gr)
      tmp <- summarise(tmp, count = sum( .data[[count_var]] ))
      tmp <- format(tmp)
      cat(tmp[-1], sep = "\n")
      cat("\n\n")
    }
  }

  cat("\n")
  invisible(object)
}
