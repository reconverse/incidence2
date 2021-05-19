#' Summary of a given incidence object
#'
#' @param object An 'incidence' object.
#' @param ... Not used.
#'
#' @return object (invisibly).
#'
#' @export
summary.incidence2 <- function(object, ...) {

  # due to NSE notes in R CMD check
  ..count_var <- . <- NULL

  # get the date and count variables
  count_var <- get_count_names(object)
  date_var <- get_dates_name(object)
  dat <- object[[date_var]]

  # header
  header <- sprintf("An incidence2 object: %s x %s\n",
                    formatC(nrow(object), big.mark = ","),
                    formatC(ncol(object), big.mark = ","))
  cat(pillar::style_subtle(header))

  # cases over date range
  for (i in count_var) {
    if (inherits(dat, "Date") ||
        inherits(dat, "numeric") ||
        inherits(dat, "grate_yearweek") ||
        inherits(dat, "grate_quarter") ||
        inherits(dat, "grate_year") ||
        (inherits(dat, "grate_month") && attr(dat, "interval") == 1)) {
      d1 <- min(dat)
      d2 <- max(dat)
    } else if (inherits(dat, "grate_int_period")) {
      d1 <- as.integer(min(dat))
      d2 <- as.integer(max(dat + 1)) - 1
    } else {
      d1 <- as.Date(min(dat))
      d2 <- as.Date(max(dat + 1)) - 1
    }

    if(i == "count") {
      msg <- sprintf("%d cases from %s to %s\n", sum(object[[i]]), d1, d2)
    } else {
      msg <- sprintf("%d %s from %s to %s\n", sum(object[[i]]), i, d1, d2)
    }
    cat(msg)
  }


  # interval
  interval <- get_interval(object)
  if (is.numeric(interval)) {
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
      tmp <- as.data.table(object)
      tmp <- tmp[, lapply(.SD, sum, na.rm = TRUE), by = c(gr), .SDcols = count_var]
      tmp <- tibble::as_tibble(tmp)
      tmp <- format(tmp)
      cat(tmp[-1], sep = "\n")
      cat("\n\n")
    }
  }

  cat("\n")
  invisible(object)
}
