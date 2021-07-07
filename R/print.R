#' Print an incidence object.
#'
#' @param x An 'incidence' object.
#' @param ... Additional arguments passed through to the tibble format method.
#'
#' @name print_incidence
#' @export
print.incidence_df <- function(x, ...) {
  writeLines(format(x, ...))
  invisible(x)
}


#' @export
#' @rdname print_incidence
format.incidence_df <- function(x, ...) {
  overview <- overview(x)
  body <- format(tibble::as_tibble(x, ...))[-1]
  if (inherits(x, "incidence2")) {
    inter <- interval(x)
    out <- c(overview, inter, "", body)
  } else {
    out <- c(overview, "", body)
  }
  out
}


overview <- function(x) {
  count_var <- get_count_names(x)
  date_var <- get_dates_name(x)
  dat <- x[[date_var]]
  d1 <- min(dat)
  d2 <- max(dat)
  header <- sprintf(
    "An incidence object: %s x %s",
    formatC(nrow(x), big.mark = ","),
    formatC(ncol(x), big.mark = ",")
  )
  header <- pillar::style_subtle(header)
  date_range <- sprintf("date range: [%s] to [%s]", d1, d2)
  cases <- vapply(
    count_var,
    function(var) {
      if(var == "count") {
        sprintf("cases: %d", sum(x[[var]]))
      } else {
        sprintf("%s: %d", var, sum(x[[var]]))
      }
    },
    character(1)
  )
  c(header, date_range, cases)
}

interval <- function(x) {
  interval <- get_interval(x)
  if (is.numeric(interval)) {
    sprintf("interval: %d %s", interval, ifelse(interval < 2, "day", "days"))
  } else if (grepl("\\d", interval)) {
    sprintf("interval: %s", interval)
  } else {
    sprintf("interval: 1 %s", interval)
  }
}
