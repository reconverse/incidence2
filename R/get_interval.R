#' @return
#'   - `get_interval()`: if `integer = TRUE`, an integer vector, otherwise the
#'     character value of the `interval`

#' @rdname accessors
#' @aliases get_interval
#' @export
get_interval <- function(x, ...) {
  UseMethod("get_interval")
}

#' @rdname accessors
#' @aliases get_interval.default
#' @export
get_interval.default <- function(x, ...) {
  stop(sprintf("Not implemented for class %s",
               paste(class(x), collapse = ", ")))
}

#' @param integer When `TRUE`, the interval will be converted to an
#'   integer vector if it is stored as a character in the incidence object.
#' @rdname accessors
#' @aliases get_interval.incidence
#' @export
get_interval.incidence <- function(x, integer = FALSE, ...) {
  ellipsis::check_dots_empty()

  interval <- attr(x, "interval")

  if (!integer || is.numeric(interval)) {
    return(interval)
  }
  if (is.character(interval)) {
    res <- get_interval_type(interval)
    n   <- get_interval_number(interval)
    date_var <- get_date_name(x)
    dates <- unique(x[[date_var]])
    res <- switch(res,
                  day     = 1L * n,
                  week    = 7L * n,
                  month   = get_days_in_month(dates, n),
                  quarter = get_days_in_quarter(dates, n),
                  year    = get_days_in_year(dates, n)
    )
    return(res)
  } else {
    stop(sprintf("I don't know how to convert a %s to an integer",
                 paste(class(interval), collapse = ", ")))
  }
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
get_interval_type <- function(x) {
  res <- NULL
  res <- if (grepl("day", x, ignore.case = TRUE)) "day" else res
  res <- if (grepl("week", x, ignore.case = TRUE)) "week" else res
  res <- if (grepl("month", x, ignore.case = TRUE)) "month" else res
  res <- if (grepl("quarter", x, ignore.case = TRUE)) "quarter" else res
  res <- if (grepl("year", x, ignore.case = TRUE)) "year" else res
  res
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
get_interval_number <- function(x) {

  if (!grepl("^\\d", x)) return(1L)
  as.integer(gsub("^(\\d*).+$", "\\1", x))

}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
get_days_in_month <- function(dates, m = 1L) {
  dates <- floor_month(dates)
  res <- vapply(strsplit(format(dates), "-"),
                add_months,
                character(1),
                months = m)
  as.integer(as.Date(res) - dates)
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
get_days_in_quarter <- function(dates, m = 1L) {
  dates <- floor_month(dates)
  res <- vapply(strsplit(format(dates), "-"),
                FUN = add_months,
                FUN.VALUE = character(1),
                months = 3L * m)
  as.integer(as.Date(res) - dates)
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
get_days_in_year <- function(dates, m = 1L) {
  dates <- floor_month(dates)
  res <- vapply(strsplit(format(dates), "-"),
                FUN = add_months,
                FUN.VALUE = character(1),
                months = 12L * m)
  as.integer(as.Date(res) - dates)
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
floor_month <- function(x) {
  x - as.integer(format(x, "%d")) + 1L
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
add_months <- function(x, months = 1L) {
  i <- as.integer(x[2]) + months
  if (i > 12L) {
    x[1] <- as.character(as.integer(x[1]) + 1L)
    i    <- i - 12L
  }
  x[2] <- sprintf("%02d", i)
  paste(x, collapse = "-")
}
# -------------------------------------------------------------------------
