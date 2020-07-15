group_labels <- function(x, interval, standard) {
  date_var <- get_date_name(x)


  if (check_week(interval) && standard) {
    week_start <- get_week_start(interval)
    x$date_group <- aweek::date2week(x[[date_var]], week_start, floor_day = TRUE)
    attr(x, "date_group") <- "date_group"
    x <- dplyr::relocate(x, .data$date_group, .after = .data[[date_var]])
  }

  date_interval <- is.character(interval) && is_date_interval(interval)
  is_month <- interval == "month" || interval == "1 month" || interval == "1 months"
  is_quarter <- interval == "quarter" || interval == "1 quarter" || interval == "1 quarters"
  is_year <- interval == "year" || interval == "1 year" || interval == "1 years"

  if (date_interval && is_month) {
    x$date_group <- format(x[[date_var]], "%b %y")
    attr(x, "date_group") <- "date_group"
    x <- dplyr::relocate(x, .data$date_group, .after = .data[[date_var]])
  }

  if (date_interval && is_quarter) {
    x$date_group <- paste(quarters(x[[date_var]]), format(x[[date_var]], "%Y"))
    attr(x, "date_group") <- "date_group"
    x <- dplyr::relocate(x, .data$date_group, .after = .data[[date_var]])
  }

  if (date_interval && is_year) {
    x$date_group <- format(x[[date_var]], "%Y")
    attr(x, "date_group") <- "date_group"
    x <- dplyr::relocate(x, .data$date_group, .after = .data[[date_var]])
  }
  x
}
