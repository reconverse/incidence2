#' Default internal constructor for incidence objects.
#'
#' @param x A tibble.
#' @param date_index The time index of the given data.  This should be the name
#'   corresponding to a date column in x.
#' @param interval An integer or character indicating the (fixed) size of the
#'   time interval used for computing the incidence; defaults to 1 day.
#' @param groups An optional character vector defining groups of observations
#'   for which incidence should be computed separately.
#' @param na_as_group A logical value indicating if missing group (NA) should be
#'   treated as a separate group.
#' @param first_date,last_date optional first/last dates to be used. When
#'   these are `NULL` (default), the dates from the first/last dates are taken
#'   from the observations. If these dates are provided, the observations will
#'   be trimmed to the range of \[first_date, last_date\].
#' @param count The count variable of the given data.  If NULL (default) the
#'   data is taken to be a linelist of individual observations.
#' @param ... Additional arguments. Currently used just for the standard
#'   argument.
#'
#' @importFrom dplyr mutate summarise n left_join .data
#' @importFrom stats complete.cases na.omit
#' @return An incidence2 object.
#' @noRd
make_incidence <- function(x, date_index, interval = 1L, groups = NULL,
                           na_as_group = TRUE, first_date = NULL,
                           last_date = NULL, type = NULL, count = NULL,
                           ...) {
  dots <- list(...)

  # pull out dates and groups
  dates <- x[[date_index]]

  # make sure input can be used
  dates <- check_dates(dates)
  x[[date_index]] <- dates
  interval <- check_interval(interval, if (is.null(dots$standard)) TRUE else dots$standard)

  # Check the interval and arrange the breaks
  first_date <- check_boundaries(dates, first_date, "first")
  last_date <- check_boundaries(dates, last_date, "last")

  # filter dates
  x <- trim_observations(x, date_index, first_date, last_date)

  # calculate breaks
  breaks <- make_breaks_easier(
    x[[date_index]],
    the_interval = interval,
    first_date = first_date,
    last_date = last_date,
    dots = dots
  )
  grouped_dates <- cut(as.integer(x[[date_index]]), breaks = c(breaks, Inf), right = FALSE)
  grouped_dates <- breaks[as.integer(grouped_dates)]
  x[[date_index]] = grouped_dates

  # choose name for date column
  if (interval == 1 || interval == 1L || interval == "1 day" || interval == "1 days") {
    date_col <- "date"
  } else {
    date_col <- "bin_date"
  }

  # generate grouped_dates
  x <- grouped_df(x, c(date_index, groups))
  if (is.null(count)) {
    x <- summarise(x, count = n(), .groups = "drop")
  } else {
    x <- summarise(x, count = sum(.data[[count]], na.rm = TRUE), .groups = "drop")
  }
  colnames(x)[1] <- date_col

  # Add in missing group_labels and give them zero count
  days <- seq(first_date, last_date, by = 1)
  grouped_days <- unique(group_dates(days, breaks))
  if (!is.null(groups)) {
    unique_groups <- lapply(groups, function(gr) unique(x[[gr]]))
    names(unique_groups) <- groups
    unique_groups[[date_col]] <- grouped_days
    combinations <- expand.grid(unique_groups)
  } else {
    combinations <- data.frame(grouped_days)
    colnames(combinations) <- date_col
  }
  x <- left_join(combinations, x, by = c(date_col, groups))
  x$count[is.na(x$count)] <- 0L

  # filter out NA
  x <- x[!is.na(x[[date_col]]), , drop=FALSE]
  if (!na_as_group) {
    x <- x[complete.cases(x[,groups,drop=FALSE]), , drop = FALSE] 
  }

  # deal with "week" intervals
  if (!is.numeric(interval) && grepl("week", interval)) {
    interval <- get_week_duration(interval)
  }

  # reorder (dates, groups, counts)
  x <- x[c(date_col, groups, "count")]

  # create subclass of tibble
  tbl <- tibble::new_tibble(x,
                            groups = groups,
                            date = date_col,
                            count = "count",
                            interval = interval,
                            cumulative = FALSE,
                            nrow = nrow(x),
                            class = "tmp"
  )
  tbl
}
