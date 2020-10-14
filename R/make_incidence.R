#' Default internal constructor for incidence objects.
#'
#' @param x A tibble.
#'
#' @param date_index The time index of the given data.  This should be the name
#'   corresponding to a date column in x.
#'
#' @param interval An integer or character indicating the (fixed) size of the
#'   time interval used for computing the incidence; defaults to 1 day.
#'
#' @param groups An optional character vector defining groups of observations
#'   for which incidence should be computed separately.
#'
#' @param na_as_group A logical value indicating if missing group (NA) should be
#'   treated as a separate group.
#'
#' @param first_date,last_date optional first/last dates to be used. When
#'   these are `NULL` (default), the dates from the first/last dates are taken
#'   from the observations. If these dates are provided, the observations will
#'   be trimmed to the range of \[first_date, last_date\].
#'
#' @param ... Additional arguments. Currently used just for the standard
#'   argument.
#'
#' @param count The count variable of the given data.  If NULL (default) the
#'   data is taken to be a linelist of individual observations.
#'
#' @author Zhian Kamvar, Tim Taylor
#' @importFrom dplyr mutate group_by across summarise n left_join filter
#' @importFrom rlang :=
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

  # filter out NA
  x <- x[!is.na(x[[date_index]]), , drop=FALSE]
  if (!na_as_group) {
    x <- x[complete.cases(x[,groups,drop=FALSE]), ] 
  } else {
    x[,groups] <- convert_to_NA(x[,groups, drop=FALSE])
  }

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
  x[date_index] = grouped_dates

  # choose name for date column
  if (interval == 1 || interval == 1L || interval == "1 day" || interval == "1 days") {
    date_col <- "date"
  } else {
    date_col <- "bin_date"
  }


  
  if (is.null(count)) {
    x$count = 1
    fm <- paste("count~", paste(c(date_index, groups), collapse = "+"))
    x <- aggregate(as.formula(fm), data = x, length, na.action = na.pass)
  } else {
    fm <- paste(count, paste(c(date_index, groups), collapse = "+"), sep = "~")
    x <- aggregate(as.formula(fm), data = x, sum, na.rm = TRUE, na.action = na.pass)
  }

  colnames(x) <- c(date_col, colnames(x)[-1])

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

  #x <- left_join(combinations, x, by = c(date_col, groups))
  x <- merge(combinations, x, by = c(date_col, groups), all.x = TRUE)
  x$count[is.na(x$count)] <- 0L



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


convert_to_NA <- function(dat) {
    out <- lapply(
        dat,
        function(x) {
            if (is.factor(x)) {
                addNA(x)
            } else {
                x[is.na(x)] <- "NA"
                x
            }
        }
    )
}