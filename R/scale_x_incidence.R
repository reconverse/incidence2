#' @param ... arguments passed to [ggplot2::scale_x_date()],
#'   [ggplot2::scale_x_datetime()], or [ggplot2::scale_x_continuous()],
#'   depending on how the `$date` element is stored in the incidence object.
#' @param format Character string of desired format.  See `?strptime`.
#' @export
#' @rdname plot.incidence
scale_x_incidence <- function(x, n_breaks = 6, group_labels = TRUE, format = NULL, ...) {

  date_var <- get_date_name(x)

  breaks <- make_breaks(x, n_breaks, group_labels)

  if (inherits(x[[date_var]], "Date")) {
    if (is.null(format)) {
      labels <- breaks$labels
      interval <- get_interval(x)
      if (grepl("week", interval)) {
        reg <- regexpr("^\\d+", interval)
        if (reg != -1L) {
          num_weeks <- regmatches(interval, reg)
          if (num_weeks > 1) {
            labels <- breaks$breaks
          }
        }
      }

    } else {
      labels <- format(breaks$breaks, format = format)
    }
    out <- ggplot2::scale_x_date(breaks = breaks$breaks,
                                 labels = labels,
                                 ...)
  } else if (inherits(x[[date_var]], "POSIXt")) {
    if (is.null(format)) {
      labels <- breaks$labels
      interval <- get_interval(x)
      if (grepl("week", interval)) {
        reg <- regexpr("^\\d+", interval)
        if (reg != -1L) {
          num_weeks <- regmatches(interval, reg)
          if (num_weeks > 1) {
            labels <- breaks$breaks
          }
        }
      }
    } else {
      labels <- format(breaks$breaks, format = format)
    }
    breaks$breaks <- as.POSIXct(as.POSIXlt(breaks$breaks))
    out <- ggplot2::scale_x_datetime(breaks   = breaks$breaks,
                                     labels   = labels,
                                     timezone = "UTC",
                                     ...
    )
  } else {
    if (!is.null(format)) {
      message("Cannot format variable\n")
    }

    out <- ggplot2::scale_x_continuous(breaks = breaks$breaks, ...)
  }

  out
}

make_breaks <- function(x, n_breaks = 6L, group_labels = TRUE) {
  stopifnot(inherits(x, "incidence"), is.logical(group_labels), is.numeric(n_breaks))

  date_var <- get_date_name(x)
  ## Defining breaks for the x axis --------------------------------------------
  ##
  ## The x axis can either be integers, Dates, or POSIXt scales. Moreover,
  ## we need to make sure that the breaks align with the left-hand side of the
  ## bins (for now). This section first defines what the breaks should be
  ## and then treats them according to whether or not the interval was specified
  ## as a character.
  if (n_breaks == nrow(x)) {
    # The number of breaks are equal to the number of dates... don't worry about
    # adjusting
    breaks <- x[[date_var]]
  } else {
    # adjust breaks to force first date to beginning.
    breaks <- pretty(x[[date_var]], n_breaks)
    breaks <- breaks + (x[[date_var]][1] - breaks[1])
  }
  ## Defining the x axis scale -------------------------------------------------
  ##
  ## Choosing between scale_x_date, scale_x_datetime, and scale_x_continuous

  # labels should be dates or numbers
  interval <- get_interval(x)
  if (is.character(interval)) {
    # The interval is a character like "2 weeks" and we have to figure out how
    # to split these manually
    has_number <- grepl("\\d", interval)
    tims       <- ceiling(get_timespan(x)/(n_breaks*mean(get_interval(x, integer = TRUE))))
    if (has_number) {
      ni <- as.integer(strsplit(interval, " ", fixed = TRUE)[[1L]][1L])
      # the replacement should be a multiple of the number
      #replacement <- if (tims <= ni) ni else ceiling(tims/ni)*ni
      replacement <- if (tims <= ni) ni else tims*ni
      db <- gsub("\\d+", replacement, interval)
    } else if (interval == "quarter") {
      db <- paste(tims * 3, "months")
    } else {
      db <- sprintf("%d %s", tims, interval)
    }
    breaks <- seq(x[[date_var]][1], x[[date_var]][nrow(x)], by = db)
  }

  date_interval <- is.character(interval) && is_date_interval(interval)
  is_month <- interval == "month" || interval == "1 month" || interval == "1 months"
  is_quarter <- interval == "quarter" || interval == "1 quarter" || interval == "1 quarters"
  is_year <- interval == "year" || interval == "1 year" || interval == "1 years"

  if (has_weeks(x)) {
    date_group <- get_date_group_names(x)
    weeks <- x[[date_group]]

    # If the data are in weeks, we should make sure that the line up correctly
    w <- aweek::date2week(breaks,
                          week_start = attr(weeks, "week_start"),
                          floor_day = TRUE)
    breaks <- aweek::week2date(w)
    labels <- if (group_labels) w else ggplot2::waiver()
  } else if (date_interval && is_month) {
    m <- format(breaks, "%b %y")
    labels <- if (group_labels) m else ggplot2::waiver()
  } else if (date_interval && is_quarter) {
    q <- paste(quarters(breaks), format(breaks, "%Y"))
    labels <- if (group_labels) q else ggplot2::waiver()
  } else if (date_interval && is_year) {
    y <- format(breaks, "%Y")
    labels <- if (group_labels) y else ggplot2::waiver()
  } else {
    labels <- ggplot2::waiver()
  }

  list(breaks = breaks, labels = labels)
}
