plot.incidence <- function(x, color = "black", col_pal = incidence_pal1,
                           border = NA, xlab = "", ylab = NULL,
                           labels_week = has_weeks(x),
                           labels_iso = has_isoweeks(x),
                           show_cases = FALSE, n_breaks = 6) {

  # get relevant variables
  date_var <- get_date_vars(x)[1]
  count_var <- get_count_vars(x)
  group_vars <- get_group_vars(x)


  df <- x

  ylab <- ylabel(df, ylab)

  ## By default, the annotation of bars in geom_bar puts the label in the
  ## middle of the bar. This is wrong in our case as the annotation of a time
  ## interval is the lower (left) bound, and should therefore be left-aligned
  ## with the bar. Note that we cannot use position_nudge to create the
  ## x-offset as we need the 'position' argument for stacking. This can be
  ## addressed by adding interval/2 to the x-axis, but this only works until we
  ## have an interval such as "month", "quarter", or "year" where the number of
  ## days for each can vary. To alleviate this, we can create a new column that
  ## counts the number of days within each interval.

  ## Adding a variable for width in ggplot
  df$interval_days <- get_interval(x, integer = TRUE)
  ## if the date type is POSIXct, then the interval is actually interval seconds
  ## and needs to be converted to days
  if (inherits(df[[date_var]], "POSIXct")) {
    df$interval_days <- df$interval_days * 86400 # 24h * 60m * 60s
  }
  ## Important note: it seems safest to specify the aes() as part of the geom,
  ## not in ggplot(), as it interacts badly with some other geoms like
  ## geom_ribbon - used e.g. in projections::add_projections().


  ## add mid-interval positions for x-axis
  x_axis <- date_var
  y_axis <- count_var

  out <- ggplot(df) +
    geom_col(aes(x = !!sym(x_axis) + interval_days/2, y = !!sym(y_axis)))

  if (!is.null(group_vars)) {
    out <- out + facet_wrap(vars(!!!syms(group_vars)))
  }
   out
}

has_weeks <- function(x) {
  if (length(get_date_vars(x) > 1)) {
    TRUE
  } else {
    FALSE
  }
}

has_isoweeks <- function(x) {
  if (length(get_date_vars(x) == 3)) {
    TRUE
  } else {
    FALSE
  }
}

ylabel <- function(x, ylab) {
  if (is.null(ylab)) {

    interval <- attr(x, "interval")
    date_vars <- attr(x, "date")

    if (is.numeric(interval)) {
      if (interval == 1) {
        ylab <- "daily incidence"
      } else if (interval == 7) {
        ylab <- "weekly incidence"
      } else if (interval == 14) {
        ylab <- "semi-weekly incidence"
      } else {
        ylab <- sprintf("incidence by period of %d days", interval)
      }
    } else if (is.character(interval)) {
      # capturing the number and type
      p     <- "(\\d*)\\s?([a-z]+?)s?$"
      num   <- gsub(p, "\\1", tolower(interval))
      itype <- gsub(p, "\\2", tolower(interval))
      if (num == "" || num == "1") {
        ylab <- sprintf("%sly incidence", itype)
      } else {
        ylab <- sprintf("incidence by a period of %s %ss", num, itype)
      }
    }

    if (length(date_vars) > 1) {
      type_of_week <- get_type_of_week(x)
      ylab <- gsub("(weekl?y?)", sprintf("%s \\1", type_of_week), ylab)
    }

    if (isTRUE(attr(x, "cumulative"))) {
      ylab <- sub("incidence", "cumulative incidence", ylab)
    }
    first_letter <- substring(ylab, 1, 1)
    substring(ylab, 1, 1) <- toupper(first_letter)
  }
  ylab
}
