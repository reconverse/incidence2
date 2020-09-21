## Plotting notes
##
## Note 1: By default, the annotation of bars in geom_bar puts the label in the
## middle of the bar. This is wrong in our case as the annotation of a time
## interval is the lower (left) bound, and should therefore be left-aligned
## with the bar. Note that we cannot use position_nudge to create the
## x-offset as we need the 'position' argument for stacking. This can be
## addressed by adding interval/2 to the x-axis, but this only works until we
## have an interval such as "month", "quarter", or "year" where the number of
## days for each can vary. To alleviate this, we can create a new column that
## counts the number of days within each interval.
##
## Note 2: it seems safest to specify the aes() as part of the geom,
## not in ggplot(), as it interacts badly with some other geoms like
## geom_ribbon - used e.g. in projections::add_projections().
##
## Note 3: because of the way 'fill' works, we need to specify it through
## 'aes' if not directly in the geom. This causes the kludge below, where we
## make a fake constant group to specify the color and remove the legend.
##
## Note 4: when there are groups, and the 'color' argument does not have one
## value per group, we generate colors from a color palette. This means that
## by default, the palette is used, but the user can manually specify the
## colors.

#' Plotting functions
#'
#' incidence2 includes two plotting functions to simplify graph creation.
#'
#' @param x An [incidence()] object.
#' @param fill Which variable to color plots by. If NULL no distinction if made
#'   for plot colors.
#' @param facets Which variable to facet plots by.  If NULL will use all
#'   group_labels of the incidence object.
#' @param title Optional title for the graph.
#' @param stack A logical indicating if bars of multiple groups should be
#'   stacked, or displayed side-by-side. Only used if fill is not NULL.
#' @param col_pal col_pal The color palette to be used for the groups; defaults
#'   to `vibrant` (see `?palettes`).
#' @param alpha The alpha level for color transparency, with 1 being fully
#'   opaque and 0 fully transparent; defaults to 0.7.
#' @param color The color to be used for the borders of the bars; NA for
#'   invisible borders; defaults to `NA`.
#' @param xlab The label to be used for the x-axis; empty by default.
#' @param ylab The label to be used for the y-axis; by default, a label will be
#'   generated automatically according to the time interval used in incidence
#'   computation.
#' @param n_breaks n_breaks the ideal number of breaks to be used for the x-axis
#'   labeling
#' @param show_cases if `TRUE` (default: `FALSE`), then each observation will be
#'   colored by a border. The border defaults to a white border unless specified
#'   otherwise. This is normally used outbreaks with a small number of cases.
#'   Note: this can only be used if `stack = TRUE`
#' @param border If show_cases is TRUE this represents the color used for the
#'   borders of the individual squares plotted (defaults to `"white"`).
#' @param group_labels group_labels a logical value indicating whether labels x
#'   axis tick marks are in week format YYYY-Www when plotting weekly incidence;
#'   defaults to TRUE.
#' @param na_color The colour to plot `NA` values in graphs (default: `grey`).
#' @param centre_ticks Should ticks on the x axis be centred on the bars. This
#'   only applies to intervals that produce unambiguous labels (i.e `1 day`,
#'   `1 month`, `1 quarter` or `1 year`).  Defaults to `FALSE`.
#' @param legend Position of legend in plot.
#' @param angle Rotation angle for text.
#' @param nrow Number of rows.
#' @param ... other arguments to pass to [scale_x_incidence()].
#'
#' @return
#'  - `facet_plot()` and `plot()` generate a [ggplot2::ggplot()] object.
#'
#' @details
#'  - `plot` creates a one-pane graph of an incidence object.
#'  - `facet_plot` creates a multi-facet graph of a grouped incidence object.
#'    If the object has no groups it returns the same outout as a call to
#'  - If the [incidence()] object has a rolling average column then that
#'    average will be overlaid on top.
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE)) {
#'   withAutoprint({
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'
#'     inci <- incidence(dat,
#'                       date_index = date_of_onset,
#'                       interval = 7,
#'                       groups = hospital)
#'
#'     inci2 <- incidence(dat,
#'                       date_index = date_of_onset,
#'                       interval = 7,
#'                       groups = c(hospital, gender))
#'
#'     plot(inci)
#'     plot(inci, fill = hospital)
#'     plot(inci, fill = hospital, stack = FALSE)
#'
#'     facet_plot(inci)
#'     facet_plot(inci2)
#'     facet_plot(inci2, facets = gender)
#'     facet_plot(inci2, facets = hospital, fill = gender)
#'   })
#' }

#' @importFrom rlang sym syms
#' @export
plot.incidence2 <- function(x, fill = NULL, stack = TRUE, title = NULL,
                           col_pal = vibrant, alpha = 0.7, color = NA,
                           xlab = "", ylab = NULL, n_breaks = 6,
                           show_cases = FALSE, border = "white",
                           na_color = "grey",
                           group_labels = TRUE, centre_ticks = FALSE,
                           legend = c("right", "left", "bottom", "top", "none"),
                           angle = 0, format = NULL,
                           ...) {

  ellipsis::check_dots_used()

  out <- plot_basic(x, !!rlang::enexpr(fill), stack,
                    col_pal, alpha, color,
                    xlab, ylab, n_breaks,
                    show_cases, border,
                    na_color,
                    group_labels, centre_ticks,
                    legend = match.arg(legend),
                    title = title)

 out + scale_x_incidence(x, n_breaks, group_labels, angle = angle,
                         format = format, ...)
}

#' @rdname plot.incidence2
#' @aliases facet_plot
#' @export
facet_plot <- function(x, ...) {
  UseMethod("facet_plot")
}

#' @importFrom rlang sym syms
#' @rdname plot.incidence2
#' @aliases facet_plot.incidence2
#' @export
facet_plot.incidence2 <- function(x, facets = NULL, stack = TRUE, fill = NULL, title = NULL,
                       col_pal = vibrant, alpha = 0.7, color = NA,
                       xlab = "", ylab = NULL, n_breaks = 3,
                       show_cases = FALSE, border = "white",
                       na_color = "grey",
                       group_labels = TRUE, centre_ticks = FALSE,
                       legend = c("bottom", "top", "left", "right", "none"),
                       angle = 0, format = NULL, nrow = NULL, ...) {

  ellipsis::check_dots_used()

  # convert inputs to character
  facets <- arg_values(!!rlang::enexpr(facets))
  fill <- arg_values(!!rlang::enexpr(fill))
  group_vars <- get_group_names(x)

  out <- plot_basic(x, !!rlang::enexpr(fill), stack,
                    col_pal, alpha, color,
                    xlab, ylab, n_breaks,
                    show_cases, border,
                    na_color,
                    group_labels, centre_ticks,
                    legend = match.arg(legend),
                    title = title)

  if (is.null(facets) && !is.null(group_vars)) {
    out <- 
      out + 
      ggplot2::facet_wrap(ggplot2::vars(!!!syms(group_vars)), nrow, ...) +
      ggplot2::theme(panel.spacing.x = ggplot2::unit(8, "mm"))
  } else if (!is.null(facets)) {
    out <- 
      out + 
      ggplot2::facet_wrap(ggplot2::vars(!!!syms(facets)), nrow, ...) +
      ggplot2::theme(panel.spacing.x = ggplot2::unit(8, "mm"))
  }

  out + scale_x_incidence(x, n_breaks, group_labels, angle = angle,
                          format = format, ...)
}

plot_basic <- function(x, fill = NULL, stack = TRUE,
                       col_pal = vibrant, alpha = 0.7, color = NA,
                       xlab = "", ylab = NULL, n_breaks = 6,
                       show_cases = FALSE, border = "white",
                       na_color = "grey",
                       group_labels = TRUE, centre_ticks = FALSE,
                       legend = c("right", "left", "bottom", "top", "none"),
                       title = NULL) {

  # convert inputs to character
  fill <- arg_values(!!rlang::enexpr(fill))

  # get relevant variables
  date_var <- get_dates_name(x)
  count_var <- get_counts_name(x)
  group_vars <- get_group_names(x)
  interval <- get_interval(x)
  legend <- match.arg(legend)

  # Handle stacking
  stack.txt <- if (stack) "stack" else "dodge"

  # warnings
  if (length(group_vars) > 1) {
    msg <- paste("plot() can only stack/dodge by one variable.",
                 "For multi-facet plotting try facet_plot()",
                 sep = "\n")
    message(msg)
  }

  # set axis variables
  x_axis <- date_var
  y_axis <- count_var

  # copy data
  df <- x

  # generate label for y-axis
  ylab <- ylabel(df, ylab)

  # Adding a variable for width in ggplot
  df$interval_days <- interval_days(df)
  if (to_label(interval) && centre_ticks) {
    df$interval_days <- 0
  } else if (!to_label(interval) && centre_ticks) {
    message(paste("centreing label for this interval is not possible",
                  "defaulting to labels on left side of bins",
                  sep = "\n"))
  }


  if (!is.null(group_vars)) {
    if (!is.null(fill) && all(fill %in% group_vars)) {
      group_vars <- fill
    } else if (!is.null(fill) && !all(fill %in% group_vars)) {
      group_vars <- NULL
    }
  }

  if (is.null(fill)) {
    out <- ggplot2::ggplot(df) +
      ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis) + .data$interval_days/2,
                                     y = !!sym(y_axis)),
                        width = .data$interval_days,
                        color = color,
                        fill = col_pal(1),
                        alpha = alpha) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = xlab, y = ylab)
  } else if (!all(fill %in% group_vars)) {
    out <- ggplot2::ggplot(df) +
      ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis) + .data$interval_days/2,
                                     y = !!sym(y_axis)),
                        width = .data$interval_days,
                        color = color,
                        fill = fill,
                        alpha = alpha) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = xlab, y = ylab)
  } else if (group_vars == fill) {
    group_names <- unique(df[[group_vars]])
    n_groups <- length(group_names)
    group_colors <- col_pal(n_groups)

    ## add colors to the plot
    out <- ggplot2::ggplot(df) +
      ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis) + .data$interval_days/2,
                                     y = !!sym(y_axis)),
                        width = .data$interval_days,
                        color = color,
                        alpha = alpha,
                        position = stack.txt) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = legend) +
      ggplot2::labs(x = xlab, y = ylab) +
      ggplot2::aes(fill = !!sym(fill)) +
      ggplot2::scale_fill_manual(values = group_colors, na.value = na_color)
  } else {
    stop("Hhhhmmm, this shouldn't happen! Please raise an issue at https://github.com/reconhub/incidence2/issues")
  }

  if (show_cases && (stack == TRUE || is.null(fill))) {
    squaredf <- suppressMessages(
      df[rep(seq.int(nrow(df)), df[[count_var]]), ]
    )
    squaredf[[count_var]] <- 1
    squares <-
      ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis) + .data$interval_days/2,
                                     y = !!sym(y_axis)),
                        color = if (is.na(border)) "white" else border,
                        fill  = NA,
                        position = "stack",
                        data = squaredf,
                        width = .data$interval_days)

    out <- out + squares
  }

  if (is.null(title)) {
    out
  } else {
    out + ggplot2::labs(title = title)
  }


}


has_weeks <- function(x) {
  date_group <- get_date_group_names(x)
  if (!is.null(date_group)) {
    if (class(x[[date_group]]) == "aweek") {
      return(TRUE)
    }
  }
  FALSE
}

has_isoweeks <- function(x) {
  if (has_weeks(x)) {
    date_group <- get_date_group_names(x)
    weeks <- x[[date_group]]
    if (attr(weeks, "week_start") == 1) {
      return(TRUE)
    }
  }
  FALSE
}

ylabel <- function(x, ylab) {
  if (is.null(ylab)) {

    interval <- get_interval(x)
    date_vars <- get_dates_name(x)

    if (is.numeric(interval)) {
      if (interval == 1) {
        ylab <- "daily incidence"
      } else if (interval == 7) {
        ylab <- "weekly incidence"
      } else if (interval == 14) {
        ylab <- "bi-weekly incidence"
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

interval_days <- function(x) {

  interval_days <- get_interval(x, integer = TRUE)

  ## if the date type is POSIXct, then the interval is actually interval seconds
  ## and needs to be converted to days
  date_var <- get_dates_name(x)
  if (inherits(x[[date_var]], "POSIXct")) {
    interval_days <- interval_days * 86400 # 24h * 60m * 60s
  }

  interval_days
}

to_label <- function(interval) {

  if (is.character(interval)) {
    interval <- tolower(interval)
  }

  is_day <- (interval %in% (c("day", "1 day", "1 days"))) || (interval %in% c(1, 1L))

  is_week <- interval == "week" || interval == "1 week" || interval == "1 weeks"

  is_month <- interval == "month" || interval == "1 month" || interval == "1 months"

  is_quarter <- interval == "quarter" || interval == "1 quarter" || interval == "1 quarters"

  is_year <- interval == "year" || interval == "1 year" || interval == "1 years"

  is_day || is_week || is_month || is_quarter || is_year

}
