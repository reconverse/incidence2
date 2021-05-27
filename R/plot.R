## Plotting notes
##
## Note 1: it seems safest to specify the aes() as part of the geom,
## not in ggplot(), as it interacts badly with some other geoms like
## geom_ribbon - used e.g. in projections::add_projections().
##
## Note 2: because of the way 'fill' works, we need to specify it through
## 'aes' if not directly in the geom. This causes the kludge below, where we
## make a fake constant group to specify the color and remove the legend.
##
## Note 3: when there are groups, and the 'color' argument does not have one
## value per group, we generate colors from a color palette. This means that
## by default, the palette is used, but the user can manually specify the
## colors.

#' Plotting functions
#'
#' incidence2 includes two plotting functions to simplify graph creation.
#'
#' @param x An [incidence()] object.
#' @param count Which count variable to have on the y-axis. If NULL (default)
#'   the first entry returned from `get_count_names(x)` is used.
#' @param fill Which variable to color plots by. If NULL no distinction if made
#'   for plot colors.
#' @param facets Which variable to facet plots by.  If NULL will use all
#'   group_labels of the incidence object.
#' @param title Optional title for the graph.
#' @param centre_dates If the interval is one of a single week, month, quarter
#'   or year the x_axis labels are centred with custom category labels. Set this
#'   option to FALSE to use date labels at the breaks.
#' @param date_format Format to use if "Date" scales are required. The value is
#'   used by `format.Date()` and can be any input acceptable by that function
#'   (defaults to "%Y-%m-%d).
#' @param stack A logical indicating if bars of multiple groups should be
#'   stacked, or displayed side-by-side. Only used if fill is not NULL.
#' @param col_pal col_pal The color palette to be used for the groups; defaults
#'   to `vibrant` (see `?palettes`).
#' @param alpha The alpha level for color transparency, with 1 being fully
#'   opaque and 0 fully transparent; defaults to 0.7.
#' @param color The color to be used for the borders of the bars; NA for
#'   invisible borders; defaults to NA.
#' @param xlab The label to be used for the x-axis; empty by default.
#' @param ylab The label to be used for the y-axis; by default, a label will be
#'   generated automatically according to the time interval used in incidence
#'   computation.
#' @param n.breaks Approximate number of breaks calculated using
#'   `scales::breaks_pretty` (default 6).
#' @param width Value between 0 and 1 indicating the relative size of the bars
#'   to the interval. Default 1.
#' @param show_cases if `TRUE` (default: `FALSE`), then each observation will be
#'   colored by a border. The border defaults to a white border unless specified
#'   otherwise. This is normally used outbreaks with a small number of cases.
#'   Note: this can only be used if `stack = TRUE`
#' @param border If show_cases is TRUE this represents the color used for the
#'   borders of the individual squares plotted (defaults to `"white"`).
#' @param na_color The colour to plot `NA` values in graphs (default: `grey`).
#' @param legend Position of legend in plot.
#' @param angle Rotation angle for text.
#' @param size text size in pts.
#' @param nrow Number of rows.
#' @param ... other arguments to pass to [`ggplot2::scale_x_continuous()`].
#'
#' @return
#'  - `facet_plot()` and `plot()` generate a [ggplot2::ggplot()] object.
#'
#' @details
#'  - `plot` creates a one-pane graph of an incidence object.
#'  - `facet_plot` creates a multi-facet graph of a grouped incidence object.
#'    If the object has no groups it returns the same output as a call to
#'    [plot()].
#'  - If the [incidence()] object has a rolling average column then that
#'    average will be overlaid on top.
#'
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
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

#' @importFrom rlang sym syms .data
#' @export
plot.incidence2 <- function(x, count = NULL, fill = NULL, centre_dates = TRUE,
                            date_format = "%Y-%m-%d", stack = TRUE,
                            title = NULL, col_pal = vibrant, alpha = 0.7,
                            color = NA, xlab = "", ylab = NULL, n.breaks = 6,
                            width = 1, show_cases = FALSE, border = "white",
                            na_color = "grey",
                            legend = c("right", "left", "bottom", "top", "none"),
                            angle = 0, size = NULL, ...) {

  check_suggests("ggplot2")
  if (!(length(width) == 1L && is.numeric(width))) {
    abort("`width` should be numeric and of length 1")
  }



  ellipsis::check_dots_used()

  # warnings
  group_vars <- get_group_names(x)
  if (length(group_vars) > 1) {
    msg <- paste("plot() can only stack/dodge by one variable.",
                 "For multi-facet plotting try facet_plot()",
                 sep = "\n")
    message(msg)
  }

  if (is.null(count)) {
    count <- get_count_names(x)[1]
  } else if (length(count) > 1) {
    abort("plot() can only work with one count variable at a time.")
  } else if (!(count %in% get_count_names(x))) {
    abort(c(
      "Value given for 'count' is not a variable in x.",
      i = "Permitted values can be obtained with get_count_names(x)."
    ))
  }

  # Convert fill to character
  tmp <- rlang::enquo(fill)
  idx <- try(tidyselect::eval_select(tmp, x), silent = TRUE)
  if (!inherits(idx, "try-error")) {
    fill <- names(x)[idx]
    if (length(fill) == 0) fill <- NULL
  }

  out <- plot_basic(x = x, count = count, fill = fill, stack = stack,
                    centre_dates = centre_dates, date_format = date_format,
                    n.breaks = n.breaks, width = width,
                    col_pal = col_pal, alpha = alpha, color = color,
                    xlab = xlab, ylab = ylab, show_cases = show_cases,
                    border = border, na_color = na_color,
                    legend = match.arg(legend), title = title, ...)

  out + rotate_and_scale(angle = angle, size = size)

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
facet_plot.incidence2 <- function(x, count = NULL, facets = NULL,
                                  centre_dates = TRUE, date_format = "%Y-%m-%d",
                                  stack = TRUE, fill = NULL, title = NULL,
                                  col_pal = vibrant, alpha = 0.7, color = NA,
                                  xlab = "", ylab = NULL, n.breaks = 3,
                                  width = 1, show_cases = FALSE,
                                  border = "white", na_color = "grey",
                                  legend = c("bottom", "top", "left", "right", "none"),
                                  angle = 0, size = NULL, nrow = NULL, ...) {

  check_suggests("ggplot2")

  ellipsis::check_dots_used()

  if (is.null(count)) {
    count <- get_count_names(x)[1]
  } else if (length(count) > 1) {
    stop(
      "plot() can only work with one count variable at a time.\n",
      call. = FALSE
    )
  } else if (!(count %in% get_count_names(x))) {
    stop(
      "Value given for 'count' is not a variable in x.\n",
      "       Permitted values can be obtained with get_count_names(x).",
      call. = FALSE
    )
  }

  # convert inputs to character
  facets <- rlang::enquo(facets)
  idx <- tidyselect::eval_select(facets, x)
  facets <- names(x)[idx]
  if (length(facets) == 0) facets <- NULL

  tmp <- rlang::enquo(fill)
  idx <- try(tidyselect::eval_select(tmp, x), silent = TRUE)
  if (!inherits(idx, "try-error")) {
    fill <- names(x)[idx]
    if (length(fill) == 0) fill <- NULL
  }

  group_vars <- get_group_names(x)

  out <- plot_basic(x = x, count = count, fill = fill, stack = stack,
                    centre_dates = centre_dates, date_format = date_format,
                    n.breaks = n.breaks, width = width,
                    col_pal = col_pal,alpha = alpha, color = color, xlab = xlab,
                    ylab = ylab, show_cases = show_cases, border = border,
                    na_color = na_color, legend = match.arg(legend),
                    title = title, ...)

  out <- out + rotate_and_scale(angle = angle, size = size)

  if (is.null(facets) && !is.null(group_vars)) {
    out <-
      out +
      ggplot2::facet_wrap(ggplot2::vars(!!!syms(group_vars)), nrow) +
      ggplot2::theme(panel.spacing.x = ggplot2::unit(8, "mm"))
  } else if (!is.null(facets)) {
    out <-
      out +
      ggplot2::facet_wrap(ggplot2::vars(!!!syms(facets)), nrow) +
      ggplot2::theme(panel.spacing.x = ggplot2::unit(8, "mm"))
  }

  out
}

plot_basic <- function(x, count, fill = NULL, centre_dates = TRUE, stack = TRUE,
                       n.breaks = 6, width = 1, date_format = "%Y-%m-%d",
                       col_pal = vibrant, alpha = 0.7, color = NA,
                       xlab = "", ylab = NULL,
                       show_cases = FALSE, border = "white",
                       na_color = "grey",
                       legend = c("right", "left", "bottom", "top", "none"),
                       title = NULL, ...) {


  # get relevant variables
  date_var <- get_dates_name(x)
  #count_var <- get_count_names(x)
  count_var <- count
  group_vars <- get_group_names(x)
  interval <- get_interval(x)
  legend <- match.arg(legend)

  # Handle stacking
  stack.txt <- if (stack) "stack" else "dodge"

  # set axis variables
  x_axis <- date_var
  y_axis <- count_var

  # copy data
  df <- x

  # generate label for y-axis
  ylab <- ylabel(df, ylab)

  if (!is.null(group_vars)) {
    if (!is.null(fill) && all(fill %in% group_vars)) {
      group_vars <- fill
    } else if (!is.null(fill) && !all(fill %in% group_vars)) {
      group_vars <- NULL
    }
  }

  d <- get_dates(x)
  width <- get_interval_number(interval) * width

  if (is.null(fill)) {
    out <- ggplot2::ggplot(df) +
      ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis), y = !!sym(y_axis)),
                        color = color,
                        fill = col_pal(1),
                        alpha = alpha,
                        width = width) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = xlab, y = ylab)
  } else if (!all(fill %in% group_vars)) {
    out <- ggplot2::ggplot(df) +
      ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis), y = !!sym(y_axis)),
                        color = color,
                        fill = fill,
                        alpha = alpha,
                        width = width) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = xlab, y = ylab)
  } else if (group_vars == fill) {
    group_names <- unique(df[[group_vars]])
    n_groups <- length(group_names)
    group_colors <- col_pal(n_groups)

    ## add colors to the plot
    out <- ggplot2::ggplot(df) +
      ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis), y = !!sym(y_axis)),
                        color = color,
                        alpha = alpha,
                        position = stack.txt,
                        width = width) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = legend) +
      ggplot2::labs(x = xlab, y = ylab) +
      ggplot2::aes(fill = !!sym(fill)) +
      ggplot2::scale_fill_manual(values = group_colors, na.value = na_color)
  } else {
    abort("Hhhhmmm, this shouldn't happen! Please raise an issue at https://github.com/reconverse/incidence2/issues")
  }

  if (show_cases && (stack == TRUE || is.null(fill))) {
    squaredf <- df[rep(seq.int(nrow(df)), df[[count_var]]), ]
    squaredf[[count_var]] <- 1
    squares <-
      ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis), y = !!sym(y_axis)),
                        color = if (is.na(border)) "white" else border,
                        fill  = NA,
                        position = "stack",
                        data = squaredf)

    out <- out + squares + ggplot2::coord_equal()
  }

  if (!is.null(title)) out <- out + ggplot2::labs(title = title)

  if (inherits(d, "grates_yearweek")) {
    if (centre_dates) date_format <- NULL
    out <- out + grates::scale_x_grates_yearweek(
      n.breaks = n.breaks,
      firstday = grates::get_firstday(d),
      format = date_format
    )
  } else if (inherits(d, "grates_quarter")) {
    if (centre_dates) date_format <- NULL
    out <- out + grates::scale_x_grates_quarter(
      n.breaks = n.breaks,
      format = date_format
    )
  } else if (inherits(d, "grates_month")) {
    if (centre_dates && attr(d, "n") == 1) date_format <- NULL
    out <- out + grates::scale_x_grates_month(
      n.breaks = n.breaks,
      format = date_format,
      n = attr(d, "n"),
      origin = as.numeric(min(d))
    )
  } else if (inherits(d, "grates_year")) {
    out <- out + grates::scale_x_grates_year(n.breaks = n.breaks)
  } else if (inherits(d, "grates_int_period")) {
    out <- out + grates::scale_x_grates_int_period(
      n.breaks = n.breaks,
      n = attr(d, "n"),
      origin = as.numeric(min(d))
    )
  } else if (inherits(d, "grates_period")) {
    out <- out + grates::scale_x_grates_period(
      n.breaks = n.breaks,
      format = date_format,
      n = attr(d, "n"),
      origin = as.numeric(min(d))
    )
  } else if (inherits(d, "Date")) {
    out <-  out + ggplot2::scale_x_date(breaks = scales::pretty_breaks(n = n.breaks), ...)
  } else if (inherits(d, "integer")) {
    out <- out + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = n.breaks), ...)
  } else {
    abort("Something has gone wrong! Please let the incidence2 devs know.")
  }

  out
}


ylabel <- function(x, ylab) {
  if (is.null(ylab)) {

    interval <- get_interval(x)
    type <- get_interval_type(interval)
    n <- get_interval_number(interval)
    date_vars <- get_dates_name(x)

    if (grates::is_int_period(get_dates(x))) {
      ylab <- sprintf("incidence by period of %d", interval)
    } else if (is.character(interval)) {
      if (interval == "1 day") {
        ylab <- "daily incidence"
      } else if (type == "period") {
        ylab <- sprintf("incidence by period of %d days", n)
      } else if (n == 1) {
        ylab <- sprintf("%sly incidence", type)
      } else {
        ylab <- sprintf("incidence by a period of %d %ss", n, type)
      }
    } else {
      if (n == 1) {
        ylab <- "incidence by day"
      } else {
        ylab <- sprintf("incidence by period of %d days", n)
      }

    }

    if (isTRUE(attr(x, "cumulative"))) {
      ylab <- sub("incidence", "cumulative incidence", ylab)
    }
    first_letter <- substring(ylab, 1, 1)
    substring(ylab, 1, 1) <- toupper(first_letter)
  }
  ylab
}

#' Rotate and scale incidence plot labels
#'
#' @param angle Angle to rotate x-axis labels.
#' @param size text size in pts.
#'
#' @noRd
rotate_and_scale <- function(angle = 0, size = NULL) {
  if (angle != 0) {
    hjust <- 1
  } else {
    hjust <- NULL
  }

  if (is.null(size)) {
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(hjust = hjust, angle = angle)
    )
  } else {
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = angle, hjust = hjust, size = size)
    )
  }
}
