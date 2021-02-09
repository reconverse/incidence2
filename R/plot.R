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
#' @param na_color The colour to plot `NA` values in graphs (default: `grey`).
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
#'    If the object has no groups it returns the same output as a call to
#'    [plot()].
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

#' @importFrom ggplot2 sym syms .data
#' @export
plot.incidence2 <- function(x, fill = NULL, stack = TRUE, title = NULL,
                           col_pal = vibrant, alpha = 0.7, color = NA,
                           xlab = "", ylab = NULL, n_breaks = 5,
                           show_cases = FALSE, border = "white",
                           na_color = "grey",
                           legend = c("right", "left", "bottom", "top", "none"),
                           angle = 0,
                           ...) {

  ellipsis::check_dots_used()

  # warnings
  group_vars <- get_group_names(x)
  if (length(group_vars) > 1) {
    msg <- paste("plot() can only stack/dodge by one variable.",
                 "For multi-facet plotting try facet_plot()",
                 sep = "\n")
    message(msg)
  }

  # Convert fill to character
  fill <- rlang::enquo(fill)
  idx <- tidyselect::eval_select(fill, x)
  fill <- names(x)[idx]
  if (length(fill) == 0) fill <- NULL

  out <- plot_basic(x = x, fill = fill, stack = stack, col_pal = col_pal,
                    alpha = alpha, color = color, xlab = xlab, ylab = ylab,
                    show_cases = show_cases, border = border,
                    na_color = na_color, legend = match.arg(legend),
                    title = title)

  out <- out + rotate_and_scale(angle = angle)

  dat <- get_dates(x)
  cl <- class(dat)[1]
  if (cl == "yrwk") {
    out + scale_x_yrwk(n = n_breaks, firstday = get_firstday(dat), ...)
  } else if (cl == "yrmon") {
    out + scale_x_yrmon(n = n_breaks, ...)
  } else if (cl == "yrqtr") {
    out + scale_x_yrqtr(n = n_breaks, ...)
  } else if (cl == "yr") {
    out + scale_x_yr(n = n_breaks, ...)
  } else if (cl == "yr") {
    out + scale_x_yr(n = n_breaks, ...)
  } else if (cl == "period") {
    out + scale_x_period(n = n_breaks, firstdate = get_firstdate(dat), interval = get_interval(dat), ...)
  } else if (cl == "Date") {
    out + ggplot2::scale_x_date(breaks = scales::pretty_breaks(n = n_breaks), ...)
  } else {
    stop("Something has gone wrong! Please let the incidence2 devs know.")
  }

}

#' @rdname plot.incidence2
#' @aliases facet_plot
#' @export
facet_plot <- function(x, ...) {
  UseMethod("facet_plot")
}

#' @importFrom ggplot2 sym syms
#' @rdname plot.incidence2
#' @aliases facet_plot.incidence2
#' @export
facet_plot.incidence2 <- function(x, facets = NULL, stack = TRUE, fill = NULL, title = NULL,
                       col_pal = vibrant, alpha = 0.7, color = NA,
                       xlab = "", ylab = NULL, n_breaks = 3,
                       show_cases = FALSE, border = "white",
                       na_color = "grey",
                       legend = c("bottom", "top", "left", "right", "none"),
                       angle = 0, nrow = NULL, ...) {

  ellipsis::check_dots_used()

  # convert inputs to character
  facets <- rlang::enquo(facets)
  idx <- tidyselect::eval_select(facets, x)
  facets <- names(x)[idx]
  if (length(facets) == 0) facets <- NULL

  fill <- rlang::enquo(fill)
  idx <- tidyselect::eval_select(fill, x)
  fill <- names(x)[idx]
  if (length(fill) == 0) fill <- NULL
  group_vars <- get_group_names(x)

  out <- plot_basic(x = x, fill = fill, stack = stack, col_pal = col_pal,
                    alpha = alpha, color = color, xlab = xlab, ylab = ylab,
                    show_cases = show_cases, border = border,
                    na_color = na_color, legend = match.arg(legend),
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

  dat <- get_dates(x)
  cl <- class(dat)[1]
  if (cl == "yrwk") {
    out <- out + scale_x_yrwk(n = n_breaks, firstday = get_firstday(dat), ...)
  } else if (cl == "yrmon") {
    out <- out + scale_x_yrmon(n = n_breaks, ...)
  } else if (cl == "yrqtr") {
    out <- out + scale_x_yrqtr(n = n_breaks, ...)
  } else if (cl == "yr") {
    out <- out + scale_x_yr(n = n_breaks, ...)
  } else if (cl == "period") {
    out <- out + scale_x_period(n = n_breaks, firstdate = get_firstdate(dat), ...)
  } else if (cl == "Date") {
    out <- out + ggplot2::scale_x_date(breaks = scales::pretty_breaks(n = n_breaks), ...)
  } else {
    stop("Something has gone wrong! Please let the incidence2 devs know.")
  }
  out + rotate_and_scale(angle = angle)


}

plot_basic <- function(x, fill = NULL, stack = TRUE,
                       col_pal = vibrant, alpha = 0.7, color = NA,
                       xlab = "", ylab = NULL,
                       show_cases = FALSE, border = "white",
                       na_color = "grey",
                       legend = c("right", "left", "bottom", "top", "none"),
                       title = NULL) {


  # get relevant variables
  date_var <- get_dates_name(x)
  count_var <- get_counts_name(x)
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

  if (is.null(fill)) {
    out <- ggplot2::ggplot(df) +
      ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis), y = !!sym(y_axis)),
                        color = color,
                        fill = col_pal(1),
                        alpha = alpha) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = xlab, y = ylab)
  } else if (!all(fill %in% group_vars)) {
    out <- ggplot2::ggplot(df) +
      ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis), y = !!sym(y_axis)),
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
      ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis), y = !!sym(y_axis)),
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
    squaredf <- df[rep(seq.int(nrow(df)), df[[count_var]]), ]
    squaredf[[count_var]] <- 1
    squares <-
      ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis), y = !!sym(y_axis)),
                        color = if (is.na(border)) "white" else border,
                        fill  = NA,
                        position = "stack",
                        data = squaredf)

    out <- out + squares
  }

  if (is.null(title)) {
    out
  } else {
    out + ggplot2::labs(title = title)
  }


}


ylabel <- function(x, ylab) {
  if (is.null(ylab)) {

    interval <- get_interval(x)
    date_vars <- get_dates_name(x)

    if (is.numeric(interval)) {
      if (interval == 1) {
        ylab <- "daily incidence"
      } else {
        ylab <- sprintf("incidence by period of %d days", interval)
      }
    } else if (is.character(interval)) {
      # capturing the number and type
      p     <- "(\\d*)\\s?([a-z]+?)s?$"
      num   <- gsub(p, "\\1", tolower(interval))
      itype <- gsub(p, "\\2", tolower(interval))

      if (itype == "yrwk") {
        ylab <- "Year-week incidence"
      } else if (itype == "yrmon") {
        ylab <- "Year-month incidence"
      } else if (itype == "yrqtr") {
        ylab <- "Year-quarter incidence"
      } else if (itype == "yr") {
        ylab <- "Yearly incidence"
      } else if (num == "" || num == "1") {
        ylab <- sprintf("%sly incidence", itype)
      } else {
        ylab <- sprintf("incidence by a period of %s %ss", num, itype)
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
