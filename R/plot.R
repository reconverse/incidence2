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
#'   groupings of the incidence object.
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
#' @param labels_week labels_week a logical value indicating whether labels x axis tick
#'   marks are in week format YYYY-Www when plotting weekly incidence; defaults to
#'   TRUE.
#' @param na_color The colour to plot `NA` values in graphs (default: `grey`).
#' @param legend Should a legend accompany the facet_plot
#' @param ... other arguments to pass to [ggplot2::facet_wrap()].
#'
#' @return
#'  - `facet_plot()` and `plot()` generate a [ggplot2::ggplot()] object.
#'  - `make_breaks()` a two-element list. The "breaks" element will contain the
#'    evenly-spaced breaks as either dates or numbers and the "labels" element
#'    will contain either a vector of weeks OR a [ggplot2::waiver()] object.
#'  - `scale_x_incidence()` a \pkg{ggplot2} "ScaleContinuous" object.
#'
#' @details
#'  - `plot` creates a one-pane graph of an incidence object.
#'  - `facet_plot` creates a multi-facet graph of a grouped incidence object.
#'    If the object has no groups it returns the same outout as a call to
#'  - `make_breaks()` calculates breaks from an incidence object that always
#'    align with the bins and start on the first observed incidence.
#'  - `scale_x_incidence()` produces and appropriate `ggplot2` scale based on
#'    an incidence object.
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

#'
#' @export
plot.incidence <- function(x, fill = NULL, stack = TRUE,
                           col_pal = vibrant, alpha = 0.7, color = NA,
                           xlab = "", ylab = NULL, n_breaks = 6,
                           show_cases = FALSE, border = "white",
                           na_color = "grey",
                           labels_week = has_weeks(x), ...) {


  ellipsis::check_dots_empty()

  # convert inputs to character
  fill <- arg_values(!!rlang::enexpr(fill))

  # get relevant variables
  date_var <- get_date_vars(x)[1]
  count_var <- get_count_vars(x)
  group_vars <- get_group_vars(x)

  # Handle stacking
  stack.txt <- if (stack) "stack" else "dodge"

  # warnings
  if (length(group_vars) > 1) {
    msg <- paste("plot() can only stack/dodge by one variable.",
                 "For multi-facet plotting try facet_plot()",
                 sep = "\n")
    message(msg)

    if (is.null(fill)) {
      x <- regroup(x)
    } else {
      x <- regroup(x, fill)
    }

    group_vars <- get_group_vars(x)
  }

  # set axis variables
  x_axis <- date_var
  y_axis <- count_var

  # copy data
  df <- x

  # generate label for y-axis
  ylab <- ylabel(df, ylab)

  # Adding a variable for width in ggplot
  df <- add_interval_days(df)

  if (is.null(fill)) {
    out <- ggplot2::ggplot(df) +
      ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis) + .data$interval_days/2, y = !!sym(y_axis)),
                        width = df$interval_days,
                        color = color,
                        fill = col_pal(1),
                        alpha = alpha) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = xlab, y = ylab)
  } else if (length(group_vars) == 1) {
    group_names <- unique(df[[group_vars]])
    n_groups <- length(group_names)
    group_colors <- col_pal(n_groups)

    ## add colors to the plot
    out <- ggplot2::ggplot(df) +
      ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis) + .data$interval_days/2, y = !!sym(y_axis)),
                        width = df$interval_days,
                        color = color,
                        alpha = alpha,
                        position = stack.txt) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = xlab, y = ylab) +
      ggplot2::aes(fill = !!sym(group_vars)) +
      ggplot2::scale_fill_manual(values = group_colors, na.value = na_color)
  }

  if (show_cases && (stack == TRUE || is.null(group_vars))) {
    squaredf <- df[rep(seq.int(nrow(df)), df[[count_var]]), ]
    squaredf[[count_var]] <- 1
    squares <- ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis) + .data$interval_days/2, y = !!sym(y_axis)),
                                 color = if (is.na(border)) "white" else border,
                                 fill  = NA,
                                 position = "stack",
                                 data = squaredf,
                                 width = squaredf$interval_days) +
      ggplot2::theme_bw()
    out <- out + squares
  }

  out <- out + scale_x_incidence(df, n_breaks, labels_week)
  out


}


#' @export
#' @rdname plot.incidence
facet_plot <- function(x, facets = NULL, fill = NULL, col_pal = vibrant,
                       alpha = 0.7, color = NA,
                       xlab = "", ylab = NULL, n_breaks = 3,
                       show_cases = FALSE, border = "white",
                       labels_week = has_weeks(x), na_color = "grey",
                       legend = TRUE, ...) {


  # convert inputs to character
  facets <- arg_values(!!rlang::enexpr(facets))
  fill <- arg_values(!!rlang::enexpr(fill))

  # get relevant variables
  date_var <- get_date_vars(x)[1]
  count_var <- get_count_vars(x)
  group_vars <- get_group_vars(x)
  legend <- if (legend) "bottom" else "none"

  # set axis variables
  x_axis <- date_var
  y_axis <- count_var

  # copy data
  df <- x

  # generate label for y-axis
  ylab <- ylabel(df, ylab)

  # Adding a variable for width in ggplot
  df <- add_interval_days(df)

  # get fill
  if (is.null(fill)) {
    out <- ggplot2::ggplot(df) +
      ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis) + .data$interval_days/2, y = !!sym(y_axis)),
                        width = df$interval_days,
                        color = color,
                        fill = col_pal(1),
                        alpha = alpha) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = xlab, y = ylab)
  } else {
    fill_names <- unique(df[[fill]])
    n_fill <- length(fill_names)
    fill_colors <- col_pal(n_fill)

    out <- ggplot2::ggplot(df) +
      ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis) + .data$interval_days/2, y = !!sym(y_axis)),
                        width = df$interval_days,
                        color = color,
                        alpha = alpha) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = legend) +
      ggplot2::labs(x = xlab, y = ylab) +
      ggplot2::aes(fill = !!sym(fill)) +
      ggplot2::scale_fill_manual(values = fill_colors, na.value = na_color)
  }

  if (show_cases) {
    squaredf <- df[rep(seq.int(nrow(df)), df[[count_var]]), ]
    squaredf[[count_var]] <- 1
    squares <- ggplot2::geom_col(ggplot2::aes(x = !!sym(x_axis) + .data$interval_days/2, y = !!sym(y_axis)),
                                 color = if (is.na(border)) "white" else border,
                                 fill  = NA,
                                 position = "stack",
                                 data = squaredf,
                                 width = squaredf$interval_days) +
      ggplot2::theme_bw()
    out <- out + squares
  }

  if (is.null(facets) && !is.null(group_vars)) {
    out <- out + ggplot2::facet_wrap(ggplot2::vars(!!!syms(group_vars)), ...)
  } else if (!is.null(facets)) {
    out <- out + ggplot2::facet_wrap(ggplot2::vars(!!!syms(facets)), ...)
  }

  out <- out + scale_x_incidence(df, n_breaks, labels_week)
  out

}

has_weeks <- function(x) {
  if (length(get_date_vars(x)) > 1) {
    TRUE
  } else {
    FALSE
  }
}

has_isoweeks <- function(x) {
  if (length(get_date_vars(x)) == 3) {
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

add_interval_days <- function(x) {
  x$interval_days <- get_interval(x, integer = TRUE)

  ## if the date type is POSIXct, then the interval is actually interval seconds
  ## and needs to be converted to days
  date_var <- get_date_vars(x)[1]
  if (inherits(x[[date_var]], "POSIXct")) {
    x$interval_days <- x$interval_days * 86400 # 24h * 60m * 60s
  }
  x
}
