#' Plot an incidence object
#'
# -------------------------------------------------------------------------
#' `plot()` can be used to provide a bar plot of an incidence object. Due
#' to the complexities with automating plotting it is some what experimental in
#' nature and it may be better to use ggplot2 directly.
#'
# -------------------------------------------------------------------------
#' - Faceting will occur automatically if either grouping variables or
#'   multiple counts are present.
#'
#' - If there are multiple count variables, each count will occupy a different
#'   row of the resulting plot.
#'
#' - Utilises ggplot2 so this must be installed to use.
#'
# -------------------------------------------------------------------------
#' @param x `<incidence2>` object.
#'
#' @param y Not used.
#'
#' Required for compatibility with the `plot()` generic.
#'
#' @param width `[numeric]`
#'
#' Value between 0 and 1 indicating the relative size of the bars to the
#' interval.
#'
#' Default 1.
#'
#' @param colour_palette `[function]`
#'
#' The color palette to be used for the different count variables.
#'
#' Defaults to `vibrant` (see `?palettes`).
#'
#' @param border_colour `[character]`
#'
#' The color to be used for the borders of the bars.
#'
#' Use `NA` (default) for invisible borders.
#'
#' @param na_colour `[character]`
#'
#' The colour to plot `NA` values in graphs.
#'
#' Defaults to `grey`.
#'
#' @param alpha `[numeric]`
#'
#' The alpha level for color transparency, with 1 being fully opaque and
#' 0 fully transparent
#'
#' Defaults to 0.7.
#'
#' @param fill `[character]`
#'
#' Which variable to colour plots by.
#'
#' Must be a `group` or `count` variable and will mean that variable is not used
#' for facetting.
#'
#' If NULL no distinction if made for plot colours.
#'
#' @param legend `[character]`
#'
#' Position of legend in plot.
#'
#' Only applied if `fill` is not NULL.
#'
#' One of "right" (default), "left", "bottom", "top" or "none".
#'
#' @param title `[character]`
#'
#' Optional title for the graph.
#'
#' @param angle `[numeric]`
#'
#' Rotation angle for text.
#'
#' @param size `[numeric]`
#'
#' text size in pts.
#'
#' @param nrow `[integer]`
#'
#' Number of rows used for facetting if there are group variables present and
#' just one count in the incidence object.
#'
#' Numeric values are coerced to integer via `as.integer()`.
#'
#' @param n_breaks `[integer]`
#'
#' Approximate number of breaks calculated using `scales::breaks_pretty`.
#'
#' Numeric values are coerced to integer via `as.integer()`.
#'
#' Default 6L.
#'
#' @param show_cases `[logical]`
#'
#' if `TRUE`, then each observation will be shown individually in a square
#' format.
#'
#' Normally only used for outbreaks with a small number of cases.
#'
#' Defaults to `FALSE`.
#'
#' @param ... Not currently used.
#'
# -------------------------------------------------------------------------
#' @return
#'  - A `[ggplot2::ggplot()]` object.
#'
# -------------------------------------------------------------------------
#' @export
plot.incidence2 <- function(
    x, y,
    width = 1,
    colour_palette = vibrant, border_colour = NA, na_colour = "grey", alpha = 0.7,
    fill = NULL,
    legend = c("right", "left", "bottom", "top", "none"),
    title = NULL,
    angle = 0, size = NULL,
    nrow = NULL,
    n_breaks = 6L,
    show_cases = FALSE,
    ...
) {

    if (!requireNamespace("ggplot2", quietly = TRUE))
        stopf("'ggplot2' is required for the incidence plot method but is not present.")

    # type checking
    if (!is.numeric(width) || length(width) != 1L)
        stopf("`width` must be a numeric scalar.")

    if (!is.na(border_colour)) {
        if (!is.character(border_colour) || length(border_colour) != 1L) {
            stopf("`border_colour` must be a scalar character or, NA.")
        }
    }

    if (!is.character(na_colour) || length(na_colour) != 1L)
        stopf("`na_colour` must be a scalar character.")

    if (!is.numeric(alpha) || length(alpha) != 1L)
        stopf("`alpha` must be a numeric scalar.")

    if (!is.null(fill)) {
        if (!is.character(fill) || length(fill) != 1L) {
            stopf("`fill` must be a scalar character.")
        }
    }

    if (!is.null(title)) {
        if (!is.character(title) || length(title) != 1L) {
            stopf("`title` must be a scalar character.")
        }
    }

    if (!is.numeric(angle) || length(angle) != 1L)
        stopf("`angle` must be a numeric scalar.")

    if (!is.null(size)) {
        if (!is.numeric(size) || length(size) != 1L) {
            stopf("`size` must be a numeric scalar or NULL.")
        }
    }

    if (!is.null(nrow)) {
        if (!is.numeric(nrow) || length(nrow) != 1L) {
            stopf("`nrow` must be a numeric scalar or NULL.")
        }
        nrow <- as.integer(nrow)
    }

    if (!is.numeric(n_breaks) || length(n_breaks) != 1L) {
        stopf("`n_breaks` must be a numeric scalar.")
    }
    n_breaks <- as.integer(n_breaks)

    legend <- match.arg(legend)

    # For R CMD check
    .data <- NULL

    # get relevant names/variables
    group_vars <- get_group_names.incidence2(x)
    count_var <- get_count_variable_name.incidence2(x)
    counts <- get_count_variable.incidence2(x)

    y_axis <- get_count_value_name.incidence2(x)
    x_axis<- get_date_index_name.incidence2(x)

    # TODO - temporary measure until I can think about this more
    supported = c(
        "Date",
        "grates_period",
        "grates_isoweek",
        "grates_epiweek",
        "grates_yearweek",
        "grates_yearmonth",
        "grates_yearquarter",
        "grates_year"
    )
    dates <- .subset2(x, x_axis)
    if (!inherits(dates, supported)) {
        stopf(paste0(
            "The `date_index` of `x` must be a <Date> or <grates> object to use the provided `plot` method. ",
            "Please raise an issue at https://github.com/reconverse/incidence2/issues If you would like another object <type> supported"
        ))
    }

    # create fill palette
    fill_var <- fill
    use_fill <- TRUE
    if (is.null(fill_var)) {
        use_fill <- FALSE
        fill_var <- count_var
    } else {
        if (!fill_var %in% names(x)) {
            stopf("`fill` must be the name of a column in `x`.")
        }
    }
    fill <- .subset2(x, fill_var)
    n_fill_colours <- length(unique(fill))
    fill_colours <- colour_palette(n_fill_colours)

    # remove groupings that are used as fill
    group_vars <- group_vars[!group_vars %in% fill_var]

    # convert input to data frame
    dat <- as.data.frame.incidence2(x)

    # TODO - This should probably be a seperate function
    if (show_cases) {
        square_dat <- dat[rep(seq.int(nrow(dat)), dat[[y_axis]]), ]
        square_dat[[y_axis]] <- 1
        out <- ggplot2::ggplot(square_dat) +
            ggplot2::geom_col(
                ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]]),
                colour = if (is.na(border_colour)) "white" else border_colour,
                position = "stack",
                alpha = alpha,
                width = width
            ) +
            ggplot2::theme_bw() +
            ggplot2::aes(fill = .data[[fill_var]]) +
            ggplot2::scale_fill_manual(values = fill_colours, na.value = na_colour) +
            ggplot2::coord_equal()
    } else {
        out <- ggplot2::ggplot(dat) +
            ggplot2::geom_col(
                ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]]),
                colour = border_colour,
                alpha = alpha,
                width = width,
            ) +
            ggplot2::theme_bw() +
            ggplot2::aes(fill = .data[[fill_var]]) +
            ggplot2::scale_fill_manual(values = fill_colours, na.value = na_colour)
    }
    out <- out + ggplot2::theme(legend.position = "none")

    # TODO - do this better
    if (inherits(dates, "Date")) {
        out <-  out +
            ggplot2::scale_x_date(
                breaks = scales::breaks_pretty(n  = n_breaks),
                date_labels = "%Y-%m-%d"
            )
    } else if (inherits(dates, "grates_period")) {
        n <- grates::get_n(dates)
        offset <- grates::get_offset(dates)
        out <- out +
            grates::scale_x_grates_period(
                n.breaks = n_breaks,
                n = n,
                offset = offset
            )
    } else {
        scale_fun <- .grates_scale(dates)
        out <-  out + scale_fun(n.breaks = n_breaks)
    }

    # conditional faceting
    luc <- length(unique(counts)) > 1L
    lg <- length(group_vars)

    if ((use_fill && fill_var != count_var && luc) || (!use_fill && luc)) {
        if (length(group_vars)) {
            out <- out +
                ggplot2::facet_grid(
                    rows = ggplot2::vars(!!rlang::sym(count_var)),
                    cols = ggplot2::vars(!!!rlang::syms(group_vars))
                )
        } else {
            out <- out + ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(count_var)))
        }
    } else if ((use_fill && fill_var != count_var && lg)  || (!use_fill && lg)) {
        out <- out +
            ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(group_vars)), nrow = nrow) +
            ggplot2::labs(x = x_axis, y = get_count_value_name(x))
    } else if (use_fill && fill_var == count_var) {
        out <- out +
            ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(group_vars)), nrow = nrow) +
            ggplot2::labs(x = x_axis, y = get_count_value_name(x))
    } else {
        out <- out + ggplot2::labs(x = x_axis, y = get_count_value_name(x))
    }

    if (use_fill) {
        out <- out + ggplot2::theme(legend.position = legend)
    }

    # rotate and scale
    hjust <- if (angle != 0) 1 else NULL
    out <- out +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                hjust = hjust,
                angle = angle,
                size = size
            )
        )

    # Add title
    if (!is.null(title)) {
        out <- out + ggplot2::labs(title = title)
    }

    # return plot
    out
}
