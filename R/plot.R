#' Plot an incidence object
#'
#' `plot()` can be used to provide a quick bar plot of an incidence object.
#'
# -------------------------------------------------------------------------
#' - Facetting will occur automatically if either grouping variables or
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
#' @param na_color `[character]`
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
#' @param ... Not currently used.
#'
# -------------------------------------------------------------------------
#' @return
#'  - A `[ggplot2::ggplot()]` object.
#'
# -------------------------------------------------------------------------
#' @examples
#' if (requireNamespace("outbreaks", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
#'   withAutoprint({
#'     data(ebola_sim_clean, package = "outbreaks")
#'     dat <- ebola_sim_clean$linelist
#'
#'     inci <- incidence(dat, date_index = "date_of_onset", groups = "hospital")
#'     plot(inci, angle = 45)
#'
#'     inci2 <- regroup(inci)
#'     plot(inci2)
#'   })
#' }
#'
# -------------------------------------------------------------------------
#' @export
plot.incidence2 <- function(
    x, y,
    width = 1,
    colour_palette = vibrant, border_colour = NA, na_color = "grey", alpha = 0.7,
    title = NULL,
    angle = 0, size = NULL,
    nrow = NULL,
    ...
) {

    if (!requireNamespace("ggplot2", quietly = TRUE))
        stopf("'ggplot2' is required for the incidence plot method but is not present.")

    # For R CMD check
    .data <- NULL

    # get relevant variables
    groups <- get_group_names.incidence2(x)
    count_values <- get_count_value_name.incidence2(x)
    dates <- get_date_index_name.incidence2(x)

    # set axis variables
    x_axis <- dates
    y_axis <- count_values

    # create fill palette by count variable
    fill <- get_count_variable_name.incidence2(x)
    count_vars <- get_count_variable.incidence2(x)[[1L]]
    n_count_vars <- length(unique(count_vars))
    fill_colours <- colour_palette(n_count_vars)

    # convert input to data frame
    dat <- as.data.frame.incidence2(x)

    # make plot
    out <- ggplot2::ggplot(dat) +
        ggplot2::geom_col(
            ggplot2::aes(x = .data[[x_axis]], y = .data[[y_axis]]),
            colour = border_colour,
            alpha = alpha,
            width = width,...
        ) +
        ggplot2::theme_bw() +
        ggplot2::aes(fill = .data[[fill]]) +
        ggplot2::scale_fill_manual(values = fill_colours, na.value = na_color) +
        ggplot2::theme(legend.position = "none")

    # facet_grid if multiple counts / groups
    if (n_count_vars > 1L) {
        if (length(groups)) {
            out <- out +
                ggplot2::facet_grid(
                    rows = ggplot2::vars(!!rlang::sym(fill)),
                    cols = ggplot2::vars(!!!rlang::syms(groups))
                )
        } else {
            out <- out +
                ggplot2::facet_grid(
                    rows = ggplot2::vars(!!rlang::sym(fill))
                )
        }
    } else if (length(groups)) {
        out <- out +
            ggplot2::facet_wrap(
                ggplot2::vars(!!!rlang::syms(groups)),
                nrow = nrow
            ) +
            ggplot2::labs(x = count_vars[[1L]], y = get_count_value_name(x))

    } else {
        out <- out + ggplot2::labs(x = count_vars[[1L]], y = get_count_value_name(x))
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

    # return plot
    out
}





