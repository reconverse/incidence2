#' Color palettes used in incidence
#'
#' These functions are color palettes used in incidence. The palettes come from
#' https://personal.sron.nl/~pault/#sec:qualitative and exclude `grey`, which
#' is reserved for missing data.
#'
#' @param n `[integer]`
#'
#' Number of colours.
#'
#' `double` vectors will be converted via `as.integer(n)`.
#'
#' @aliases palettes vibrant muted
#'
#' @examples
#' vibrant(5)
#' muted(10)
#'
#' @export
#' @rdname palettes
vibrant <- function(n) {
    f <- .make_palette(.vibrant_colors, suggest = "muted")
    f(n)
}

# -------------------------------------------------------------------------

#' @export
#' @rdname palettes
muted <- function(n) {
    f <- .make_palette(.muted_colors)
    f(n)
}

# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #
# -------------------------------- INTERNALS ------------------------------ #
# ------------------------------------------------------------------------- #
# ------------------------------------------------------------------------- #

.make_palette <- function(x, quiet = FALSE, suggest = NULL) {
    function(n) {

        if (!is.integer(n)) {
            if (is.vector(n, "double")) {
                n <- as.integer(n)
            } else {
                stopf("`n` must be integer.")
            }
        }

        if (n <= length(x)) {
            x[seq_len(n)]
        } else {
            if (!quiet) {
                msg <- sprintf(
                    "Using more colors (%d) than this palette can handle (%d); some colors will be interpolated.",
                    n, length(x)
                )
                if (!is.null(suggest)) {
                    msg <- paste0(
                        msg,
                        sprintf("\nConsider using `%s` palette instead?", suggest)
                    )
                }
                message(msg)
            }
            colorRampPalette(x)(n)
        }
    }
}

.vibrant_colors <- c(
    "#0077BB",
    "#33BBEE",
    "#009988",
    "#EE7733",
    "#CC3311",
    "#EE3377"
)

.muted_colors <- c(
    "#332288",
    "#88CCEE",
    "#44AA99",
    "#117733",
    "#999933",
    "#DDCC77",
    "#CC6677",
    "#882255",
    "#AA4499"
)
