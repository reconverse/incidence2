# -------------------------------------------------------------------------
make_palette <- function(x, quiet = FALSE, suggest = NULL) {
  function(n) {
    if (!is.numeric(n)) abort("n is not a number")

    if (n <= length(x)) {
      x[seq_len(n)]
    } else {
      if (!quiet) {
        msg <- sprintf(
          paste("Using more colors (%d) than this palette can handle (%d);",
                "some colors will be interpolated."),
          n,
          length(x)
        )
        if (!is.null(suggest)) {
          msg <- paste0(
            msg,
            sprintf("\nConsider using `%s` palette instead?",
                    suggest)
          )
        }
        message(msg)
      }
      colorRampPalette(x)(n)
    }
  }
}
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
vibrant_colors <- c(
  "#0077BB",
  "#33BBEE",
  "#009988",
  "#EE7733",
  "#CC3311",
  "#EE3377"
)

muted_colors <- c(
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
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' Color palettes used in incidence
#'
#' These functions are color palettes used in incidence. The palettes come from
#' https://personal.sron.nl/~pault/#sec:qualitative and exclude `grey`, which
#' is reserved for missing data.
#'
#' @param n a number of colors
#'
#' @aliases palettes vibrant muted
#'
#' @examples
#' vibrant(5)
#' muted(10)
#' @export
#' @rdname palettes
vibrant <- make_palette(vibrant_colors, suggest = "muted")
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
#' @export
#' @rdname palettes
muted <- make_palette(muted_colors)
# -------------------------------------------------------------------------
