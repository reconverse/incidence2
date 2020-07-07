#' Color palettes used in incidence
#'
#' These functions are color palettes used in incidence. The palettes come from
#' https://personal.sron.nl/~pault/#sec:qualitative and exclude `grey`, which
#' is reserved for missing data.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @param n a number of colors
#'
#' @aliases palettes vibrant muted
#'
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' vibrant(5)
#' muted(10)
#'
#' @export
#' @rdname palettes
vibrant <- make_palette(vibrant_colors, suggest = "muted")

#' @export
#' @rdname palettes
muted <- make_palette(muted_colors)



# Source: color palettes come from https://personal.sron.nl/~pault/#sec:qualitative
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
