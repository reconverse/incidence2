#' Color palette used in incidence
#'
#' This function creates the default color palette used in incidence. It is based
#' on the colour-blind friendly palette's of Paul Tol
#' (https://personal.sron.nl/~pault/#sec:qualitative)
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @param n a number of colors
#'
#' @aliases palettes incidence_pal
#'
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#'
#' plot(1:4, cex=8, pch=20, col = incidence_pal(4),
#'      main = "palette: incidence_pal")
#'
#' @rdname palettes
#' @export
incidence_pal <- function(n){
  if (!is.numeric(n)) {
    stop("n is not a number")
  }

  col_vibrant <- c(
    "#0077BB",
    "#33BBEE",
    "#009988",
    "#EE7733",
    "#CC3311",
    "#EE3377",
    "#BBBBBB"
  )

  if (n < 7) {
    return(col_vibrant[1:n])
  }

  return(colorRampPalette(col_vibrant)(n))
}

