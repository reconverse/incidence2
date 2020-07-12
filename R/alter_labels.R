#' Rotate and scale incidence plot labels
#'
#' @param plot A ggplot2 incidence graph.
#' @param angle Angle to rotate x-axis labels (default 90 degrees)
#' @export
#' @rdname plot.incidence
rotate_and_scale <- function(angle = 0, size = NULL) {
  if (is.null(size)) {
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(hjust = 1, angle = angle)
      )
  } else {
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = angle, hjust = 1, size = size)
      )
  }
}

