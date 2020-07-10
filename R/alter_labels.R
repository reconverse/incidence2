#' Alter an incidence plot labels
#'
#' @param plot A ggplot2 incidence graph.
#' @param angle Angle to rotate x-axis labels (default 90 degrees)
#' @export
alter_labels <- function(plot, angle = 90) {
  plot + ggplot2::theme(axis.text = ggplot2::element_text(hjust = 1, angle = angle))
}

