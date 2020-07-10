alter_labels <- function(plot, angle = 90) {
  plot + ggplot2::theme(axis.text = ggplot2::element_text(hjust = 1, angle = angle))
}

