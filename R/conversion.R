#' Conversion of incidence objects
#'
#' These functions convert `incidence` objects into other classes.
#'
#' @param x An [incidence()] object.
#'
#' @param ... Not used.
#'
#' @seealso the [incidence()] function to generate the 'incidence' objects.
#'
#' @rdname conversions
#' @export
#'
as.data.frame.incidence <- function(x, ...) {
  ellipsis::check_dots_empty()
  vctrs::new_data_frame(x)
}

#' @rdname conversions
#' @export
#'
as_tibble.incidence <- function(x, ...) {
  ellipsis::check_dots_empty()
  new_bare_tibble(x)
}
