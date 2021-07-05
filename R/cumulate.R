#' Compute cumulative 'incidence'
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' `cumulate` has been deprecated to avoid encouraging erroneous regression
#'   on cumulative counts.
#'
#' @param x An incidence object.
#'
#' @name cumulate-deprecated
#' @keywords internal
#' @export
cumulate <- function(x) {
  lifecycle::deprecate_stop(
    when = "2.0.0",
    what = "cumulate()",
    details = "* See the 2.0.0 NEWS for more information"
  )
}

