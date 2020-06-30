#' Compute cumulative 'incidence'
#'
#' `cumulate` is an S3 generic to compute cumulative numbers, with methods
#' for different types of objects:
#' * default method is a wrapper for `cumsum`
#' * `incidence` objects: computes cumulative incidence over time
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @seealso The [incidence()] function to generate the 'incidence'
#' objects.
#'
#' @param x An incidence object.
#'
#' @export
#' @rdname cumulate
cumulate <- function(x) {
  UseMethod("cumulate", x)
}


#' @rdname cumulate
#' @export
cumulate.default <- function(x) {
  cumsum(x)
}

#' @rdname cumulate
#' @export
cumulate.incidence <- function(x) {
  is_cumulate <- attr(x, "cumulative")
  if (is_cumulate) {
    stop("x is already a cumulative incidence")
  }
  out <- x
  groups <- attr(out, "groups")
  if (!is.null(groups)) {
    out <- group_by(out, across(all_of(groups)))
  }
  out <- mutate(out, count = cumsum(.data$count))
  out <- ungroup(out)
  attributes(out) <- attributes(x)
  attr(out, "cumulative") <- TRUE
  out
}
