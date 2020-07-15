#' Compute cumulative 'incidence'
#'
#' `cumulate` is an S3 generic to compute cumulative numbers, with methods
#' for different types of objects:
#' * default method is a wrapper for `cumsum`
#' * `incidence` objects: computes cumulative incidence over time
#'
#' @author Thibaut Jombart
#'
#' @param x An incidence object.
#'
#' @examples
#' dat <- data.frame(
#'   dates = as.integer(c(0,1,2,2,3,5,7)),
#'   groups = factor(c(1, 2, 3, 3, 3, 3, 1))
#' )
#'
#' i <- incidence(dat, date_index = dates, groups = groups)
#' i
#'
#' cumulative_i <- cumulate(i)
#' cumulative_i
#' @export
#' @rdname cumulate
cumulate <- function(x) {
  UseMethod("cumulate", x)
}


#' @export
#' @rdname cumulate
cumulate.default <- function(x) {
  cumsum(x)
}

#' @export
#' @rdname cumulate
cumulate.incidence <- function(x) {
  is_cumulate <- attr(x, "cumulative")
  if (is_cumulate) {
    stop("x is already a cumulative incidence")
  }
  out <- x
  groups <- get_group_names(x)
  count_var <- get_count_name(x)
  if (!is.null(groups)) {
    out <- group_by(out, across(all_of({{groups}})))
  }
  out <- mutate(out, count = cumsum(.data[[count_var]]))
  out <- ungroup(out)
  names(out) <- names(x)
  attributes(out) <- attributes(x)
  attr(out, "cumulative") <- TRUE
  out
}
