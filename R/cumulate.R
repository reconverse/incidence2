#' Compute cumulative 'incidence'
#'
#' `cumulate` is an S3 generic to compute cumulative numbers, with methods
#' for different types of objects:
#' * default method is a wrapper for `cumsum`
#' * `incidence` objects: computes cumulative incidence over time
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
#' @importFrom dplyr .data
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
cumulate.incidence2 <- function(x) {
  is_cumulate <- attr(x, "cumulative")
  if (is_cumulate) {
    stop("x is already a cumulative incidence")
  }

  groups <- get_group_names(x)
  count_var <- get_counts_name(x)

  out <- as.data.table(x)
  if (!is.null(groups)) {
    out[, (count_var) := cumsum(get(..count_var)), by = groups]
  } else out[, (count_var) := cumsum(get(..count_var))]
  setDF(out)

  nms <- names(out)
  attributes(out) <- attributes(x)
  names(out) <- nms
  attr(out, "cumulative") <- TRUE
  out
}
