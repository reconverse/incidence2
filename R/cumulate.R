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


#' @importFrom dplyr grouped_df mutate ungroup
#' @export
#' @rdname cumulate
cumulate.incidence2 <- function(x) {
  is_cumulate <- attr(x, "cumulative")
  if (is_cumulate) {
    stop("x is already a cumulative incidence")
  }
  out <- x
  groups <- get_group_names(x)
  count_var <- get_counts_name(x)
  if (!is.null(groups)) {
    out <- grouped_df(out, groups)
  }
  out <- mutate(out, count = cumsum(.data[[count_var]]))
  out <- ungroup(out)
  names(out) <- names(x)
  attributes(out) <- attributes(x)
  attr(out, "cumulative") <- TRUE
  out
}
