#' Compute cumulative 'incidence'
#'
#' `cumulate` is an S3 generic to compute cumulative numbers, with methods
#' for different types of objects:
#' * default method is a wrapper for `cumsum`
#' * `incidence` objects: computes cumulative incidence over time
#'
#' @param x An incidence object.
#' @param ... Not currently used
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
#'
#' @import data.table
#' @export
#'
#' @rdname cumulate
cumulate <- function(x, ...) {
  UseMethod("cumulate", x)
}

#' @export
#' @rdname cumulate
cumulate.default <- function(x, ...) {
  cumsum(x)
}


#' @param fill Value to complete missing date-grouping combinations with. If
#'   NULL, no completion is performed. Default: 0L.
#'
#' @export
#' @rdname cumulate
cumulate.incidence_df <- function(x, fill = 0L, ...) {

  # due to NSE notes in R CMD check
  ..count_var <- NULL

  is_cumulate <- attr(x, "cumulative")
  if (is_cumulate) abort("x is already a cumulative incidence")

  x <- complete_counts(x)

  groups <- get_group_names(x)
  count_var <- get_count_names(x)
  date_var <- get_date_group_names(x)

  dt <- !any(vapply(x, typeof, character(1)) == "list")
  if (dt) {
    out <- as.data.table(x)
    if (!is.null(groups)) {
      out[, (count_var) := lapply(.SD, cumsum), keyby = groups, .SDcols = count_var]
    } else {
      out[, (count_var) := lapply(.SD, cumsum), .SDcols = count_var]
    }
    setDF(out)
  } else {
    out <- x
    if (!is.null(groups)) {
      out <- grouped_df(out, groups)
      out <- mutate(out, across(all_of(count_var), cumsum))
      out <- ungroup(out)
      out <- out[order(out[[groups]], out[[date_var]]),]
    } else {
      out[count_var] <- lapply(out[count_var], cumsum)
    }
  }

  attributes(out) <- attributes(x)
  if (inherits(out, "incidence_df")) attr(out, "cumulative") <- TRUE
  out
}
