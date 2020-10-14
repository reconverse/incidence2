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

#' @importFrom dplyr grouped_df across all_of mutate ungroup
#' @export
#' @rdname cumulate
cumulate.incidence2 <- function(x) {
  is_cumulate <- attr(x, "cumulative")
  if (is_cumulate) {
    stop("x is already a cumulative incidence")
  }
  
  groups <- get_group_names(x)
  count_var <- get_counts_name(x)
  if (!is.null(groups)) {
    f_groups <- lapply(suppressMessages(x[groups]), factor, exclude = NULL)
    split_x <- split(x, f_groups, sep = "_and_")
    out <- lapply(
      split_x, 
      function(z) {
        z[[count_var]] <- cumsum(z[[count_var]])
        z
      }
    )
    out <- do.call(rbind, out)
  } else {
    out <- x
    out[[count_var]] <- cumsum(out[[count_var]])
  }

  date_var <- get_date_group_names(x)
  
  out <- out[order(out[[date_var]],out[[groups]]),]
  names(out) <- names(x)
  attributes(out) <- attributes(x)
  attr(out, "cumulative") <- TRUE
  out
}