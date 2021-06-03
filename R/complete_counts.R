#' Complete counts for all date and group combinations
#'
#' This function ensures that an incidence object has the same range of dates
#' for each grouping. By default missing counts will be filled with `NA` but
#' you can optionally specify a value to replace these by.
#'
#' @param x An [incidence()] object.
#' @param fill The value to replace missing counts by. Defaults to `NA`.
#'
#' @examples
#' dat <- data.frame(
#'   dates = Sys.Date() + 1:4,
#'   groups = rep(c("grp1","grp2"), 2),
#'   counts = 1:4
#' )
#'
#' i <- incidence(dat, date_index = dates, groups = groups, counts = counts)
#' complete_counts(i, fill = 0)
#'
#' @export
complete_counts <- function(x, fill = NA) {

  if (!inherits(x, "incidence")) {
    abort(sprintf("`%s` is not an incidence object", deparse(substitute(x))))
  }

  if (!is.na(fill)) {
    stopifnot("fill must be NA or of length 1" = length(fill) == 1)
  }

  date_var <- get_dates_name(x)
  group_vars <- get_group_names(x)
  tmp <- attributes(x)

  if (is.na(fill)) {
    x <- tidyr::complete(x, !!!rlang::syms(date_var), !!!rlang::syms(group_vars))
  } else {
    count_vars <- get_count_names(x)
    count_fill <- rep(fill, length(count_vars))
    count_fill <- setNames(as.list(count_fill), count_vars)
    x <- tidyr::complete(x, !!!rlang::syms(date_var), !!!rlang::syms(group_vars), fill = count_fill)
  }

  new_row_names <- attr(x, "row.names")
  attributes(x) <- tmp
  attr(x, "row.names") = new_row_names
  x
}
