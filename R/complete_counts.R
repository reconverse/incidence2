#' Complete counts for all date and group combinations
#'
#' This function ensures that an incidence object has the same range of dates
#' for each grouping. By default missing counts will be filled with `0L`.
#'
#' @param x An [incidence()] object.
#' @param fill The value to replace missing counts by. Defaults to `0L`.
#'
#' @examples
#' dat <- data.frame(
#'   dates = Sys.Date() + 1:4,
#'   groups = rep(c("grp1","grp2"), 2),
#'   counts = 1:4
#' )
#'
#' i <- incidence(dat, date_index = dates, groups = groups, counts = counts)
#' complete_counts(i)
#'
#' @export
complete_counts <- function(x, fill = 0L) {

  if (!inherits(x, "incidence_df")) {
    abort(sprintf("`%s` is not an 'incidence_df' object", deparse(substitute(x))))
  }

  if (!is.na(fill)) {
    stopifnot("fill must be NA or of length 1" = length(fill) == 1)
  }

  count_vars <- get_count_names(x)
  date_var <- get_dates_name(x)
  group_vars <- get_group_names(x)

  dates <- get_dates(x)
  if (inherits(dates, "Date")) {
    dates_seq <- seq(from = min(dates), to = max(dates), by = 1)
  } else {
    dates_seq <- seq(from = min(dates), to = max(dates))
  }

  dates_seq <- setNames(data.frame(dates_seq), date_var)
  if (!is.null(group_vars)) {
    dates_seq <- unclass(dates_seq)
    vars <- lapply(x[,group_vars], unique)
    dates_seq <- do.call(expand.grid, c(dates_seq, vars))
  }

  tmp <- attributes(x)
  x <- dplyr::left_join(dates_seq, x, by = c(date_var, group_vars))
  if (!is.na(fill)) x[, count_vars][is.na(x[, count_vars])] <- fill

  new_row_names <- attr(x, "row.names")
  attributes(x) <- tmp
  attr(x, "row.names") = new_row_names
  x
}
